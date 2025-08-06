;;; reversible-meta-synthesis.el --- Emacs configuration for Reversible Meta-Synthesis project

;;; Commentary:
;; Project-specific Emacs configuration for Scheme development
;; with Geiser, Guile3, Org-mode, TRAMP, and Paredit support

;;; Code:

;; Package management setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Ensure required packages are installed
(defvar reversible-meta-synthesis-packages
  '(geiser
    geiser-guile
    paredit
    rainbow-delimiters
    company
    flycheck
    org
    magit))

;; Install missing packages
(dolist (package reversible-meta-synthesis-packages)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

;; Project configuration
(defvar reversible-meta-synthesis-root
  (or (getenv "PROJECT_ROOT")
      (expand-file-name ".")))

(defvar reversible-meta-synthesis-name
  (or (getenv "PROJECT_NAME")
      "reversible-meta-synthesis"))

;; Geiser configuration for Guile
(require 'geiser)
(require 'geiser-guile)
(setq geiser-guile-binary "guile3")
(setq geiser-default-implementation 'guile)
(setq geiser-guile-load-path
      (list (expand-file-name "src/scheme" reversible-meta-synthesis-root)))

;; Scheme mode configuration
(add-hook 'scheme-mode-hook
          (lambda ()
            (paredit-mode 1)
            (rainbow-delimiters-mode 1)
            (company-mode 1)
            (setq-local indent-tabs-mode nil)))

;; Paredit configuration
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'hy-mode-hook 'paredit-mode)

;; Rainbow delimiters for better visibility
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Company mode for autocompletion
(require 'company)
(global-company-mode 1)
(setq company-idle-delay 0.3)
(setq company-minimum-prefix-length 2)

;; Org-mode configuration
(require 'org)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)
   (shell . t)
   (python . t)))

;; TRAMP configuration for remote development
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-verbose 6)

;; Project-specific file associations
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.ss\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.hy\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; Custom functions for the project
(defun reversible-meta-synthesis-run-scheme-example ()
  "Run the current Scheme example file."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match "\\.scm\\'" file-name))
      (compile (format "guile %s" file-name)))))

(defun reversible-meta-synthesis-run-tests ()
  "Run project tests."
  (interactive)
  (compile "make test-all"))

(defun reversible-meta-synthesis-open-repl ()
  "Open a Guile REPL for the project."
  (interactive)
  (run-geiser 'guile))

;; Key bindings
(global-set-key (kbd "C-c r e") 'reversible-meta-synthesis-run-scheme-example)
(global-set-key (kbd "C-c r t") 'reversible-meta-synthesis-run-tests)
(global-set-key (kbd "C-c r r") 'reversible-meta-synthesis-open-repl)
(global-set-key (kbd "C-c g") 'magit-status)

;; Set up initial layout
(defun reversible-meta-synthesis-setup-layout ()
  "Set up initial window layout for development."
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (split-window-vertically)
  (other-window 1)
  (eshell)
  (rename-buffer "*project-shell*")
  (other-window 1)
  (find-file (expand-file-name "src/scheme/reversible-interpreter.scm" 
                               reversible-meta-synthesis-root))
  (other-window 1))

;; Display project information
(message "Reversible Meta-Synthesis development environment loaded")
(message "Project root: %s" reversible-meta-synthesis-root)
(message "Key bindings:")
(message "  C-c r e - Run Scheme example")
(message "  C-c r t - Run all tests")
(message "  C-c r r - Open Guile REPL")
(message "  C-c g   - Open Magit")

;; Auto-load project layout
(when (string= (buffer-name) "*scratch*")
  (reversible-meta-synthesis-setup-layout))

(provide 'reversible-meta-synthesis)
;;; reversible-meta-synthesis.el ends here