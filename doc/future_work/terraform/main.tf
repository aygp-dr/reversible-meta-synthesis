# Main Terraform Configuration for Multi-Region Deployment

provider "aws" {
  region = var.primary_region
  alias  = "primary"
}

provider "aws" {
  region = var.secondary_region_1
  alias  = "secondary_1"
}

provider "aws" {
  region = var.secondary_region_2
  alias  = "secondary_2"
}

# Remote state configuration
terraform {
  backend "s3" {
    bucket         = "synthesis-terraform-state"
    key            = "terraform.tfstate"
    region         = "us-west-1"
    encrypt        = true
    dynamodb_table = "synthesis-terraform-locks"
  }
  
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.0"
    }
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.0"
    }
  }
}

# Variables
variable "primary_region" {
  description = "Primary AWS region"
  type        = string
  default     = "us-west-1"
}

variable "secondary_region_1" {
  description = "First secondary AWS region"
  type        = string
  default     = "eu-central-1"
}

variable "secondary_region_2" {
  description = "Second secondary AWS region"
  type        = string
  default     = "ap-southeast-1"
}

variable "environment" {
  description = "Deployment environment"
  type        = string
  default     = "production"
}

variable "vpc_cidr" {
  description = "CIDR block for VPC"
  type        = string
  default     = "10.0.0.0/16"
}

variable "availability_zones" {
  description = "List of availability zones"
  type        = map(list(string))
  default = {
    "us-west-1"      = ["us-west-1a", "us-west-1b", "us-west-1c"]
    "eu-central-1"   = ["eu-central-1a", "eu-central-1b", "eu-central-1c"]
    "ap-southeast-1" = ["ap-southeast-1a", "ap-southeast-1b", "ap-southeast-1c"]
  }
}

# Modules for each region
module "primary_network" {
  source = "./modules/network"
  
  providers = {
    aws = aws.primary
  }
  
  region            = var.primary_region
  environment       = var.environment
  vpc_cidr          = var.vpc_cidr
  availability_zones = var.availability_zones[var.primary_region]
}

module "secondary_1_network" {
  source = "./modules/network"
  
  providers = {
    aws = aws.secondary_1
  }
  
  region            = var.secondary_region_1
  environment       = var.environment
  vpc_cidr          = var.vpc_cidr
  availability_zones = var.availability_zones[var.secondary_region_1]
}

module "secondary_2_network" {
  source = "./modules/network"
  
  providers = {
    aws = aws.secondary_2
  }
  
  region            = var.secondary_region_2
  environment       = var.environment
  vpc_cidr          = var.vpc_cidr
  availability_zones = var.availability_zones[var.secondary_region_2]
}

module "primary_database" {
  source = "./modules/database"
  
  providers = {
    aws = aws.primary
  }
  
  region             = var.primary_region
  environment        = var.environment
  vpc_id             = module.primary_network.vpc_id
  subnet_ids         = module.primary_network.database_subnet_ids
  availability_zones = var.availability_zones[var.primary_region]
  instance_type      = "db.r6g.2xlarge"
  storage_size       = 1000
  multi_az           = true
  is_primary         = true
}

module "secondary_1_database" {
  source = "./modules/database"
  
  providers = {
    aws = aws.secondary_1
  }
  
  region             = var.secondary_region_1
  environment        = var.environment
  vpc_id             = module.secondary_1_network.vpc_id
  subnet_ids         = module.secondary_1_network.database_subnet_ids
  availability_zones = var.availability_zones[var.secondary_region_1]
  instance_type      = "db.r6g.2xlarge"
  storage_size       = 1000
  multi_az           = true
  is_primary         = false
  primary_db_arn     = module.primary_database.db_instance_arn
}

module "secondary_2_database" {
  source = "./modules/database"
  
  providers = {
    aws = aws.secondary_2
  }
  
  region             = var.secondary_region_2
  environment        = var.environment
  vpc_id             = module.secondary_2_network.vpc_id
  subnet_ids         = module.secondary_2_network.database_subnet_ids
  availability_zones = var.availability_zones[var.secondary_region_2]
  instance_type      = "db.r6g.2xlarge"
  storage_size       = 1000
  multi_az           = true
  is_primary         = false
  primary_db_arn     = module.primary_database.db_instance_arn
}

module "primary_kubernetes" {
  source = "./modules/kubernetes"
  
  providers = {
    aws = aws.primary
  }
  
  region             = var.primary_region
  environment        = var.environment
  vpc_id             = module.primary_network.vpc_id
  subnet_ids         = module.primary_network.private_subnet_ids
  kubernetes_version = "1.28"
  node_groups = {
    synthesis-workers = {
      instance_type = "c5.2xlarge"
      min_size      = 3
      max_size      = 20
      desired_size  = 5
    },
    synthesis-batch = {
      instance_type = "c5.4xlarge"
      min_size      = 0
      max_size      = 10
      desired_size  = 0
    }
  }
}

module "secondary_1_kubernetes" {
  source = "./modules/kubernetes"
  
  providers = {
    aws = aws.secondary_1
  }
  
  region             = var.secondary_region_1
  environment        = var.environment
  vpc_id             = module.secondary_1_network.vpc_id
  subnet_ids         = module.secondary_1_network.private_subnet_ids
  kubernetes_version = "1.28"
  node_groups = {
    synthesis-workers = {
      instance_type = "c5.2xlarge"
      min_size      = 2
      max_size      = 20
      desired_size  = 3
    },
    synthesis-batch = {
      instance_type = "c5.4xlarge"
      min_size      = 0
      max_size      = 10
      desired_size  = 0
    }
  }
}

module "secondary_2_kubernetes" {
  source = "./modules/kubernetes"
  
  providers = {
    aws = aws.secondary_2
  }
  
  region             = var.secondary_region_2
  environment        = var.environment
  vpc_id             = module.secondary_2_network.vpc_id
  subnet_ids         = module.secondary_2_network.private_subnet_ids
  kubernetes_version = "1.28"
  node_groups = {
    synthesis-workers = {
      instance_type = "c5.2xlarge"
      min_size      = 2
      max_size      = 20
      desired_size  = 3
    },
    synthesis-batch = {
      instance_type = "c5.4xlarge"
      min_size      = 0
      max_size      = 10
      desired_size  = 0
    }
  }
}

module "global_dns" {
  source = "./modules/dns"
  
  providers = {
    aws = aws.primary
  }
  
  domain_name = "reversible-meta-synthesis.com"
  
  endpoints = [
    {
      region     = var.primary_region
      dns_name   = module.primary_load_balancer.dns_name
      is_primary = true
    },
    {
      region     = var.secondary_region_1
      dns_name   = module.secondary_1_load_balancer.dns_name
      is_primary = false
    },
    {
      region     = var.secondary_region_2
      dns_name   = module.secondary_2_load_balancer.dns_name
      is_primary = false
    }
  ]
}

module "primary_load_balancer" {
  source = "./modules/load_balancer"
  
  providers = {
    aws = aws.primary
  }
  
  region             = var.primary_region
  environment        = var.environment
  vpc_id             = module.primary_network.vpc_id
  subnet_ids         = module.primary_network.public_subnet_ids
  certificate_arn    = module.primary_certificate.certificate_arn
  target_groups = {
    synthesis-service = {
      port        = 8080
      protocol    = "HTTP"
      target_type = "ip"
      health_check = {
        path                = "/health"
        port                = "traffic-port"
        healthy_threshold   = 2
        unhealthy_threshold = 3
        timeout             = 5
        interval            = 15
        matcher             = "200"
      }
    }
  }
}

module "secondary_1_load_balancer" {
  source = "./modules/load_balancer"
  
  providers = {
    aws = aws.secondary_1
  }
  
  region             = var.secondary_region_1
  environment        = var.environment
  vpc_id             = module.secondary_1_network.vpc_id
  subnet_ids         = module.secondary_1_network.public_subnet_ids
  certificate_arn    = module.secondary_1_certificate.certificate_arn
  target_groups = {
    synthesis-service = {
      port        = 8080
      protocol    = "HTTP"
      target_type = "ip"
      health_check = {
        path                = "/health"
        port                = "traffic-port"
        healthy_threshold   = 2
        unhealthy_threshold = 3
        timeout             = 5
        interval            = 15
        matcher             = "200"
      }
    }
  }
}

module "secondary_2_load_balancer" {
  source = "./modules/load_balancer"
  
  providers = {
    aws = aws.secondary_2
  }
  
  region             = var.secondary_region_2
  environment        = var.environment
  vpc_id             = module.secondary_2_network.vpc_id
  subnet_ids         = module.secondary_2_network.public_subnet_ids
  certificate_arn    = module.secondary_2_certificate.certificate_arn
  target_groups = {
    synthesis-service = {
      port        = 8080
      protocol    = "HTTP"
      target_type = "ip"
      health_check = {
        path                = "/health"
        port                = "traffic-port"
        healthy_threshold   = 2
        unhealthy_threshold = 3
        timeout             = 5
        interval            = 15
        matcher             = "200"
      }
    }
  }
}

module "primary_certificate" {
  source = "./modules/certificate"
  
  providers = {
    aws = aws.primary
  }
  
  domain_name = "reversible-meta-synthesis.com"
  region      = var.primary_region
}

module "secondary_1_certificate" {
  source = "./modules/certificate"
  
  providers = {
    aws = aws.secondary_1
  }
  
  domain_name = "reversible-meta-synthesis.com"
  region      = var.secondary_region_1
}

module "secondary_2_certificate" {
  source = "./modules/certificate"
  
  providers = {
    aws = aws.secondary_2
  }
  
  domain_name = "reversible-meta-synthesis.com"
  region      = var.secondary_region_2
}

module "global_s3" {
  source = "./modules/s3"
  
  providers = {
    aws = aws.primary
  }
  
  bucket_name     = "synthesis-results"
  environment     = var.environment
  versioning      = true
  lifecycle_rules = true
  
  replication = {
    enabled = true
    destination_regions = [
      var.secondary_region_1,
      var.secondary_region_2
    ]
  }
}

module "monitoring" {
  source = "./modules/monitoring"
  
  providers = {
    aws = aws.primary
  }
  
  environment = var.environment
  regions = [
    var.primary_region,
    var.secondary_region_1,
    var.secondary_region_2
  ]
  
  alerting = {
    email    = "alerts@reversible-meta-synthesis.com"
    sns_topic = "synthesis-alerts"
    pagerduty = {
      integration_key = var.pagerduty_integration_key
    }
  }
}

# Outputs
output "primary_vpc_id" {
  value = module.primary_network.vpc_id
}

output "primary_kubernetes_cluster_name" {
  value = module.primary_kubernetes.cluster_name
}

output "primary_database_endpoint" {
  value = module.primary_database.db_instance_endpoint
}

output "global_dns_name" {
  value = module.global_dns.dns_name
}

output "global_s3_bucket" {
  value = module.global_s3.bucket_name
}
