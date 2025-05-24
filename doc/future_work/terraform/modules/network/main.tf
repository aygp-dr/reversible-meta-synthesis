# Network Module for Multi-Region Deployment

resource "aws_vpc" "synthesis" {
  cidr_block           = var.vpc_cidr
  enable_dns_support   = true
  enable_dns_hostnames = true
  
  tags = {
    Name        = "${var.environment}-synthesis-vpc-${var.region}"
    Environment = var.environment
    Region      = var.region
  }
}

resource "aws_subnet" "public" {
  count             = length(var.availability_zones)
  vpc_id            = aws_vpc.synthesis.id
  cidr_block        = cidrsubnet(var.vpc_cidr, 8, count.index)
  availability_zone = var.availability_zones[count.index]
  
  tags = {
    Name        = "${var.environment}-synthesis-public-subnet-${var.availability_zones[count.index]}"
    Environment = var.environment
    Region      = var.region
    Tier        = "public"
  }
}

resource "aws_subnet" "private" {
  count             = length(var.availability_zones)
  vpc_id            = aws_vpc.synthesis.id
  cidr_block        = cidrsubnet(var.vpc_cidr, 8, count.index + length(var.availability_zones))
  availability_zone = var.availability_zones[count.index]
  
  tags = {
    Name        = "${var.environment}-synthesis-private-subnet-${var.availability_zones[count.index]}"
    Environment = var.environment
    Region      = var.region
    Tier        = "private"
  }
}

resource "aws_subnet" "database" {
  count             = length(var.availability_zones)
  vpc_id            = aws_vpc.synthesis.id
  cidr_block        = cidrsubnet(var.vpc_cidr, 8, count.index + 2 * length(var.availability_zones))
  availability_zone = var.availability_zones[count.index]
  
  tags = {
    Name        = "${var.environment}-synthesis-database-subnet-${var.availability_zones[count.index]}"
    Environment = var.environment
    Region      = var.region
    Tier        = "database"
  }
}

resource "aws_internet_gateway" "synthesis" {
  vpc_id = aws_vpc.synthesis.id
  
  tags = {
    Name        = "${var.environment}-synthesis-igw-${var.region}"
    Environment = var.environment
    Region      = var.region
  }
}

resource "aws_eip" "nat" {
  count = length(var.availability_zones)
  vpc   = true
  
  tags = {
    Name        = "${var.environment}-synthesis-nat-eip-${var.availability_zones[count.index]}"
    Environment = var.environment
    Region      = var.region
  }
}

resource "aws_nat_gateway" "synthesis" {
  count         = length(var.availability_zones)
  allocation_id = aws_eip.nat[count.index].id
  subnet_id     = aws_subnet.public[count.index].id
  
  tags = {
    Name        = "${var.environment}-synthesis-nat-${var.availability_zones[count.index]}"
    Environment = var.environment
    Region      = var.region
  }
}

resource "aws_route_table" "public" {
  vpc_id = aws_vpc.synthesis.id
  
  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.synthesis.id
  }
  
  tags = {
    Name        = "${var.environment}-synthesis-public-rt-${var.region}"
    Environment = var.environment
    Region      = var.region
    Tier        = "public"
  }
}

resource "aws_route_table" "private" {
  count  = length(var.availability_zones)
  vpc_id = aws_vpc.synthesis.id
  
  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = aws_nat_gateway.synthesis[count.index].id
  }
  
  tags = {
    Name        = "${var.environment}-synthesis-private-rt-${var.availability_zones[count.index]}"
    Environment = var.environment
    Region      = var.region
    Tier        = "private"
  }
}

resource "aws_route_table" "database" {
  count  = length(var.availability_zones)
  vpc_id = aws_vpc.synthesis.id
  
  route {
    cidr_block     = "0.0.0.0/0"
    nat_gateway_id = aws_nat_gateway.synthesis[count.index].id
  }
  
  tags = {
    Name        = "${var.environment}-synthesis-database-rt-${var.availability_zones[count.index]}"
    Environment = var.environment
    Region      = var.region
    Tier        = "database"
  }
}

resource "aws_route_table_association" "public" {
  count          = length(var.availability_zones)
  subnet_id      = aws_subnet.public[count.index].id
  route_table_id = aws_route_table.public.id
}

resource "aws_route_table_association" "private" {
  count          = length(var.availability_zones)
  subnet_id      = aws_subnet.private[count.index].id
  route_table_id = aws_route_table.private[count.index].id
}

resource "aws_route_table_association" "database" {
  count          = length(var.availability_zones)
  subnet_id      = aws_subnet.database[count.index].id
  route_table_id = aws_route_table.database[count.index].id
}

# Security Groups
resource "aws_security_group" "public" {
  name        = "${var.environment}-synthesis-public-sg"
  description = "Security group for public resources"
  vpc_id      = aws_vpc.synthesis.id

  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name        = "${var.environment}-synthesis-public-sg"
    Environment = var.environment
    Region      = var.region
  }
}

resource "aws_security_group" "private" {
  name        = "${var.environment}-synthesis-private-sg"
  description = "Security group for private resources"
  vpc_id      = aws_vpc.synthesis.id

  ingress {
    from_port       = 0
    to_port         = 0
    protocol        = "-1"
    security_groups = [aws_security_group.public.id]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name        = "${var.environment}-synthesis-private-sg"
    Environment = var.environment
    Region      = var.region
  }
}

resource "aws_security_group" "database" {
  name        = "${var.environment}-synthesis-database-sg"
  description = "Security group for database resources"
  vpc_id      = aws_vpc.synthesis.id

  ingress {
    from_port       = 5432
    to_port         = 5432
    protocol        = "tcp"
    security_groups = [aws_security_group.private.id]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name        = "${var.environment}-synthesis-database-sg"
    Environment = var.environment
    Region      = var.region
  }
}

# VPC Endpoints for AWS services
resource "aws_vpc_endpoint" "s3" {
  vpc_id            = aws_vpc.synthesis.id
  service_name      = "com.amazonaws.${var.region}.s3"
  vpc_endpoint_type = "Gateway"
  route_table_ids   = concat(
    [aws_route_table.public.id],
    aws_route_table.private[*].id,
    aws_route_table.database[*].id
  )

  tags = {
    Name        = "${var.environment}-synthesis-s3-endpoint"
    Environment = var.environment
    Region      = var.region
  }
}

# Flow logs for VPC traffic monitoring
resource "aws_flow_log" "synthesis" {
  iam_role_arn    = var.flow_log_role_arn
  log_destination = var.flow_log_destination
  traffic_type    = "ALL"
  vpc_id          = aws_vpc.synthesis.id

  tags = {
    Name        = "${var.environment}-synthesis-flow-log"
    Environment = var.environment
    Region      = var.region
  }
}

# Output the created resources
output "vpc_id" {
  value = aws_vpc.synthesis.id
}

output "public_subnet_ids" {
  value = aws_subnet.public[*].id
}

output "private_subnet_ids" {
  value = aws_subnet.private[*].id
}

output "database_subnet_ids" {
  value = aws_subnet.database[*].id
}

output "public_security_group_id" {
  value = aws_security_group.public.id
}

output "private_security_group_id" {
  value = aws_security_group.private.id
}

output "database_security_group_id" {
  value = aws_security_group.database.id
}
