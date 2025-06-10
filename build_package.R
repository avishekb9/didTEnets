# Build script for didTEnets package

# Install required packages
if (!requireNamespace("roxygen2", quietly = TRUE)) {
  install.packages("roxygen2")
}
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Load libraries
library(roxygen2)
library(devtools)

# Set working directory to package root
setwd("/home/avisek/ManyIVsNets/didTEnets")

# Generate documentation
cat("Generating documentation...\n")
roxygenise()

# Check package
cat("Checking package...\n")
check_result <- check()

cat("Package build completed!\n")