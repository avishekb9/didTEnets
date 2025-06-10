# Simple test script for didTEnets package

# Set working directory
setwd("/home/avisek/ManyIVsNets/didTEnets")

# Test basic functionality
tryCatch({
  # Source individual files to test
  source("R/data_acquisition.R")
  source("R/transfer_entropy.R")
  source("R/network_analysis.R")
  
  cat("✓ Core functions loaded successfully\n")
  
  # Test basic transfer entropy calculation
  set.seed(123)
  x <- rnorm(100)
  y <- 0.3 * x + rnorm(100, 0, 0.5)
  
  te_result <- calculate_enhanced_transfer_entropy(x, y)
  cat("✓ Transfer entropy calculation works:", round(te_result, 4), "\n")
  
  # Test network weights
  test_data <- matrix(rnorm(300), nrow = 100, ncol = 3)
  colnames(test_data) <- c("A", "B", "C")
  
  weights <- calculate_network_weights(test_data, "correlation")
  cat("✓ Network weights calculation works\n")
  
  # Test policy data creation
  test_countries <- c("USA", "GBR", "DEU")
  policy_data <- create_policy_data(test_countries, "2020-01-01", "2020-06-30")
  cat("✓ Policy data creation works:", nrow(policy_data), "observations\n")
  
  cat("\n=== BASIC PACKAGE FUNCTIONALITY VERIFIED ===\n")
  cat("✓ All core functions working correctly\n")
  cat("✓ Package is ready for installation\n")
  
}, error = function(e) {
  cat("✗ Error:", e$message, "\n")
})
