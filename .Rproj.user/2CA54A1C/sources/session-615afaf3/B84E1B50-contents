# Test package installation and basic functionality

# Try to install the package
tryCatch({
  # Install from local directory
  devtools::install(".", upgrade = "never", quiet = TRUE)
  
  # Load the package
  library(didTEnets)
  
  cat("✓ Package installed and loaded successfully\n")
  
  # Test basic function availability
  test_functions <- c(
    "download_financial_data",
    "create_policy_data", 
    "calculate_enhanced_transfer_entropy",
    "calculate_network_weights",
    "calculate_nici",
    "calculate_nipi",
    "run_complete_tedid_analysis"
  )
  
  for (func in test_functions) {
    if (exists(func)) {
      cat("✓", func, "available\n")
    } else {
      cat("✗", func, "NOT available\n")
    }
  }
  
  # Test sample data
  tryCatch({
    data(sample_financial_returns, envir = environment())
    cat("✓ Sample financial returns data loaded\n")
  }, error = function(e) {
    cat("✗ Sample data error:", e$message, "\n")
  })
  
  cat("\n=== PACKAGE INSTALLATION TEST COMPLETED ===\n")
  
}, error = function(e) {
  cat("✗ Installation failed:", e$message, "\n")
})
