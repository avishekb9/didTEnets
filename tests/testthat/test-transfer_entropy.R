test_that("calculate_enhanced_transfer_entropy works correctly", {
  # Generate test data
  set.seed(42)
  n <- 100
  source <- rnorm(n)
  target <- 0.5 * source + rnorm(n, 0, 0.5)
  
  # Test basic functionality
  te_result <- calculate_enhanced_transfer_entropy(source, target)
  
  expect_type(te_result, "double")
  expect_length(te_result, 1)
  expect_gte(te_result, 0.001)
  expect_lte(te_result, 1.0)
})

test_that("calculate_enhanced_transfer_entropy handles edge cases", {
  # Test with insufficient data
  te_short <- calculate_enhanced_transfer_entropy(c(1, 2, 3), c(2, 3, 4))
  expect_equal(te_short, 0.001)
  
  # Test with missing values
  source_na <- c(1, 2, NA, 4, 5)
  target_na <- c(2, NA, 4, 5, 6)
  te_na <- calculate_enhanced_transfer_entropy(source_na, target_na)
  expect_type(te_na, "double")
  
  # Test with identical series
  identical_series <- rep(1, 50)
  te_identical <- calculate_enhanced_transfer_entropy(identical_series, identical_series)
  expect_gte(te_identical, 0.001)
})

test_that("calculate_te_matrices_by_period produces correct structure", {
  # Create sample returns data
  set.seed(123)
  n_obs <- 150
  n_countries <- 3
  countries <- c("USA", "GBR", "DEU")
  
  returns_data <- matrix(rnorm(n_obs * n_countries, 0, 2), 
                        nrow = n_obs, ncol = n_countries)
  colnames(returns_data) <- countries
  returns_data <- as.data.frame(returns_data)
  
  # Define periods
  period_breaks <- list(
    "Pre_Crisis" = 1:50,
    "Crisis" = 51:100,
    "Post_Crisis" = 101:150
  )
  
  # Calculate TE matrices
  te_matrices <- calculate_te_matrices_by_period(returns_data, period_breaks, 
                                               quantiles = c(0.1, 0.9))
  
  expect_type(te_matrices, "list")
  expect_length(te_matrices, 6)  # 3 periods × 2 quantiles
  
  # Check matrix structure
  expect_true(all(sapply(te_matrices, is.matrix)))
  expect_true(all(sapply(te_matrices, function(x) nrow(x) == n_countries)))
  expect_true(all(sapply(te_matrices, function(x) ncol(x) == n_countries)))
  
  # Check diagonal is zero
  expect_true(all(sapply(te_matrices, function(x) all(diag(x) == 0))))
})

test_that("quantile conditioning works properly", {
  set.seed(456)
  n <- 200
  
  # Create data with extreme events
  source <- rnorm(n)
  source[1:10] <- -3  # Extreme negative events
  target <- rnorm(n)
  
  # Test crisis quantile (0.1)
  te_crisis <- calculate_enhanced_transfer_entropy(source, target, quantile_threshold = 0.1)
  
  # Test normal quantile (0.5)
  te_normal <- calculate_enhanced_transfer_entropy(source, target, quantile_threshold = 0.5)
  
  expect_type(te_crisis, "double")
  expect_type(te_normal, "double")
  expect_gte(te_crisis, 0.001)
  expect_gte(te_normal, 0.001)
})