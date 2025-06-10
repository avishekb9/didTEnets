test_that("calculate_network_weights produces correct structure", {
  # Create sample returns data
  set.seed(789)
  n_countries <- 4
  n_obs <- 100
  countries <- c("USA", "GBR", "DEU", "FRA")
  
  returns_data <- matrix(rnorm(n_obs * n_countries, 0, 1.5), 
                        nrow = n_obs, ncol = n_countries)
  colnames(returns_data) <- countries
  
  # Test all weight types
  weights <- calculate_network_weights(returns_data)
  
  expect_type(weights, "list")
  expect_true("correlation" %in% names(weights))
  expect_true("composite" %in% names(weights))
  
  # Check matrix properties
  for (weight_name in names(weights)) {
    w <- weights[[weight_name]]
    expect_true(is.matrix(w))
    expect_equal(nrow(w), n_countries)
    expect_equal(ncol(w), n_countries)
    expect_equal(rownames(w), countries)
    expect_equal(colnames(w), countries)
    
    # Check diagonal is zero
    expect_true(all(diag(w) == 0))
    
    # Check row normalization (approximately)
    row_sums <- rowSums(w)
    expect_true(all(abs(row_sums - 1) < 1e-10 | row_sums == 0))
    
    # Check non-negative values
    expect_true(all(w >= 0))
  }
})

test_that("calculate_network_weights handles different weight types", {
  set.seed(101)
  n_countries <- 3
  countries <- c("USA", "GBR", "DEU")
  
  returns_data <- matrix(rnorm(50 * n_countries), nrow = 50, ncol = n_countries)
  colnames(returns_data) <- countries
  
  # Test individual weight types
  cor_weights <- calculate_network_weights(returns_data, "correlation")
  expect_true("correlation" %in% names(cor_weights))
  expect_length(cor_weights, 1)
  
  geo_weights <- calculate_network_weights(returns_data, "geographic")
  expect_true("geographic" %in% names(geo_weights))
  expect_length(geo_weights, 1)
  
  # Test custom geographic groups
  custom_groups <- list(
    europe = c("GBR", "DEU"),
    americas = c("USA")
  )
  
  custom_weights <- calculate_network_weights(returns_data, "geographic", 
                                            geographic_groups = custom_groups)
  expect_true(is.matrix(custom_weights$geographic))
})

test_that("calculate_network_statistics works correctly", {
  # Create sample TE matrices
  n_countries <- 3
  countries <- c("USA", "GBR", "DEU")
  
  # Create synthetic TE matrices
  te_matrix1 <- matrix(runif(9, 0, 0.5), nrow = 3, ncol = 3)
  diag(te_matrix1) <- 0
  rownames(te_matrix1) <- colnames(te_matrix1) <- countries
  
  te_matrix2 <- matrix(runif(9, 0, 0.8), nrow = 3, ncol = 3)
  diag(te_matrix2) <- 0
  rownames(te_matrix2) <- colnames(te_matrix2) <- countries
  
  te_matrices <- list(
    "Pre_Crisis_Q0.1" = te_matrix1,
    "Crisis_Q0.1" = te_matrix2
  )
  
  # Calculate network statistics
  network_stats <- calculate_network_statistics(te_matrices)
  
  expect_s3_class(network_stats, "data.frame")
  expect_equal(nrow(network_stats), 2)
  
  expected_cols <- c("Period", "Quantile", "Matrix_Name", "Density", 
                    "Transitivity", "Avg_Strength_In", "Avg_Strength_Out", 
                    "Max_TE", "Mean_TE")
  expect_true(all(expected_cols %in% names(network_stats)))
  
  # Check value ranges
  expect_true(all(network_stats$Density >= 0 & network_stats$Density <= 1))
  expect_true(all(network_stats$Max_TE >= 0))
  expect_true(all(network_stats$Mean_TE >= 0))
})

test_that("network statistics handle edge cases", {
  # Empty matrix
  empty_matrix <- matrix(0, nrow = 2, ncol = 2)
  rownames(empty_matrix) <- colnames(empty_matrix) <- c("A", "B")
  
  te_matrices_empty <- list("Empty_Q0.1" = empty_matrix)
  
  stats_empty <- calculate_network_statistics(te_matrices_empty)
  expect_s3_class(stats_empty, "data.frame")
  expect_equal(nrow(stats_empty), 1)
  expect_equal(stats_empty$Density, 0)
  
  # Single country (trivial case)
  single_matrix <- matrix(0, nrow = 1, ncol = 1)
  rownames(single_matrix) <- colnames(single_matrix) <- "USA"
  
  te_matrices_single <- list("Single_Q0.1" = single_matrix)
  
  stats_single <- calculate_network_statistics(te_matrices_single)
  expect_s3_class(stats_single, "data.frame")
  expect_equal(nrow(stats_single), 1)
})