test_that("calculate_nici produces correct structure", {
  # Create sample inputs
  set.seed(202)
  n_countries <- 3
  countries <- c("USA", "GBR", "DEU")
  
  # Sample TE matrix
  te_matrix <- matrix(runif(9, 0, 0.5), nrow = 3, ncol = 3)
  diag(te_matrix) <- 0
  rownames(te_matrix) <- colnames(te_matrix) <- countries
  
  te_matrices <- list("Crisis_Q0.1" = te_matrix)
  
  # Sample weight matrix
  weight_matrix <- matrix(runif(9, 0, 1), nrow = 3, ncol = 3)
  diag(weight_matrix) <- 0
  weight_matrix <- weight_matrix / rowSums(weight_matrix)
  rownames(weight_matrix) <- colnames(weight_matrix) <- countries
  
  weight_matrices <- list("composite" = weight_matrix)
  
  # Test NICI calculation
  monthly_dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "month")
  nici_data <- calculate_nici(te_matrices, weight_matrices, monthly_dates)
  
  expect_type(nici_data, "list")
  expect_true("composite" %in% names(nici_data))
  
  nici_result <- nici_data$composite
  expect_true("values" %in% names(nici_result))
  expect_true("dates" %in% names(nici_result))
  expect_true("countries" %in% names(nici_result))
  
  # Check dimensions
  expect_equal(nrow(nici_result$values), length(monthly_dates))
  expect_equal(ncol(nici_result$values), n_countries)
  expect_equal(length(nici_result$dates), length(monthly_dates))
  expect_equal(length(nici_result$countries), n_countries)
})

test_that("calculate_nipi produces correct structure", {
  # Create sample policy data
  countries <- c("USA", "GBR", "DEU")
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-06-30"), by = "day")
  
  monthly_policy <- data.frame(
    CountryCode = rep(countries, each = 6),
    year_month = rep(c("2020-01", "2020-02", "2020-03", "2020-04", "2020-05", "2020-06"), 3),
    StringencyIndex = runif(18, 20, 80)
  )
  
  # Sample weight matrix
  weight_matrix <- matrix(runif(9, 0, 1), nrow = 3, ncol = 3)
  diag(weight_matrix) <- 0
  weight_matrix <- weight_matrix / rowSums(weight_matrix)
  rownames(weight_matrix) <- colnames(weight_matrix) <- countries
  
  weight_matrices <- list("composite" = weight_matrix)
  
  # Test NIPI calculation
  monthly_dates <- seq(as.Date("2020-01-01"), as.Date("2020-06-30"), by = "month")
  nipi_data <- calculate_nipi(monthly_policy, weight_matrices, 
                             monthly_dates = monthly_dates)
  
  expect_type(nipi_data, "list")
  expect_true("composite" %in% names(nipi_data))
  
  nipi_result <- nipi_data$composite
  expect_true("values" %in% names(nipi_result))
  expect_true("dates" %in% names(nipi_result))
  expect_true("countries" %in% names(nipi_result))
  
  # Check dimensions
  expect_equal(nrow(nipi_result$values), length(countries))
  expect_equal(ncol(nipi_result$values), length(monthly_dates))
  expect_equal(length(nipi_result$dates), length(monthly_dates))
  expect_equal(length(nipi_result$countries), length(countries))
})

test_that("NICI time-varying effects work correctly", {
  # Test crisis amplification
  set.seed(303)
  countries <- c("USA", "GBR")
  
  te_matrix <- matrix(c(0, 0.3, 0.4, 0), nrow = 2, ncol = 2)
  rownames(te_matrix) <- colnames(te_matrix) <- countries
  te_matrices <- list("Crisis_Q0.1" = te_matrix)
  
  weight_matrix <- matrix(c(0, 0.6, 0.4, 0), nrow = 2, ncol = 2)
  rownames(weight_matrix) <- colnames(weight_matrix) <- countries
  weight_matrices <- list("composite" = weight_matrix)
  
  # Test different periods
  crisis_start <- as.Date("2020-03-01")
  crisis_end <- as.Date("2020-06-30")
  
  monthly_dates <- c(
    as.Date("2020-01-01"),  # Pre-crisis
    as.Date("2020-04-01"),  # Crisis
    as.Date("2020-08-01")   # Post-crisis
  )
  
  nici_data <- calculate_nici(te_matrices, weight_matrices, monthly_dates, 
                             crisis_start, crisis_end)
  
  nici_values <- nici_data$composite$values
  
  # Crisis period should have higher values than pre-crisis
  pre_crisis_avg <- mean(nici_values[1, ])
  crisis_avg <- mean(nici_values[2, ])
  post_crisis_avg <- mean(nici_values[3, ])
  
  expect_gt(crisis_avg, pre_crisis_avg)
})

test_that("NIPI spillover effects work correctly", {
  countries <- c("USA", "GBR")
  
  # Create policy data where one country has high stringency
  monthly_policy <- data.frame(
    CountryCode = c("USA", "USA", "GBR", "GBR"),
    year_month = c("2020-01", "2020-02", "2020-01", "2020-02"),
    StringencyIndex = c(80, 90, 20, 30)  # USA high, GBR low
  )
  
  # High spillover weight from USA to GBR
  weight_matrix <- matrix(c(0, 0.8, 0.2, 0), nrow = 2, ncol = 2)
  rownames(weight_matrix) <- colnames(weight_matrix) <- countries
  weight_matrices <- list("composite" = weight_matrix)
  
  monthly_dates <- c(as.Date("2020-01-01"), as.Date("2020-02-01"))
  
  nipi_data <- calculate_nipi(monthly_policy, weight_matrices, 
                             spillover_param = 0.5, monthly_dates = monthly_dates)
  
  nipi_values <- nipi_data$composite$values
  
  # GBR should have higher NIPI than its direct policy due to USA spillover
  gbr_direct_policy <- monthly_policy$StringencyIndex[monthly_policy$CountryCode == "GBR"]
  gbr_nipi <- nipi_values["GBR", ]
  
  expect_true(all(gbr_nipi > gbr_direct_policy))
})