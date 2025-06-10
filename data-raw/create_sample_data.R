# Create sample data for the didTEnets package
# This script generates realistic sample datasets for testing and demonstration

library(dplyr)
library(lubridate)

# Set seed for reproducibility
set.seed(42)

# 1. Create sample financial returns data
# Simulate realistic financial market returns for major indices

countries <- c("USA", "GBR", "DEU", "FRA", "JPN", "AUS")
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2021-12-31")

# Create business days sequence
business_days <- seq(start_date, end_date, by = "day")
business_days <- business_days[!weekdays(business_days) %in% c("Saturday", "Sunday")]

n_obs <- length(business_days)
n_countries <- length(countries)

# Generate correlated returns with realistic properties
# Create correlation matrix
correlation_matrix <- matrix(c(
  1.00, 0.65, 0.60, 0.58, 0.45, 0.55,  # USA
  0.65, 1.00, 0.75, 0.72, 0.40, 0.50,  # GBR
  0.60, 0.75, 1.00, 0.78, 0.35, 0.45,  # DEU
  0.58, 0.72, 0.78, 1.00, 0.38, 0.42,  # FRA
  0.45, 0.40, 0.35, 0.38, 1.00, 0.60,  # JPN
  0.55, 0.50, 0.45, 0.42, 0.60, 1.00   # AUS
), nrow = n_countries, ncol = n_countries)

# Cholesky decomposition for correlated random variables
chol_matrix <- chol(correlation_matrix)

# Generate base returns
base_returns <- matrix(rnorm(n_obs * n_countries, 0, 1), nrow = n_obs, ncol = n_countries)
correlated_returns <- base_returns %*% chol_matrix

# Add realistic features
returns_matrix <- matrix(0, nrow = n_obs, ncol = n_countries)

for (i in 1:n_countries) {
  base_vol <- c(1.2, 1.1, 1.3, 1.2, 1.4, 1.1)[i]  # Different volatility levels
  
  # Add time-varying volatility
  volatility <- base_vol * (1 + 0.3 * sin(1:n_obs / 50) + 
                           ifelse(business_days >= as.Date("2020-03-01") & 
                                 business_days <= as.Date("2020-12-31"), 0.8, 0))
  
  # Add crisis shock
  crisis_shock <- ifelse(business_days >= as.Date("2020-03-15") & 
                        business_days <= as.Date("2020-04-15"), 
                        -2 + rnorm(n_obs, 0, 1), 0)
  
  returns_matrix[, i] <- correlated_returns[, i] * volatility + crisis_shock
}

# Convert to xts object
library(xts)
sample_financial_returns <- xts(returns_matrix, order.by = business_days)
colnames(sample_financial_returns) <- countries

# 2. Create sample policy data
# Generate realistic COVID-19 policy stringency patterns

dates <- seq(start_date, end_date, by = "day")

# Policy response patterns (based on real-world timing)
policy_patterns <- list(
  "USA" = list(start = as.Date("2020-03-22"), peak = 62, volatility = 18),
  "GBR" = list(start = as.Date("2020-03-18"), peak = 68, volatility = 14),
  "DEU" = list(start = as.Date("2020-03-08"), peak = 75, volatility = 10),
  "FRA" = list(start = as.Date("2020-03-12"), peak = 73, volatility = 12),
  "JPN" = list(start = as.Date("2020-03-15"), peak = 52, volatility = 8),
  "AUS" = list(start = as.Date("2020-03-02"), peak = 78, volatility = 8)
)

sample_policy_data <- data.frame()

for (country in countries) {
  pattern <- policy_patterns[[country]]
  
  stringency <- ifelse(
    dates < pattern$start,
    pmax(0, rnorm(length(dates), 8, 4)),  # Low pre-crisis baseline
    pmin(100, pattern$peak + 
         18 * sin((as.numeric(dates - pattern$start))/35) * 
         exp(-as.numeric(dates - pattern$start)/180) +
         rnorm(length(dates), 0, pattern$volatility))
  )
  
  stringency <- pmax(0, pmin(100, stringency))
  
  country_data <- data.frame(
    CountryCode = country,
    Date = dates,
    StringencyIndex = stringency
  )
  
  sample_policy_data <- rbind(sample_policy_data, country_data)
}

# 3. Create sample TE-DiD results
# Generate a realistic results structure (simplified for demonstration)

# Define treatment groups
stringency_threshold <- 70
first_hit_dates <- sample_policy_data %>%
  filter(StringencyIndex >= stringency_threshold) %>%
  group_by(CountryCode) %>%
  summarise(FirstHitDate = min(Date), .groups = "drop")

median_hit_date <- median(first_hit_dates$FirstHitDate, na.rm = TRUE)

treatment_groups <- first_hit_dates %>%
  mutate(
    Group = ifelse(FirstHitDate <= median_hit_date, "EarlyAdopter", "LateAdopter"),
    TreatmentDummy = as.numeric(FirstHitDate <= median_hit_date)
  )

# Create simplified sample results structure
sample_tedid_results <- list(
  summary = list(
    analysis_period = "2019-01-01 to 2021-12-31",
    n_countries = length(countries),
    countries = countries,
    n_observations = 360,  # 6 countries × 60 months
    crisis_period = "2020-03-01 to 2020-12-31",
    contagion_established = TRUE,
    early_adopters = sum(treatment_groups$Group == "EarlyAdopter"),
    late_adopters = sum(treatment_groups$Group == "LateAdopter"),
    key_coefficient = -0.0052,
    policy_effect = "Early adoption of stringent policies REDUCED incoming financial contagion",
    te_matrices_computed = 9,
    weight_types_used = c("correlation", "geographic", "economic", "composite")
  ),
  data = list(
    treatment_groups = treatment_groups
  )
)

# Save datasets
usethis::use_data(sample_financial_returns, overwrite = TRUE)
usethis::use_data(sample_policy_data, overwrite = TRUE)
usethis::use_data(sample_tedid_results, overwrite = TRUE)

cat("Sample datasets created successfully!\n")
cat("- sample_financial_returns:", nrow(sample_financial_returns), "observations\n")
cat("- sample_policy_data:", nrow(sample_policy_data), "observations\n")
cat("- sample_tedid_results: Complete results structure\n")