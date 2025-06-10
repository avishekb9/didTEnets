# Simple script to create sample data for the package

# Create sample financial returns
library(xts)
set.seed(42)

countries <- c("USA", "GBR", "DEU", "FRA", "JPN", "AUS")
dates <- seq(as.Date("2019-01-01"), as.Date("2021-12-30"), by = "day")
dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]

n_obs <- length(dates)
returns_matrix <- matrix(rnorm(n_obs * length(countries), 0, 1.5), 
                        nrow = n_obs, ncol = length(countries))

sample_financial_returns <- xts(returns_matrix, order.by = dates)
colnames(sample_financial_returns) <- countries

# Create sample policy data
sample_policy_data <- data.frame(
  CountryCode = rep(countries, each = length(seq(as.Date("2019-01-01"), as.Date("2021-12-31"), by = "day"))),
  Date = rep(seq(as.Date("2019-01-01"), as.Date("2021-12-31"), by = "day"), length(countries)),
  StringencyIndex = c(rep(runif(length(seq(as.Date("2019-01-01"), as.Date("2021-12-31"), by = "day")) * length(countries), 10, 80)))
)

# Create sample results
sample_tedid_results <- list(
  summary = list(
    analysis_period = "2019-01-01 to 2021-12-31",
    n_countries = length(countries),
    countries = countries,
    policy_effect = "Sample results for demonstration"
  )
)

# Save to data directory
save(sample_financial_returns, file = "/home/avisek/ManyIVsNets/didTEnets/data/sample_financial_returns.rda")
save(sample_policy_data, file = "/home/avisek/ManyIVsNets/didTEnets/data/sample_policy_data.rda")
save(sample_tedid_results, file = "/home/avisek/ManyIVsNets/didTEnets/data/sample_tedid_results.rda")

cat("Sample data created successfully!\n")