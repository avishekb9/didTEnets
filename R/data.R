#' Sample Financial Returns Data
#'
#' A sample dataset containing daily financial returns for major stock market indices
#' from January 2019 to December 2021, suitable for demonstrating the didTEnets package
#' functionality without requiring external data downloads.
#'
#' @format An xts object with 528 observations and 6 variables:
#' \describe{
#'   \item{USA}{S&P 500 daily log returns (percentage)}
#'   \item{GBR}{FTSE 100 daily log returns (percentage)}
#'   \item{DEU}{DAX daily log returns (percentage)}
#'   \item{FRA}{CAC 40 daily log returns (percentage)}
#'   \item{JPN}{Nikkei 225 daily log returns (percentage)}
#'   \item{AUS}{ASX 200 daily log returns (percentage)}
#' }
#'
#' @details
#' This dataset represents actual financial market returns data and can be used
#' to test and demonstrate all didTEnets package functions. The data covers the
#' COVID-19 pandemic period, making it suitable for crisis analysis and policy
#' evaluation studies.
#'
#' The returns are calculated as: \code{returns = diff(log(prices)) * 100}
#'
#' @source Yahoo Finance (via quantmod package)
#'
#' @examples
#' \dontrun{
#' data(sample_financial_returns)
#' head(sample_financial_returns)
#' 
#' # Basic statistics
#' summary(sample_financial_returns)
#' 
#' # Use in analysis
#' weights <- calculate_network_weights(sample_financial_returns)
#' }
#'
#' @keywords datasets
"sample_financial_returns"

#' Sample Policy Data
#'
#' A sample dataset containing COVID-19 policy stringency indices for major countries
#' from January 2019 to December 2021, based on realistic policy response patterns.
#'
#' @format A data frame with 6,570 observations and 3 variables:
#' \describe{
#'   \item{CountryCode}{Character. ISO3 country codes (USA, GBR, DEU, FRA, JPN, AUS)}
#'   \item{Date}{Date. Daily dates from 2019-01-01 to 2021-12-31}
#'   \item{StringencyIndex}{Numeric. Policy stringency index (0-100 scale)}
#' }
#'
#' @details
#' This synthetic dataset reflects realistic COVID-19 policy response patterns,
#' including early adopters (faster policy implementation) and late adopters
#' (slower policy response). The data can be used to demonstrate policy
#' evaluation using the TE-DiD framework.
#'
#' Policy patterns include:
#' \itemize{
#'   \item Early adopters: AUS, DEU (March 1-8, 2020)
#'   \item Moderate adopters: FRA, JPN (March 10-15, 2020)  
#'   \item Late adopters: GBR, USA (March 18-22, 2020)
#' }
#'
#' @examples
#' \dontrun{
#' data(sample_policy_data)
#' head(sample_policy_data)
#' 
#' # Policy evolution by country
#' library(ggplot2)
#' ggplot(sample_policy_data, aes(x = Date, y = StringencyIndex, color = CountryCode)) +
#'   geom_line() +
#'   theme_minimal()
#' }
#'
#' @keywords datasets
"sample_policy_data"

#' Sample TE-DiD Analysis Results
#'
#' Pre-computed results from a complete TE-DiD analysis using the sample datasets,
#' provided for quick demonstration and testing purposes.
#'
#' @format A named list containing:
#' \describe{
#'   \item{data}{List with financial, policy, and treatment group data}
#'   \item{networks}{List with network weights, TE matrices, and statistics}
#'   \item{tedid}{List with TE-DiD models and results}
#'   \item{summary}{Analysis summary statistics}
#' }
#'
#' @details
#' This dataset contains the complete output from \code{run_complete_tedid_analysis()}
#' using the sample data, allowing users to explore results structure and
#' visualization options without running the full analysis pipeline.
#'
#' Key components include:
#' \itemize{
#'   \item Transfer entropy matrices for different periods
#'   \item Network-Informed Contagion Index (NICI) values
#'   \item Network-Informed Policy Intensity (NIPI) measures
#'   \item TE-DiD model estimation results
#'   \item Network statistics and contagion analysis
#' }
#'
#' @examples
#' \dontrun{
#' data(sample_tedid_results)
#' 
#' # View analysis summary
#' print(sample_tedid_results$summary)
#' 
#' # Access model results
#' summary(sample_tedid_results$tedid$main_model)
#' 
#' # Network statistics
#' head(sample_tedid_results$networks$statistics)
#' }
#'
#' @keywords datasets
"sample_tedid_results"