#' Download Financial Data for TE-DiD Analysis
#'
#' Downloads financial market data from Yahoo Finance for multiple countries
#' and processes it for transfer entropy and network analysis.
#'
#' @param tickers Named character vector where names are Yahoo Finance tickers 
#'   and values are country codes (e.g., c("^GSPC" = "USA", "^FTSE" = "GBR"))
#' @param start_date Character string in "YYYY-MM-DD" format for start date
#' @param end_date Character string in "YYYY-MM-DD" format for end date
#' @param min_observations Minimum number of observations required per market
#'
#' @return List containing:
#'   \item{prices}{xts object with adjusted closing prices}
#'   \item{returns}{xts object with log returns (percentage)}
#'   \item{countries}{Character vector of country codes}
#'   \item{summary}{List with data summary statistics}
#'
#' @examples
#' \dontrun{
#' tickers <- c("^GSPC" = "USA", "^FTSE" = "GBR", "^GDAXI" = "DEU")
#' data <- download_financial_data(tickers, "2019-01-01", "2021-12-31")
#' }
#'
#' @export
#' @importFrom quantmod getSymbols Ad
#' @importFrom xts merge.xts
#' @importFrom zoo index
download_financial_data <- function(tickers, start_date, end_date, min_observations = 100) {
  if (!is.character(tickers) || is.null(names(tickers))) {
    stop("tickers must be a named character vector")
  }
  
  price_list <- list()
  successful_downloads <- 0
  
  cat("Downloading financial data for", length(tickers), "markets...\n")
  
  for (i in 1:length(tickers)) {
    ticker <- names(tickers)[i]
    country <- tickers[ticker]
    
    cat("Downloading", country, "(", ticker, ")...")
    
    tryCatch({
      data <- getSymbols(ticker, 
                        src = "yahoo", 
                        from = start_date, 
                        to = end_date,
                        auto.assign = FALSE,
                        warnings = FALSE)
      
      if (!is.null(data) && nrow(data) > min_observations) {
        adj_close <- Ad(data)
        colnames(adj_close) <- country
        price_list[[country]] <- adj_close
        successful_downloads <- successful_downloads + 1
        cat(" ✓\n")
      } else {
        cat(" ✗ (insufficient data)\n")
      }
      
      Sys.sleep(0.4)  # Rate limiting
    }, error = function(e) {
      cat(" ✗ (", e$message, ")\n")
    })
  }
  
  if (length(price_list) < 2) {
    stop("Insufficient data: need at least 2 markets for network analysis")
  }
  
  # Merge and process data
  all_prices <- do.call(merge.xts, price_list)
  all_prices <- na.omit(all_prices)
  all_returns <- diff(log(all_prices)) * 100
  all_returns <- na.omit(all_returns)
  
  countries <- colnames(all_returns)
  
  # Create summary
  summary_info <- list(
    n_countries = length(countries),
    n_observations = nrow(all_returns),
    date_range = range(index(all_returns)),
    countries = countries,
    successful_downloads = successful_downloads,
    total_requested = length(tickers)
  )
  
  cat("Successfully downloaded:", successful_downloads, "markets\n")
  cat("Final dataset:", summary_info$n_countries, "countries,", 
      summary_info$n_observations, "observations\n")
  
  return(list(
    prices = all_prices,
    returns = all_returns,
    countries = countries,
    summary = summary_info
  ))
}

#' Create Enhanced Policy Data
#'
#' Creates realistic policy stringency data based on COVID-19 response patterns
#' for use in TE-DiD analysis. This function generates synthetic policy data
#' that reflects real-world policy implementation timing and intensity.
#'
#' @param countries Character vector of country codes
#' @param start_date Character string for start date ("YYYY-MM-DD")
#' @param end_date Character string for end date ("YYYY-MM-DD")
#' @param policy_profiles Optional list of custom policy profiles per country
#'
#' @return data.frame with columns:
#'   \item{CountryCode}{Country identifier}
#'   \item{Date}{Date of observation}
#'   \item{StringencyIndex}{Policy stringency index (0-100)}
#'
#' @examples
#' \dontrun{
#' countries <- c("USA", "GBR", "DEU", "FRA")
#' policy_data <- create_policy_data(countries, "2019-01-01", "2021-12-31")
#' }
#'
#' @export
create_policy_data <- function(countries, start_date, end_date, policy_profiles = NULL) {
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  
  # Default policy profiles based on real COVID-19 patterns
  if (is.null(policy_profiles)) {
    policy_profiles <- list(
      # Very early adopters (before March 5, 2020)
      "KOR" = list(start = as.Date("2020-02-25"), peak = 85, volatility = 6),
      "AUS" = list(start = as.Date("2020-03-02"), peak = 78, volatility = 8),
      
      # Early adopters (March 5-15, 2020) 
      "DEU" = list(start = as.Date("2020-03-08"), peak = 75, volatility = 10),
      "FRA" = list(start = as.Date("2020-03-12"), peak = 73, volatility = 12),
      "IND" = list(start = as.Date("2020-03-10"), peak = 82, volatility = 15),
      
      # Moderate adopters (March 15-25, 2020)
      "GBR" = list(start = as.Date("2020-03-18"), peak = 68, volatility = 14),
      "JPN" = list(start = as.Date("2020-03-15"), peak = 52, volatility = 8),
      "HKG" = list(start = as.Date("2020-03-20"), peak = 65, volatility = 12),
      
      # Late adopters (after March 25, 2020)
      "USA" = list(start = as.Date("2020-03-22"), peak = 62, volatility = 18),
      "BRA" = list(start = as.Date("2020-03-28"), peak = 58, volatility = 22)
    )
  }
  
  policy_data <- data.frame()
  
  for (country in countries) {
    if (country %in% names(policy_profiles)) {
      profile <- policy_profiles[[country]]
      
      stringency <- ifelse(
        dates < profile$start,
        pmax(0, rnorm(length(dates), 8, 4)),  # Low pre-crisis baseline
        pmin(100, profile$peak + 
             18 * sin((as.numeric(dates - profile$start))/35) * 
             exp(-as.numeric(dates - profile$start)/180) +
             rnorm(length(dates), 0, profile$volatility))
      )
    } else {
      # Default pattern for any additional countries
      default_start <- as.Date("2020-03-15")
      stringency <- ifelse(
        dates < default_start,
        pmax(0, rnorm(length(dates), 7, 3)),
        pmin(100, 65 + 15 * sin((as.numeric(dates - default_start))/40) + 
             rnorm(length(dates), 0, 12))
      )
    }
    
    stringency <- pmax(0, pmin(100, stringency))
    
    country_data <- data.frame(
      CountryCode = country,
      Date = dates,
      StringencyIndex = stringency
    )
    
    policy_data <- rbind(policy_data, country_data)
  }
  
  return(policy_data)
}