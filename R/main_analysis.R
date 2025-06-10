#' Run Complete TE-DiD Analysis
#'
#' Executes the complete Transfer Entropy-informed Difference-in-Differences
#' analysis pipeline from data acquisition to model estimation and visualization.
#' This is the main wrapper function for the didTEnets package.
#'
#' @param tickers Named character vector of financial market tickers
#' @param start_date Character string for analysis start date ("YYYY-MM-DD")
#' @param end_date Character string for analysis end date ("YYYY-MM-DD")
#' @param crisis_start Date when crisis period begins
#' @param crisis_end Date when crisis period ends
#' @param stringency_threshold Numeric threshold for policy treatment classification
#' @param weight_types Character vector of network weight types to calculate
#' @param quantiles Numeric vector of quantile thresholds for TE analysis
#' @param create_plots Logical indicating whether to generate visualizations
#' @param verbose Logical indicating whether to print progress messages
#'
#' @return Named list containing all analysis results:
#'   \item{data}{Financial and policy data}
#'   \item{networks}{Network weights and TE matrices}
#'   \item{tedid}{TE-DiD model results}
#'   \item{plots}{Visualization objects (if create_plots = TRUE)}
#'   \item{summary}{Analysis summary statistics}
#'
#' @examples
#' \dontrun{
#' # Define financial markets
#' tickers <- c("^GSPC" = "USA", "^FTSE" = "GBR", "^GDAXI" = "DEU", 
#'              "^FCHI" = "FRA", "^N225" = "JPN", "^AXJO" = "AUS")
#' 
#' # Run complete analysis
#' results <- run_complete_tedid_analysis(
#'   tickers = tickers,
#'   start_date = "2019-01-01",
#'   end_date = "2021-12-31",
#'   create_plots = TRUE
#' )
#' 
#' # View main results
#' print(results$tedid$main_model)
#' print(results$summary)
#' }
#'
#' @export
#' @importFrom dplyr %>% filter group_by summarise mutate
run_complete_tedid_analysis <- function(
  tickers = c("^GSPC" = "USA", "^FTSE" = "GBR", "^GDAXI" = "DEU", 
              "^FCHI" = "FRA", "^N225" = "JPN", "^AXJO" = "AUS"),
  start_date = "2019-01-01",
  end_date = "2021-12-31",
  crisis_start = as.Date("2020-03-01"),
  crisis_end = as.Date("2020-12-31"),
  stringency_threshold = 70,
  weight_types = c("correlation", "geographic", "composite"),
  quantiles = c(0.1, 0.5, 0.9),
  create_plots = TRUE,
  verbose = TRUE
) {
  
  if (verbose) cat("=== TE-DiD Complete Analysis Pipeline ===\n")
  
  # 1. Data Acquisition
  if (verbose) cat("Step 1: Acquiring financial data...\n")
  financial_data <- download_financial_data(tickers, start_date, end_date)
  
  if (verbose) cat("Step 2: Creating policy data...\n")
  policy_data <- create_policy_data(financial_data$countries, start_date, end_date)
  
  # 2. Treatment Group Classification
  if (verbose) cat("Step 3: Classifying treatment groups...\n")
  first_hit_dates <- policy_data %>%
    filter(StringencyIndex >= stringency_threshold) %>%
    group_by(CountryCode) %>%
    summarise(FirstHitDate = min(Date), .groups = "drop")
  
  median_hit_date <- median(first_hit_dates$FirstHitDate, na.rm = TRUE)
  
  treatment_groups <- first_hit_dates %>%
    mutate(
      Group = ifelse(FirstHitDate <= median_hit_date, "EarlyAdopter", "LateAdopter"),
      TreatmentDummy = as.numeric(FirstHitDate <= median_hit_date)
    )
  
  # 3. Network Analysis
  if (verbose) cat("Step 4: Computing network weights...\n")
  network_weights <- calculate_network_weights(financial_data$returns, weight_types)
  
  if (verbose) cat("Step 5: Calculating transfer entropy matrices...\n")
  # Define periods
  period_breaks <- list(
    "Pre_Crisis" = which(index(financial_data$returns) < crisis_start),
    "Crisis" = which(index(financial_data$returns) >= crisis_start & 
                    index(financial_data$returns) <= crisis_end),
    "Post_Crisis" = which(index(financial_data$returns) > crisis_end)
  )
  
  te_matrices <- calculate_te_matrices_by_period(financial_data$returns, 
                                                period_breaks, quantiles)
  
  # 4. Network Statistics and Contagion Analysis
  if (verbose) cat("Step 6: Analyzing network statistics...\n")
  network_stats <- calculate_network_statistics(te_matrices)
  
  # Establish contagion
  crisis_stats <- network_stats %>%
    filter(Quantile == "Q0.1") %>%  # Focus on crisis quantile
    arrange(Period)
  
  contagion_established <- FALSE
  if (nrow(crisis_stats) >= 2) {
    pre_crisis <- crisis_stats[crisis_stats$Period == "Pre_Crisis", ]
    crisis <- crisis_stats[crisis_stats$Period == "Crisis", ]
    
    if (nrow(pre_crisis) > 0 && nrow(crisis) > 0) {
      density_change <- crisis$Density - pre_crisis$Density
      mean_te_change <- crisis$Mean_TE - pre_crisis$Mean_TE
      
      if (density_change > 0.05 || mean_te_change > 0.01) {
        contagion_established <- TRUE
      }
    }
  }
  
  # 5. NICI and NIPI Construction
  if (verbose) cat("Step 7: Computing NICI and NIPI...\n")
  monthly_dates <- seq(from = as.Date(start_date), to = as.Date(end_date), by = "month")
  
  nici_data <- calculate_nici(te_matrices, network_weights, monthly_dates, 
                             crisis_start, crisis_end)
  
  # Prepare monthly policy data
  monthly_policy <- policy_data %>%
    mutate(year_month = format(Date, "%Y-%m")) %>%
    group_by(CountryCode, year_month) %>%
    summarise(StringencyIndex = mean(StringencyIndex, na.rm = TRUE), .groups = "drop")
  
  nipi_data <- calculate_nipi(monthly_policy, network_weights, 
                             monthly_dates = monthly_dates,
                             crisis_start = crisis_start, crisis_end = crisis_end)
  
  # 6. TE-DiD Model Estimation
  if (verbose) cat("Step 8: Preparing TE-DiD dataset...\n")
  tedid_dataset <- prepare_tedid_dataset(nici_data, nipi_data, treatment_groups, 
                                        policy_data, weight_type = "composite")
  
  if (verbose) cat("Step 9: Estimating TE-DiD models...\n")
  # Main model
  main_model <- estimate_tedid_model(tedid_dataset)
  
  # Enhanced model with controls
  enhanced_model <- tryCatch({
    if ("AvgStringency" %in% names(tedid_dataset)) {
      estimate_tedid_model(tedid_dataset, 
        formula = "IncomingFlow ~ EarlyAdopter * CrisisPeriod + AvgStringency + Month")
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  # Extract results
  main_results <- extract_tedid_results(main_model, tedid_dataset)
  enhanced_results <- if (!is.null(enhanced_model)) {
    extract_tedid_results(enhanced_model, tedid_dataset)
  } else {
    NULL
  }
  
  # 7. Create Visualizations
  plots_list <- NULL
  if (create_plots) {
    if (verbose) cat("Step 10: Creating visualizations...\n")
    
    tryCatch({
      plots_list <- list()
      
      # Network evolution plots
      plots_list$network_comparison <- create_network_comparison(te_matrices)
      
      # Transfer entropy heatmaps
      plots_list$te_heatmaps <- create_te_heatmaps(te_matrices)
      
      # TE-DiD result plots
      plots_list$tedid_plots <- visualize_tedid_results(tedid_dataset, crisis_start)
      
    }, error = function(e) {
      warning("Some visualizations failed: ", e$message)
    })
  }
  
  # 8. Compile Results Summary
  if (verbose) cat("Step 11: Compiling results...\n")
  
  # Extract key findings
  key_coefficient <- NA
  policy_effect <- "Inconclusive"
  
  if ("EarlyAdopter:CrisisPeriod" %in% names(main_results)) {
    key_coef_info <- main_results[["EarlyAdopter:CrisisPeriod"]]
    key_coefficient <- key_coef_info$coefficient
    policy_effect <- key_coef_info$interpretation
  }
  
  summary_results <- list(
    analysis_period = paste(start_date, "to", end_date),
    n_countries = financial_data$summary$n_countries,
    countries = financial_data$countries,
    n_observations = nrow(tedid_dataset),
    crisis_period = paste(crisis_start, "to", crisis_end),
    contagion_established = contagion_established,
    early_adopters = sum(treatment_groups$Group == "EarlyAdopter"),
    late_adopters = sum(treatment_groups$Group == "LateAdopter"),
    key_coefficient = key_coefficient,
    policy_effect = policy_effect,
    te_matrices_computed = length(te_matrices),
    weight_types_used = names(network_weights)
  )
  
  # Compile final results
  final_results <- list(
    data = list(
      financial = financial_data,
      policy = policy_data,
      treatment_groups = treatment_groups
    ),
    networks = list(
      weights = network_weights,
      te_matrices = te_matrices,
      statistics = network_stats,
      nici = nici_data,
      nipi = nipi_data
    ),
    tedid = list(
      dataset = tedid_dataset,
      main_model = main_model,
      enhanced_model = enhanced_model,
      main_results = main_results,
      enhanced_results = enhanced_results
    ),
    plots = plots_list,
    summary = summary_results
  )
  
  if (verbose) {
    cat("\n=== Analysis Complete ===\n")
    cat("Countries analyzed:", summary_results$n_countries, "\n")
    cat("Total observations:", summary_results$n_observations, "\n")
    cat("Contagion established:", summary_results$contagion_established, "\n")
    cat("Policy effect:", summary_results$policy_effect, "\n")
    if (!is.na(key_coefficient)) {
      cat("Key coefficient:", round(key_coefficient, 4), "\n")
    }
  }
  
  return(final_results)
}

