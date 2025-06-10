#' Prepare TE-DiD Dataset
#'
#' Prepares the dataset for Transfer Entropy-informed Difference-in-Differences
#' estimation by combining NICI outcomes, NIPI treatments, and policy data.
#'
#' @param nici_data Named list of NICI results from calculate_nici()
#' @param nipi_data Named list of NIPI results from calculate_nipi()
#' @param treatment_groups data.frame with treatment group assignments
#' @param policy_data data.frame with additional policy controls
#' @param weight_type Character string specifying which weight type to use
#' @param crisis_start Date when crisis period begins
#' @param scale_nici Numeric scaling factor for NICI (for better estimation)
#'
#' @return data.frame ready for TE-DiD estimation
#'
#' @examples
#' \dontrun{
#' # Prepare treatment groups
#' treatment_groups <- data.frame(
#'   CountryCode = c("USA", "GBR", "DEU"),
#'   Group = c("LateAdopter", "LateAdopter", "EarlyAdopter"),
#'   TreatmentDummy = c(0, 0, 1)
#' )
#' 
#' tedid_dataset <- prepare_tedid_dataset(nici_data, nipi_data, treatment_groups)
#' }
#'
#' @export
#' @importFrom dplyr %>% left_join select mutate group_by summarise
prepare_tedid_dataset <- function(nici_data, nipi_data, treatment_groups = NULL,
                                 policy_data = NULL, weight_type = "composite",
                                 crisis_start = as.Date("2020-03-01"),
                                 scale_nici = 1000) {
  
  # Select weight type for analysis
  if (!weight_type %in% names(nici_data)) {
    available_types <- names(nici_data)
    weight_type <- available_types[1]
    warning("Requested weight type not available, using: ", weight_type)
  }
  
  nici_sample <- nici_data[[weight_type]]
  nipi_sample <- nipi_data[[weight_type]]
  
  if (is.null(nici_sample) || is.null(nipi_sample)) {
    stop("NICI or NIPI data not available for weight type: ", weight_type)
  }
  
  # Create panel dataset
  panel_list <- list()
  dates <- nici_sample$dates
  countries <- intersect(nici_sample$countries, nipi_sample$countries)
  
  for (i in 1:length(dates)) {
    for (j in 1:length(countries)) {
      country <- countries[j]
      date <- dates[i]
      
      # Extract NICI value (scaled for realistic estimation)
      if (country %in% colnames(nici_sample$values)) {
        nici_value <- nici_sample$values[i, country] * scale_nici
      } else {
        nici_value <- 0
      }
      
      # Extract NIPI value
      if (country %in% rownames(nipi_sample$values)) {
        nipi_value <- nipi_sample$values[country, i]
      } else {
        nipi_value <- 30  # Default moderate stringency
      }
      
      # Create basic panel row
      panel_row <- data.frame(
        Country = country,
        Date = date,
        YearMonth = format(date, "%Y-%m"),
        IncomingFlow = nici_value,  # Y_it from did.txt equation
        NIPI = nipi_value,
        CrisisPeriod = as.numeric(date >= crisis_start),
        Year = as.numeric(format(date, "%Y")),
        Month = as.numeric(format(date, "%m")),
        stringsAsFactors = FALSE
      )
      
      panel_list[[length(panel_list) + 1]] <- panel_row
    }
  }
  
  # Combine into data frame
  tedid_dataset <- do.call(rbind, panel_list)
  
  # Add treatment group information if provided
  if (!is.null(treatment_groups)) {
    tedid_dataset <- tedid_dataset %>%
      left_join(treatment_groups %>% 
                select(CountryCode, Group, TreatmentDummy), 
                by = c("Country" = "CountryCode"))
    
    # Handle missing treatment assignments
    tedid_dataset$TreatmentDummy[is.na(tedid_dataset$TreatmentDummy)] <- 0
    tedid_dataset$EarlyAdopter <- tedid_dataset$TreatmentDummy
  } else {
    # Create default treatment assignment based on policy timing
    tedid_dataset$EarlyAdopter <- 0  # Default to control group
  }
  
  # Add policy controls if provided
  if (!is.null(policy_data)) {
    monthly_policy_summary <- policy_data %>%
      mutate(YearMonth = format(Date, "%Y-%m")) %>%
      group_by(CountryCode, YearMonth) %>%
      summarise(AvgStringency = mean(StringencyIndex, na.rm = TRUE), .groups = "drop")
    
    tedid_dataset <- tedid_dataset %>%
      left_join(monthly_policy_summary, 
                by = c("Country" = "CountryCode", "YearMonth" = "YearMonth"))
    
    # Handle missing policy data
    tedid_dataset$AvgStringency[is.na(tedid_dataset$AvgStringency)] <- 30
  }
  
  return(tedid_dataset)
}

#' Estimate TE-DiD Model
#'
#' Estimates the Transfer Entropy-informed Difference-in-Differences model
#' using panel data methods with two-way fixed effects.
#'
#' @param tedid_dataset data.frame prepared by prepare_tedid_dataset()
#' @param formula Character string or formula object for model specification
#' @param method Character string specifying estimation method ("plm" or "lm")
#' @param robust Logical indicating whether to use robust standard errors
#'
#' @return Model object (plm or lm) with estimation results
#'
#' @examples
#' \dontrun{
#' # Basic TE-DiD model
#' model1 <- estimate_tedid_model(tedid_dataset)
#' 
#' # Enhanced model with controls
#' model2 <- estimate_tedid_model(tedid_dataset, 
#'   formula = "IncomingFlow ~ EarlyAdopter * CrisisPeriod + AvgStringency")
#' }
#'
#' @export
#' @importFrom plm plm
#' @importFrom stats lm as.formula
estimate_tedid_model <- function(tedid_dataset, 
                                formula = "IncomingFlow ~ EarlyAdopter * CrisisPeriod",
                                method = "plm", robust = FALSE) {
  
  # Convert character formula to formula object if needed
  if (is.character(formula)) {
    formula <- as.formula(formula)
  }
  
  # Check for required variables
  required_vars <- c("IncomingFlow", "Country", "YearMonth")
  missing_vars <- setdiff(required_vars, names(tedid_dataset))
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  if (method == "plm") {
    # Try PLM estimation with two-way fixed effects
    tryCatch({
      model <- plm(formula, 
                  data = tedid_dataset,
                  index = c("Country", "YearMonth"),
                  model = "within",
                  effect = "twoways")
      
      if (robust) {
        # Add robust standard errors if requested
        # Note: Implementation would require additional packages like sandwich
        warning("Robust standard errors not implemented yet")
      }
      
      return(model)
    }, error = function(e) {
      warning("PLM estimation failed: ", e$message, "\nFalling back to OLS")
      method <- "lm"
    })
  }
  
  if (method == "lm") {
    # Fallback to OLS with fixed effects
    # Modify formula to include fixed effects
    formula_terms <- terms(formula)
    response_var <- as.character(formula)[2]
    predictor_vars <- as.character(formula)[3]
    
    # Add fixed effects to formula
    ols_formula <- as.formula(paste(response_var, "~", predictor_vars, 
                                   "+ Country + factor(YearMonth)"))
    
    model <- lm(ols_formula, data = tedid_dataset)
    return(model)
  }
}

#' Extract TE-DiD Results
#'
#' Extracts and formats key results from TE-DiD model estimation,
#' including treatment effects, standard errors, and economic interpretation.
#'
#' @param model Model object from estimate_tedid_model()
#' @param tedid_dataset Original dataset used for estimation
#' @param interaction_terms Character vector of interaction terms to extract
#'
#' @return Named list with model results and interpretation
#'
#' @examples
#' \dontrun{
#' model <- estimate_tedid_model(tedid_dataset)
#' results <- extract_tedid_results(model, tedid_dataset)
#' print(results$summary)
#' }
#'
#' @export
extract_tedid_results <- function(model, tedid_dataset = NULL, 
                                 interaction_terms = c("EarlyAdopter:CrisisPeriod")) {
  
  # Get model summary
  model_summary <- summary(model)
  
  # Extract coefficient table
  if (inherits(model, "plm")) {
    coef_table <- model_summary$coefficients
  } else {
    coef_table <- summary(model)$coefficients
  }
  
  # Extract key results
  results <- list(
    model = model,
    summary = model_summary,
    coefficients = coef_table
  )
  
  # Look for interaction terms
  for (term in interaction_terms) {
    if (term %in% rownames(coef_table)) {
      interaction_coef <- coef_table[term, "Estimate"]
      interaction_se <- coef_table[term, "Std. Error"]
      
      # Get p-value (column name varies)
      p_col <- which(colnames(coef_table) %in% c("Pr(>|t|)", "p-value", "P(>|t|)"))
      if (length(p_col) > 0) {
        interaction_pval <- coef_table[term, p_col[1]]
      } else {
        interaction_pval <- NA
      }
      
      # Economic interpretation
      interpretation <- list(
        coefficient = interaction_coef,
        std_error = interaction_se,
        p_value = interaction_pval,
        significant = !is.na(interaction_pval) && interaction_pval < 0.05
      )
      
      # Add economic magnitude if dataset provided
      if (!is.null(tedid_dataset) && "IncomingFlow" %in% names(tedid_dataset)) {
        mean_outcome <- mean(tedid_dataset$IncomingFlow, na.rm = TRUE)
        if (mean_outcome != 0) {
          interpretation$effect_magnitude_pct <- abs(interaction_coef) / mean_outcome * 100
        }
      }
      
      # Add interpretation text
      if (interpretation$significant) {
        if (interaction_coef < 0) {
          interpretation$interpretation <- "Early adoption of stringent policies REDUCED incoming financial contagion"
        } else {
          interpretation$interpretation <- "Early adoption of stringent policies INCREASED incoming financial contagion"
        }
      } else {
        interpretation$interpretation <- "No statistically significant policy effect detected"
      }
      
      results[[term]] <- interpretation
      break  # Use first found interaction term
    }
  }
  
  return(results)
}

