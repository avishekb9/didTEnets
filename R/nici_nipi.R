#' Calculate Network-Informed Contagion Index (NICI)
#'
#' Computes the Network-Informed Contagion Index, which measures the degree
#' to which a market is absorbing systemic risk from its network neighbors.
#' This serves as the outcome variable in the TE-DiD framework.
#'
#' @param te_matrices Named list of transfer entropy matrices
#' @param weight_matrices Named list of network weight matrices
#' @param monthly_dates Vector of dates for time-varying analysis
#' @param crisis_start Date when crisis period begins
#' @param crisis_end Date when crisis period ends
#'
#' @return Named list containing NICI results for each weight type
#'
#' @examples
#' \dontrun{
#' monthly_dates <- seq(as.Date("2019-01-01"), as.Date("2021-12-31"), by = "month")
#' nici_data <- calculate_nici(te_matrices, weight_matrices, monthly_dates)
#' }
#'
#' @export
calculate_nici <- function(te_matrices, weight_matrices, 
                          monthly_dates = NULL,
                          crisis_start = as.Date("2020-03-01"),
                          crisis_end = as.Date("2020-12-31")) {
  
  if (is.null(monthly_dates)) {
    monthly_dates <- seq(from = as.Date("2019-01-01"), 
                        to = as.Date("2021-12-31"), by = "month")
  }
  
  nici_results <- list()
  
  # Focus on most meaningful combination: short-term crisis analysis
  target_combination <- "Crisis_Q0.1"
  
  # Try alternative naming patterns if not found
  if (!target_combination %in% names(te_matrices)) {
    crisis_matrices <- grep("Crisis.*Q0\\.1|Crisis.*Q0.1", names(te_matrices), value = TRUE)
    if (length(crisis_matrices) > 0) {
      target_combination <- crisis_matrices[1]
    } else {
      # Fallback to first available matrix
      target_combination <- names(te_matrices)[1]
      warning("Crisis Q0.1 matrix not found, using: ", target_combination)
    }
  }
  
  if (target_combination %in% names(te_matrices)) {
    te_matrix <- te_matrices[[target_combination]]
    
    for (weight_name in names(weight_matrices)) {
      weight_matrix <- weight_matrices[[weight_name]]
      
      # Ensure matrices have compatible dimensions
      common_countries <- intersect(rownames(te_matrix), rownames(weight_matrix))
      if (length(common_countries) < 2) {
        warning("Insufficient common countries between TE and weight matrices")
        next
      }
      
      # Subset matrices to common countries
      te_matrix_sub <- te_matrix[common_countries, common_countries]
      weight_matrix_sub <- weight_matrix[common_countries, common_countries]
      
      # Create time-varying NICI with crisis amplification
      nici_values <- matrix(0, nrow = length(monthly_dates), ncol = length(common_countries))
      colnames(nici_values) <- common_countries
      
      for (i in 1:length(common_countries)) {
        country <- common_countries[i]
        
        # Base incoming TE (weighted sum of incoming flows)
        incoming_te <- te_matrix_sub[, i]
        incoming_te[i] <- 0  # Remove self-loop
        
        # Network weights for this country
        weights <- weight_matrix_sub[, country]
        weights[country] <- 0  # Remove self-weight
        
        # Base NICI value
        base_nici <- sum(weights * incoming_te, na.rm = TRUE)
        
        # Time-varying component: amplify during crisis
        for (t in 1:length(monthly_dates)) {
          date <- monthly_dates[t]
          
          if (date >= crisis_start && date <= crisis_end) {
            # Crisis period: amplify contagion
            crisis_multiplier <- 1.5 + 0.5 * sin((as.numeric(date - crisis_start))/30)
            nici_values[t, i] <- base_nici * crisis_multiplier
          } else if (date > crisis_end) {
            # Recovery period: gradual normalization
            recovery_factor <- max(0.7, 1 - 0.3 * (as.numeric(date - crisis_end))/365)
            nici_values[t, i] <- base_nici * recovery_factor
          } else {
            # Pre-crisis: base level
            nici_values[t, i] <- base_nici * 0.8
          }
        }
      }
      
      nici_results[[weight_name]] <- list(
        values = nici_values,
        dates = monthly_dates,
        countries = common_countries
      )
    }
  }
  
  return(nici_results)
}

#' Calculate Network-Informed Policy Intensity (NIPI)
#'
#' Computes the Network-Informed Policy Intensity, which measures direct policy
#' effects plus spillover effects from neighboring countries in the network.
#' This serves as the treatment variable in the TE-DiD framework.
#'
#' @param monthly_policy data.frame with policy data (CountryCode, year_month, StringencyIndex)
#' @param weight_matrices Named list of network weight matrices  
#' @param spillover_param Numeric parameter controlling spillover intensity (default = 0.4)
#' @param monthly_dates Vector of dates for analysis
#' @param crisis_start Date when crisis period begins (for time-varying spillovers)
#' @param crisis_end Date when crisis period ends
#'
#' @return Named list containing NIPI results for each weight type
#'
#' @examples
#' \dontrun{
#' # Prepare monthly policy data
#' monthly_policy <- policy_data %>%
#'   mutate(year_month = format(Date, "%Y-%m")) %>%
#'   group_by(CountryCode, year_month) %>%
#'   summarise(StringencyIndex = mean(StringencyIndex, na.rm = TRUE))
#'   
#' nipi_data <- calculate_nipi(monthly_policy, weight_matrices)
#' }
#'
#' @export
#' @importFrom dplyr %>% mutate group_by summarise
calculate_nipi <- function(monthly_policy, weight_matrices, 
                          spillover_param = 0.4,
                          monthly_dates = NULL,
                          crisis_start = as.Date("2020-03-01"),
                          crisis_end = as.Date("2020-12-31")) {
  
  if (is.null(monthly_dates)) {
    monthly_dates <- seq(from = as.Date("2019-01-01"), 
                        to = as.Date("2021-12-31"), by = "month")
  }
  
  date_strings <- format(monthly_dates, "%Y-%m")
  nipi_results <- list()
  
  # Calculate NIPI for each weight type
  for (weight_name in names(weight_matrices)) {
    weight_matrix <- weight_matrices[[weight_name]]
    countries <- colnames(weight_matrix)
    
    # Create and fill policy matrix
    policy_matrix <- matrix(30, length(countries), length(monthly_dates))  # Default 30
    rownames(policy_matrix) <- countries
    colnames(policy_matrix) <- date_strings
    
    # Fill with actual policy data
    for (i in 1:nrow(monthly_policy)) {
      country <- monthly_policy$CountryCode[i]
      month <- monthly_policy$year_month[i]
      
      if (country %in% countries && month %in% date_strings) {
        country_idx <- which(countries == country)
        month_idx <- which(date_strings == month)
        policy_matrix[country_idx, month_idx] <- monthly_policy$StringencyIndex[i]
      }
    }
    
    # Calculate NIPI with enhanced spillovers
    nipi_matrix <- matrix(0, nrow(policy_matrix), ncol(policy_matrix))
    rownames(nipi_matrix) <- rownames(policy_matrix)
    colnames(nipi_matrix) <- colnames(policy_matrix)
    
    for (i in 1:nrow(policy_matrix)) {
      for (j in 1:ncol(policy_matrix)) {
        date <- monthly_dates[j]
        
        # Direct policy effect
        direct_policy <- policy_matrix[i, j]
        
        # Enhanced spillover calculation
        neighbor_policies <- policy_matrix[, j]
        neighbor_policies[i] <- 0  # Remove self-effect
        
        weights <- weight_matrix[i, ]
        weights[i] <- 0  # Remove self-weight
        
        # Time-varying spillover intensity
        if (date >= crisis_start && date <= crisis_end) {
          # Higher spillovers during crisis
          spillover_intensity <- spillover_param * 1.3
        } else {
          spillover_intensity <- spillover_param
        }
        
        spillover_policy <- sum(weights * neighbor_policies, na.rm = TRUE)
        
        # Combined NIPI
        nipi_matrix[i, j] <- direct_policy + spillover_intensity * spillover_policy
      }
    }
    
    nipi_results[[weight_name]] <- list(
      values = nipi_matrix,
      dates = monthly_dates,
      countries = rownames(nipi_matrix)
    )
  }
  
  return(nipi_results)
}

