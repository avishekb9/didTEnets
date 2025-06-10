#' Calculate Network Weights
#'
#' Computes various types of network weights for use in network-informed analysis.
#' Supports correlation-based, geographic, economic similarity, and composite weights.
#'
#' @param returns_data xts object or matrix with financial returns
#' @param weight_types Character vector specifying which weights to calculate
#'   Options: "correlation", "geographic", "economic", "composite"
#' @param geographic_groups Optional named list defining geographic regions
#'
#' @return Named list containing different weight matrices
#'
#' @examples
#' \dontrun{
#' # Calculate all weight types
#' weights <- calculate_network_weights(returns_data)
#' 
#' # Calculate only correlation weights
#' cor_weights <- calculate_network_weights(returns_data, "correlation")
#' }
#'
#' @export
#' @importFrom stats cor sd
calculate_network_weights <- function(returns_data, 
                                    weight_types = c("correlation", "geographic", "economic", "composite"),
                                    geographic_groups = NULL) {
  
  n <- ncol(returns_data)
  country_names <- colnames(returns_data)
  weights_list <- list()
  
  # 1. Correlation-based weights
  if ("correlation" %in% weight_types) {
    cor_matrix <- cor(returns_data, use = "complete.obs")
    w_correlation <- pmax(0, cor_matrix)  # Only positive correlations
    
    # Ensure matrix structure
    if (!is.matrix(w_correlation)) {
      w_correlation <- as.matrix(w_correlation)
    }
    
    diag(w_correlation) <- 0
    
    # Row normalize (avoiding division by zero)
    row_sums <- rowSums(w_correlation)
    row_sums[row_sums == 0] <- 1
    w_correlation <- w_correlation / row_sums
    
    weights_list$correlation <- w_correlation
  }
  
  # 2. Economic development similarity weights
  if ("economic" %in% weight_types) {
    # Proxy using market volatility patterns
    volatilities <- apply(returns_data, 2, sd, na.rm = TRUE)
    w_economic <- matrix(0, n, n)
    rownames(w_economic) <- colnames(w_economic) <- country_names
    
    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {
          vol_similarity <- exp(-abs(volatilities[i] - volatilities[j]) / max(volatilities))
          w_economic[i, j] <- vol_similarity
        }
      }
    }
    
    row_sums <- rowSums(w_economic)
    row_sums[row_sums == 0] <- 1
    w_economic <- w_economic / row_sums
    
    weights_list$economic <- w_economic
  }
  
  # 3. Geographic/regional weights
  if ("geographic" %in% weight_types) {
    w_geographic <- matrix(0.1, n, n)  # Base connectivity
    rownames(w_geographic) <- colnames(w_geographic) <- country_names
    diag(w_geographic) <- 0
    
    # Default regional clusters if not provided
    if (is.null(geographic_groups)) {
      geographic_groups <- list(
        asia_pacific = c("JPN", "KOR", "AUS", "HKG", "IND"),
        europe = c("GBR", "DEU", "FRA"),
        americas = c("USA", "BRA", "CAN", "MEX")
      )
    }
    
    # Higher weights within regions
    for (region in geographic_groups) {
      region_countries <- intersect(region, country_names)
      for (i in region_countries) {
        for (j in region_countries) {
          if (i != j) {
            w_geographic[i, j] <- 0.7
          }
        }
      }
    }
    
    row_sums <- rowSums(w_geographic)
    row_sums[row_sums == 0] <- 1
    w_geographic <- w_geographic / row_sums
    
    weights_list$geographic <- w_geographic
  }
  
  # 4. Composite weights
  if ("composite" %in% weight_types) {
    if ("correlation" %in% names(weights_list) && 
        "economic" %in% names(weights_list) && 
        "geographic" %in% names(weights_list)) {
      w_composite <- 0.5 * weights_list$correlation + 
                     0.3 * weights_list$economic + 
                     0.2 * weights_list$geographic
    } else if ("correlation" %in% names(weights_list) && 
               "geographic" %in% names(weights_list)) {
      w_composite <- 0.6 * weights_list$correlation + 
                     0.4 * weights_list$geographic
    } else if ("correlation" %in% names(weights_list)) {
      w_composite <- weights_list$correlation
    } else {
      # Create basic composite from correlation
      cor_matrix <- cor(returns_data, use = "complete.obs")
      w_composite <- pmax(0, cor_matrix)
      diag(w_composite) <- 0
      row_sums <- rowSums(w_composite)
      row_sums[row_sums == 0] <- 1
      w_composite <- w_composite / row_sums
    }
    
    weights_list$composite <- w_composite
  }
  
  return(weights_list)
}

#' Calculate Network Statistics
#'
#' Computes comprehensive network statistics for transfer entropy matrices
#' to analyze network evolution and establish contagion patterns.
#'
#' @param te_matrices Named list of transfer entropy matrices
#' @param threshold Minimum TE value to consider for network connections
#'
#' @return data.frame with network statistics for each matrix
#'
#' @examples
#' \dontrun{
#' network_stats <- calculate_network_statistics(te_matrices)
#' print(network_stats)
#' }
#'
#' @export
#' @importFrom igraph graph_from_adjacency_matrix edge_density transitivity strength
calculate_network_statistics <- function(te_matrices, threshold = 0.001) {
  stats_results <- data.frame()
  
  for (matrix_name in names(te_matrices)) {
    te_matrix <- te_matrices[[matrix_name]]
    
    # Parse period and quantile
    parts <- strsplit(matrix_name, "_")[[1]]
    if (length(parts) >= 2) {
      period <- paste(parts[1:(length(parts)-1)], collapse = "_")
      quantile <- parts[length(parts)]
    } else {
      period <- matrix_name
      quantile <- "Unknown"
    }
    
    # Apply threshold and create network graph
    te_matrix_thresh <- te_matrix
    te_matrix_thresh[te_matrix_thresh < threshold] <- 0
    
    g <- graph_from_adjacency_matrix(te_matrix_thresh, 
                                   mode = "directed", 
                                   weighted = TRUE, 
                                   diag = FALSE)
    
    # Calculate statistics
    density <- edge_density(g)
    transitivity_global <- transitivity(g, type = "global")
    if (is.na(transitivity_global)) transitivity_global <- 0
    
    avg_strength_in <- mean(strength(g, mode = "in"))
    avg_strength_out <- mean(strength(g, mode = "out"))
    max_te <- max(te_matrix)
    mean_te_nonzero <- mean(te_matrix[te_matrix > 0])
    if (is.na(mean_te_nonzero)) mean_te_nonzero <- 0
    
    # Add to results
    stats_results <- rbind(stats_results, data.frame(
      Period = period,
      Quantile = quantile,
      Matrix_Name = matrix_name,
      Density = density,
      Transitivity = transitivity_global,
      Avg_Strength_In = avg_strength_in,
      Avg_Strength_Out = avg_strength_out,
      Max_TE = max_te,
      Mean_TE = mean_te_nonzero,
      stringsAsFactors = FALSE
    ))
  }
  
  return(stats_results)
}