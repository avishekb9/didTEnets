#' Create Network Comparison Visualizations
#'
#' Creates comprehensive network visualization plots showing the evolution
#' of financial networks across different crisis periods.
#'
#' @param te_matrices Named list of transfer entropy matrices
#' @param threshold Minimum TE value for network connections (default = 0.02)
#' @param layout Character string specifying network layout algorithm
#' @param title_prefix Character string for plot titles
#'
#' @return Named list of ggplot objects for each period
#'
#' @examples
#' \dontrun{
#' network_plots <- create_network_comparison(te_matrices)
#' print(network_plots$Pre_Crisis)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_text labs theme_void theme element_text
#' @importFrom ggraph ggraph geom_edge_arc geom_node_point geom_node_text scale_edge_width_continuous scale_edge_alpha_continuous scale_size_continuous scale_color_identity
#' @importFrom igraph graph_from_adjacency_matrix degree edge_density average.path.length is.connected V E
#' @importFrom tidygraph as_tbl_graph
#' @importFrom dplyr case_when
create_network_comparison <- function(te_matrices, threshold = 0.02, 
                                    layout = "fr", title_prefix = "Financial Contagion Network") {
  
  # Focus on crisis period analysis (Q0.1 - extreme negative events)
  crisis_periods <- c("Pre_Crisis", "Crisis", "Post_Crisis")
  network_plots <- list()
  
  for (period in crisis_periods) {
    matrix_name <- paste(period, "Q0.1", sep = "_")
    
    if (matrix_name %in% names(te_matrices)) {
      te_matrix <- te_matrices[[matrix_name]]
      
      # Apply threshold to focus on significant connections
      te_matrix_thresh <- te_matrix
      te_matrix_thresh[te_matrix_thresh < threshold] <- 0
      
      # Create igraph object
      g <- graph_from_adjacency_matrix(te_matrix_thresh, 
                                     mode = "directed", 
                                     weighted = TRUE, 
                                     diag = FALSE)
      
      # Add node attributes
      V(g)$size <- degree(g, mode = "in") * 3 + 5  # Size based on incoming connections
      V(g)$color <- case_when(
        V(g)$name %in% c("USA", "GBR") ~ "red",      # Major developed markets
        V(g)$name %in% c("DEU", "FRA", "JPN") ~ "blue", # Other developed markets
        V(g)$name %in% c("KOR", "AUS") ~ "green",    # Early adopters
        TRUE ~ "orange"                               # Emerging markets
      )
      
      # Add edge attributes
      E(g)$width <- E(g)$weight * 10  # Scale edge width by TE value
      E(g)$color <- "gray60"
      E(g)$arrow.size <- 0.5
      
      # Create tidy graph for ggraph
      tidy_g <- as_tbl_graph(g)
      
      # Calculate network statistics for subtitle
      density <- edge_density(g)
      avg_path_length <- ifelse(is.connected(g), average.path.length(g), NA)
      
      # Create plot
      p <- ggraph(tidy_g, layout = layout) +
        geom_edge_arc(aes(width = weight, alpha = weight), 
                     arrow = arrow(length = unit(2, "mm"), type = "closed"),
                     start_cap = circle(3, "mm"), end_cap = circle(3, "mm"),
                     color = "gray50") +
        geom_node_point(aes(size = size, color = color), alpha = 0.8) +
        geom_node_text(aes(label = name), repel = TRUE, size = 3, fontface = "bold") +
        scale_edge_width_continuous(range = c(0.5, 3), guide = "none") +
        scale_edge_alpha_continuous(range = c(0.3, 0.8), guide = "none") +
        scale_size_continuous(range = c(3, 12), guide = "none") +
        scale_color_identity() +
        labs(title = paste(title_prefix, ":", gsub("_", " ", period)),
             subtitle = paste("Density:", round(density, 3), 
                            "| Avg Path Length:", round(avg_path_length, 2)),
             caption = "Edge thickness = Transfer Entropy strength\nNode size = Incoming connections") +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          plot.caption = element_text(hjust = 0.5, size = 8)
        )
      
      network_plots[[period]] <- p
    }
  }
  
  return(network_plots)
}

#' Create Transfer Entropy Heatmaps
#'
#' Creates heatmap visualizations of transfer entropy matrices to show
#' the intensity and direction of information flows between countries.
#'
#' @param te_matrices Named list of transfer entropy matrices
#' @param periods Character vector of periods to visualize
#' @param quantile Character string specifying quantile to focus on
#'
#' @return Named list of ggplot heatmap objects
#'
#' @examples
#' \dontrun{
#' heatmaps <- create_te_heatmaps(te_matrices)
#' gridExtra::grid.arrange(grobs = heatmaps, ncol = 3)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c labs theme_minimal theme element_text
#' @importFrom viridis scale_fill_viridis
#' @importFrom dplyr mutate
create_te_heatmaps <- function(te_matrices, 
                              periods = c("Pre_Crisis", "Crisis", "Post_Crisis"),
                              quantile = "Q0.1") {
  
  heatmap_plots <- list()
  
  for (period in periods) {
    matrix_name <- paste(period, quantile, sep = "_")
    
    if (matrix_name %in% names(te_matrices)) {
      te_matrix <- te_matrices[[matrix_name]]
      
      # Convert to long format for ggplot
      te_long <- expand.grid(Source = rownames(te_matrix), 
                            Target = colnames(te_matrix)) %>%
        mutate(TE_Value = as.vector(te_matrix),
               Period = gsub("_", " ", period))
      
      # Create heatmap
      p <- ggplot(te_long, aes(x = Source, y = Target, fill = TE_Value)) +
        geom_tile() +
        scale_fill_viridis_c(name = "Transfer\nEntropy", option = "plasma") +
        labs(title = paste("Transfer Entropy Matrix:", te_long$Period[1]),
             subtitle = "Darker colors = stronger information flow",
             x = "Source Country", y = "Target Country") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
      
      heatmap_plots[[period]] <- p
    }
  }
  
  return(heatmap_plots)
}

#' Visualize TE-DiD Results
#'
#' Creates comprehensive visualizations for TE-DiD analysis results,
#' including time series plots and treatment effect visualizations.
#'
#' @param tedid_dataset data.frame with TE-DiD dataset
#' @param crisis_start Date when crisis period begins
#' @param group_var Character string specifying grouping variable
#' @param outcome_var Character string specifying outcome variable
#' @param treatment_var Character string specifying treatment variable
#'
#' @return Named list of ggplot objects
#'
#' @examples
#' \dontrun{
#' tedid_plots <- visualize_tedid_results(tedid_dataset)
#' print(tedid_plots$timeseries)
#' print(tedid_plots$difference)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_vline geom_hline geom_ribbon scale_color_manual scale_fill_manual labs theme_minimal theme element_text
#' @importFrom dplyr %>% group_by summarise mutate select
#' @importFrom tidyr pivot_wider
visualize_tedid_results <- function(tedid_dataset, 
                                   crisis_start = as.Date("2020-03-01"),
                                   group_var = "EarlyAdopter",
                                   outcome_var = "IncomingFlow",
                                   treatment_var = "NIPI") {
  
  plots_list <- list()
  
  # Prepare group labels
  tedid_dataset$Group <- ifelse(tedid_dataset[[group_var]] == 1, 
                               "Early Adopters", "Late Adopters")
  
  # Time series plot
  tryCatch({
    group_timeseries <- tedid_dataset %>%
      group_by(Date, Group, CrisisPeriod) %>%
      summarise(AvgIncomingFlow = mean(.data[[outcome_var]], na.rm = TRUE), 
                .groups = "drop") %>%
      mutate(Period = ifelse(CrisisPeriod == 1, "Crisis", "Pre-Crisis"))
    
    p1 <- ggplot(group_timeseries, aes(x = Date, y = AvgIncomingFlow, color = Group)) +
      geom_line(size = 1.2, alpha = 0.8) +
      geom_vline(xintercept = crisis_start, linetype = "dashed", color = "red", size = 1) +
      scale_color_manual(values = c("Early Adopters" = "blue", "Late Adopters" = "red")) +
      labs(title = "TE-DiD Analysis: Incoming Financial Contagion Over Time",
           subtitle = "Red line marks crisis start",
           x = "Date", y = "Average Incoming Transfer Entropy",
           color = "Policy Group") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    plots_list$timeseries <- p1
  }, error = function(e) {
    warning("Time series plot failed: ", e$message)
  })
  
  # Difference plot (parallel trends visualization)
  tryCatch({
    diff_data <- group_timeseries %>%
      select(Date, Group, AvgIncomingFlow) %>%
      pivot_wider(names_from = Group, values_from = AvgIncomingFlow) %>%
      mutate(Difference = `Early Adopters` - `Late Adopters`,
             Period = ifelse(Date >= crisis_start, "Crisis", "Pre-Crisis"))
    
    p2 <- ggplot(diff_data, aes(x = Date, y = Difference, color = Period)) +
      geom_line(size = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = crisis_start, linetype = "dashed", color = "red", size = 1) +
      scale_color_manual(values = c("Pre-Crisis" = "blue", "Crisis" = "red")) +
      labs(title = "TE-DiD: Treatment Effect Visualization",
           subtitle = "Difference in incoming contagion (Early Adopters - Late Adopters)",
           x = "Date", y = "Difference in Incoming TE",
           color = "Period") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    plots_list$difference <- p2
  }, error = function(e) {
    warning("Difference plot failed: ", e$message)
  })
  
  # Scatter plot: outcome vs treatment by period
  tryCatch({
    scatter_data <- tedid_dataset %>%
      mutate(Period = ifelse(CrisisPeriod == 1, "Crisis Period", "Pre-Crisis")) %>%
      group_by(Country, Period) %>%
      summarise(avg_outcome = mean(.data[[outcome_var]], na.rm = TRUE),
               avg_treatment = mean(.data[[treatment_var]], na.rm = TRUE), 
               .groups = "drop")
    
    p3 <- ggplot(scatter_data, aes(x = avg_treatment, y = avg_outcome, 
                                  color = Period, shape = Period)) +
      geom_point(size = 4, alpha = 0.8) +
      geom_text(aes(label = Country), vjust = -0.7, size = 3) +
      labs(title = "TE-DiD Analysis: Outcome vs Treatment by Period",
           subtitle = "Network-Informed Contagion vs Policy Intensity",
           x = paste("Average", treatment_var),
           y = paste("Average", outcome_var),
           color = "Period", shape = "Period") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    plots_list$scatter <- p3
  }, error = function(e) {
    warning("Scatter plot failed: ", e$message)
  })
  
  return(plots_list)
}

