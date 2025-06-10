# didTEnets

<!-- badges: start -->
[![R-CMD-check](https://github.com/avishekb9/didTEnets/workflows/R-CMD-check/badge.svg)](https://github.com/avishekb9/didTEnets/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/didTEnets)](https://CRAN.R-project.org/package=didTEnets)
[![codecov](https://codecov.io/gh/avishekb9/didTEnets/branch/main/graph/badge.svg)](https://codecov.io/gh/avishekb9/didTEnets)
<!-- badges: end -->

**Transfer Entropy-Informed Difference-in-Differences for Network Analysis**

The `didTEnets` package implements the Transfer Entropy-informed Difference-in-Differences (TE-DiD) framework for policy evaluation in interconnected systems. This methodology combines network analysis with causal inference to measure policy effects on financial contagion and systemic risk transmission.

## Overview

Traditional difference-in-differences approaches often fail to capture the complex dynamics of systemic risk transmission in interconnected financial markets. The TE-DiD framework addresses this by:

- 🌐 **Network-Informed Outcomes**: Using directed information flows instead of simple returns
- 📊 **Crisis-Focused Analysis**: Quantile conditioning to focus on extreme events  
- 🔄 **Policy Spillovers**: Accounting for spillover effects through network weights
- 📈 **Comprehensive Visualization**: Network evolution and policy effect plots

## Key Features

- **Transfer Entropy Calculation**: Enhanced TE with quantile conditioning and crisis analysis
- **Network Weight Construction**: Multiple weight types (correlation, geographic, economic, composite)
- **NICI Construction**: Network-Informed Contagion Index as sophisticated outcome variable
- **NIPI Construction**: Network-Informed Policy Intensity with spillover effects
- **TE-DiD Estimation**: Robust econometric models with two-way fixed effects
- **Network Visualization**: Comprehensive plots for network evolution and policy effects

## Installation

You can install the development version of didTEnets from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install didTEnets
devtools::install_github("avishekb9/didTEnets")
```

## Quick Start

```r
library(didTEnets)

# Define financial markets for analysis
tickers <- c("^GSPC" = "USA", "^FTSE" = "GBR", "^GDAXI" = "DEU", 
             "^FCHI" = "FRA", "^N225" = "JPN", "^AXJO" = "AUS")

# Run complete TE-DiD analysis
results <- run_complete_tedid_analysis(
  tickers = tickers,
  start_date = "2019-01-01",
  end_date = "2021-12-31",
  create_plots = TRUE
)

# View main results
summary(results$tedid$main_model)
print(results$summary)

# Access visualizations
if (!is.null(results$plots)) {
  print(results$plots$tedid_plots$timeseries)
  print(results$plots$network_comparison$Crisis)
}
```

## Methodology

The TE-DiD framework implements the following equation:

**IncomingFlow_{it} = β₀ + β₁ · EarlyAdopter_i × CrisisPeriod_t + α_i + γ_t + ε_{it}**

Where:
- **IncomingFlow_{it}**: Network-Informed Contagion Index (NICI) - the outcome variable
- **EarlyAdopter_i**: Treatment group indicator (early vs. late policy adopters)  
- **CrisisPeriod_t**: Crisis period indicator
- **β₁**: Key coefficient measuring policy effect on financial contagion
- **α_i**: Country fixed effects
- **γ_t**: Time fixed effects

## Example Output

```r
# Sample model results
#                            Estimate Std. Error t-value Pr(>|t|)  
# EarlyAdopter:CrisisPeriod -0.0052    0.0021    -2.476   0.014 *

# Interpretation: Early adoption of stringent policies REDUCED 
# incoming financial contagion by 0.52% during the crisis period
```

## Core Functions

| Function | Purpose |
|----------|---------|
| `run_complete_tedid_analysis()` | Complete analysis pipeline |
| `download_financial_data()` | Financial data acquisition |
| `calculate_enhanced_transfer_entropy()` | Transfer entropy calculation |
| `calculate_network_weights()` | Network weight construction |
| `calculate_nici()` | Network-Informed Contagion Index |
| `calculate_nipi()` | Network-Informed Policy Intensity |
| `estimate_tedid_model()` | TE-DiD model estimation |
| `create_network_comparison()` | Network visualization |

## Workflow

```r
# 1. Data Acquisition
financial_data <- download_financial_data(tickers, "2019-01-01", "2021-12-31")
policy_data <- create_policy_data(financial_data$countries, "2019-01-01", "2021-12-31")

# 2. Network Analysis  
network_weights <- calculate_network_weights(financial_data$returns)
te_matrices <- calculate_te_matrices_by_period(financial_data$returns, period_breaks)

# 3. Variable Construction
nici_data <- calculate_nici(te_matrices, network_weights)
nipi_data <- calculate_nipi(monthly_policy, network_weights)

# 4. TE-DiD Estimation
tedid_dataset <- prepare_tedid_dataset(nici_data, nipi_data, treatment_groups)
tedid_model <- estimate_tedid_model(tedid_dataset)

# 5. Visualization
network_plots <- create_network_comparison(te_matrices)
tedid_plots <- visualize_tedid_results(tedid_dataset)
```

## Applications

The TE-DiD framework is particularly useful for:

- **Crisis Policy Evaluation**: Measuring policy effectiveness during financial crises
- **Contagion Analysis**: Understanding systemic risk transmission channels
- **Network Evolution**: Tracking changes in financial network structure
- **Spillover Assessment**: Quantifying cross-border policy effects
- **Systemic Risk Monitoring**: Real-time financial stability analysis

## Documentation

- [Introduction to didTEnets](https://avishekb9.github.io/didTEnets/articles/introduction.html) - Basic concepts and workflow
- [Network Analysis](https://avishekb9.github.io/didTEnets/articles/network_analysis.html) - Detailed network construction methods
- [Function Reference](https://avishekb9.github.io/didTEnets/reference/index.html) - Complete function documentation

## Sample Data

The package includes sample datasets for testing and demonstration:

- `sample_financial_returns`: Daily returns for 6 major stock indices (2019-2021)
- `sample_policy_data`: COVID-19 policy stringency data
- `sample_tedid_results`: Pre-computed analysis results

```r
# Load and explore sample data
data(sample_financial_returns)
data(sample_policy_data)
head(sample_financial_returns)
```

## Performance Considerations

For large networks (>20 countries) or long time periods:

- Transfer entropy calculation can be computationally intensive
- Consider parallel processing for multiple periods/quantiles  
- Use appropriate thresholds for network visualization
- Cache intermediate results for repeated analysis

## Citation

If you use didTEnets in your research, please cite:

```
@Manual{didTEnets,
  title = {didTEnets: Transfer Entropy-Informed Difference-in-Differences for Network Analysis},
  author = {Avishek Bhandari},
  year = {2024},
  note = {R package version 0.1.0},
  url = {https://github.com/avishekb9/didTEnets}
}
```

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## References

1. Schreiber, T. (2000). Measuring information transfer. *Physical Review Letters*, 85(2), 461.
2. Angrist, J. D., & Pischke, J. S. (2009). *Mostly harmless econometrics: An empiricist's companion*. Princeton University Press.
3. Hale, T., Angrist, N., Goldszmidt, R., et al. (2021). A global panel database of pandemic policies (Oxford COVID-19 Government Response Tracker). *Nature Human Behaviour*, 5, 529-538.

## Related Work

The TE-DiD framework builds on several methodological advances:

- **Network Econometrics**: Spatial models and spillover analysis
- **Information Theory**: Transfer entropy and directed information measures  
- **Causal Inference**: Difference-in-differences and policy evaluation
- **Financial Networks**: Systemic risk and contagion analysis

---

## Support

If you encounter any issues or have questions:

- 📋 [File an issue](https://github.com/avishekb9/didTEnets/issues)
- 📖 [Read the documentation](https://avishekb9.github.io/didTEnets/)
- 💬 [Start a discussion](https://github.com/avishekb9/didTEnets/discussions)
