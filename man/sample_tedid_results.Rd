% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{sample_tedid_results}
\alias{sample_tedid_results}
\title{Sample TE-DiD Analysis Results}
\format{
A named list containing:
\describe{
\item{data}{List with financial, policy, and treatment group data}
\item{networks}{List with network weights, TE matrices, and statistics}
\item{tedid}{List with TE-DiD models and results}
\item{summary}{Analysis summary statistics}
}
}
\usage{
sample_tedid_results
}
\description{
Pre-computed results from a complete TE-DiD analysis using the sample datasets,
provided for quick demonstration and testing purposes.
}
\details{
This dataset contains the complete output from \code{run_complete_tedid_analysis()}
using the sample data, allowing users to explore results structure and
visualization options without running the full analysis pipeline.

Key components include:
\itemize{
\item Transfer entropy matrices for different periods
\item Network-Informed Contagion Index (NICI) values
\item Network-Informed Policy Intensity (NIPI) measures
\item TE-DiD model estimation results
\item Network statistics and contagion analysis
}
}
\examples{
\dontrun{
data(sample_tedid_results)

# View analysis summary
print(sample_tedid_results$summary)

# Access model results
summary(sample_tedid_results$tedid$main_model)

# Network statistics
head(sample_tedid_results$networks$statistics)
}

}
\keyword{datasets}
