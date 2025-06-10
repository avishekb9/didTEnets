# Fix import issues in roxygen comments
files_to_fix <- c(
  "/home/avisek/ManyIVsNets/didTEnets/R/main_analysis.R",
  "/home/avisek/ManyIVsNets/didTEnets/R/nici_nipi.R", 
  "/home/avisek/ManyIVsNets/didTEnets/R/tedid_estimation.R",
  "/home/avisek/ManyIVsNets/didTEnets/R/visualization.R"
)

# Remove problematic import statements
for (file in files_to_fix) {
  if (file.exists(file)) {
    content <- readLines(file)
    
    # Remove problematic imports
    content <- content[!grepl("@importFrom lubridate as.Date", content)]
    content <- content[!grepl("@importFrom stats summary", content)]
    content <- content[!grepl("@importFrom ggraph scale_size_continuous", content)]
    content <- content[!grepl("@importFrom ggraph scale_color_identity", content)]
    content <- content[!grepl("@importFrom tidyr expand.grid", content)]
    content <- content[!grepl("@importFrom dplyr pivot_wider", content)]
    
    writeLines(content, file)
  }
}

cat("Fixed import statements\n")