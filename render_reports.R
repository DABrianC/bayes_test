# Script to render parameterized Quarto reports

library(quarto)

# Set the path to your Quarto document
quarto_doc <- "parameterized_report.qmd"

# Define a list of IDs to generate reports for
ids <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

# Loop through each ID and render a custom report
for (id in ids) {
  # Define output file name based on ID
  output_file <- paste0("Respondent_", id, "_Report", ".pdf")
  
  # Set parameters for this specific report
  params <- list(
    id = id
    )
  
  # Render the report with custom parameters
  quarto::quarto_render(
    input = quarto_doc,
    output_file = output_file,
    execute_params = params
  )
  
  cat("Generated report for ID:", id, "\n")
}

