# Load necessary library
library(xtable)

# Define the function
create_model_table <- function(..., metrics = c("Party", "Coefficient", "Standard Error", "p Value", "BW Type", "Bandwidth", "N", "Effective N"), output_type = c("latex", "text")) {
  models <- list(...)
  output_type <- match.arg(output_type)
  
  # Initialize an empty list to store the values for each model
  model_values <- list()
  
  for (model in models) {
    coef_value <- round(model$coef[3], 3)
    se_value <- round(model$se[3], 3)
    pv_value <- round(model$pv[3], 3)
    bw_type <- ifelse(model$bwselect == "mserd", "MSE", "CER")
    bws_value <- round(model$bws[1], 3)
    N_sum <- sum(model$N)
    N_b_sum <- sum(model$N_h)
    party <- "NA"
    
    model_values[[length(model_values) + 1]] <- c(party, coef_value, se_value, pv_value, bw_type, bws_value, N_sum, N_b_sum)
  }
  
  # Combine the values into a data frame
  result_table <- data.frame(
    Metric = metrics,
    do.call(cbind, model_values)
  )
  
  if (output_type == "latex") {
    # Convert the data frame to a LaTeX table using xtable
    latex_table <- xtable(result_table, include.rownames = FALSE)
    
    # Print the LaTeX table
    print(latex_table, type = "latex")
  } else if (output_type == "text") {
    # Print the data frame as plain text
    print(result_table)
  }
}

# Example usage of the function
# Assuming you have model objects model1, model2, etc.
# create_model_table(model1, model2, model3, output_type = "latex")
# create_model_table(model1, model2, model3, output_type = "text")