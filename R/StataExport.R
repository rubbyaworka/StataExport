
# Define the function
#' Epoeting crosstab of stata datasets into excel
#'
#' @param file_path
#' @param variables
#' @param output_excel
#' @param sheet_name
#'
#' @return
#' @export
#'
#' @examples
export_dynamic_crosstab <- function(file_path, variables, output_excel, sheet_name) {
  # Load the .dta file
  data <- read_dta(file = file_path) %>%
    mutate(across(where(is_labelled), ~ sjlabelled::as_character(.)))

  # Check if all specified variables exist in the data
  if (!all(variables %in% colnames(data))) {
    stop("One or more specified variables are not found in the dataset")
  }

  # Cross-tabulation: Frequency, Mean, Percentage
  crosstab <- data %>%
    group_by(across(all_of(variables))) %>%
    summarise(Frequency = n(),
              Mean = mean(as.numeric(!!sym(variables[1])), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Percentage = (Frequency / sum(Frequency)) * 100)

  # Print the cross-tabulation in the console
  print(crosstab)

  # Export the cross-tabulation to Excel
  write_xlsx(list(sheet_name = crosstab), path = output_excel)

  cat("Data exported to", output_excel, "in sheet", sheet_name, "\n")
}

# Example of how to call the function
# export_dynamic_crosstab("data_file.dta", c("region", "sex", "age"), "output_file.xlsx", "CrossTabSheet")

#export_dynamic_crosstab("survey_data.dta", c("region", "sex", "age"), "output.xlsx", "Summary")
