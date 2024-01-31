
## Validate types of data inputs with error messaging
validate_df_cols <- function(data, pivot_cols) {
  if (!is.data.frame(data)) {
    stop("The input data is not a dataframe.")
  }

  if (nrow(data) == 0) {
    message("The dataframe is empty. No reshaping applied.")
    return(FALSE)
  }

  if (!is.character(pivot_cols) && !is.numeric(pivot_cols)) {
    stop("pivot_cols must be a vector of column names or indices.")
  }

  if (any(is.numeric(pivot_cols) & (pivot_cols < 1 | pivot_cols > ncol(data)))) {
    stop("Some indices in pivot_cols are out of the dataframe's column range.")
  }

  if (any(is.character(pivot_cols) & !(pivot_cols %in% names(data)))) {
    stop("Some column names in pivot_cols do not exist in the dataframe.")
  }

  return(TRUE)
}

validate_survey_data <- function(data, required_cols) {
  # Check for required columns
  if (!all(required_cols %in% names(data))) {
    stop("Not all required columns are present in the dataframe.")
  }
  return(TRUE)
}
