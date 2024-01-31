#' Collapse Survey Weights and Calculate Proportions
#'
#' @param data A dataframe containing survey data.
#' @param depvar Name of the dependent variable column.
#' @param response Name of the response variable column.
#' @param wgt Name of the weight variable column.
#' @param grouping_vars Vector of strings of columns used for additional grouping, NULL if no additional grouping is needed.
#' @param verbose Boolean indicating if detailed output should be printed.
#' @import dplyr
#' @import survey
#' @export
survey_collapse <- function(data, depvar, response, wgt, grouping_vars = NULL, verbose = FALSE) {
  # Validate input data
  required_cols <- c(depvar, response, wgt)
  if (!is.null(grouping_vars)) {
    required_cols <- c(required_cols, grouping_vars)
  }
  validate_df_cols(data, required_cols)

  # Preparing data for survey design
  survey_design <- data %>%
    filter(!is.na(.[[depvar]]), !is.na(.[[response]])) %>%
    as_survey_design(ids = 1, weights = .data[[wgt]])

  # Grouping and summarizing data
  group_by_cols <- if (!is.null(grouping_vars)) c(grouping_vars, depvar, response) else c(depvar, response)
  collapsed_data <- survey_design %>%
    group_by(across(all_of(group_by_cols))) %>%
    summarize(prop = 100 * survey_mean(na.rm = TRUE)) %>%
    filter(response %in% c(100, 1)) %>%
    mutate(proplabel = paste0(round(prop, 1), "%")) %>%
    ungroup() %>%
    as.data.frame()

  # Verbose output
  if (verbose) {
    message("Grouping by: ", paste(group_by_cols, collapse = ", "))
    message("Calculated proportions for ", nrow(collapsed_data), " groups.")
  }

  return(collapsed_data)
}
