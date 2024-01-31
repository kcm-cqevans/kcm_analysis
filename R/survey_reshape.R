#' Reshape Survey Data
#'
#' This function reshapes a survey dataframe from wide to long format.
#'
#' @param data A dataframe containing survey data.
#' @param pivot_cols Vector of string column names to pivot into longer format.
#' @import dplyr
#' @import tidyr
#' @export
survey_reshape <- function(data, pivot_cols) {
  validate_df_cols(data, pivot_cols)

  # Reshape the data
  data %>%
    pivot_longer(cols = all_of(pivot_cols), names_to = "depvar", values_to = "response") %>%
    as.data.frame()
}
