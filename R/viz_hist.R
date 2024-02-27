#' Generate histogram with customizable appearance
#'
#' This function generates a histogram plot using ggplot2 with customizable appearance settings.
#'
#' @param data A data frame containing the variables.
#' @param outcome_var The title of the outcome variable column in the data frame.
#' @param label_var The title of the label variable column in the data frame.
#' @param cat_var The title of the categorical variable column in the data frame.
#' @param ymin The minimum value for the y-axis.
#' @param ymax The maximum value for the y-axis.
#' @param lang Language setting for text elements (default: "en").
#' @param main_title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param source_info Information about the data source.
#' @param order Logical value indicating whether to order the data by the outcome variable (default: FALSE).
#' @param color_scheme Color scheme for the plot (default: "#FDB71A").
#' @return A ggplot object representing the histogram.
#' @export
viz_hist <- function(data, outcome_var = "prop", label_var = "proplabel",
                     cat_var = "cat",
                     ymin = 0,
                     ymax = 100,
                     lang = "en",
                     main_title = "",
                     subtitle = "",
                     source_info = "",
                     order = FALSE,
                     color_scheme = "#FDB71A") {

  if(order) {
    data <- data[order(-data[[outcome_var]]), ]
    cat_var <- cat_var[order(-data[[outcome_var]])]
    label_var <- label_var[order(-data[[outcome_var]])]
    outcome_var <- outcome_var[order(-data[[outcome_var]])]
  }

  update_geom_defaults("text", list(family = "inter"))

  ggplot(data, aes_string(x = factor(data[[cat_var]], levels = data[[cat_var]]), y = data[[outcome_var]])) +
    geom_bar(stat = "identity", color = color_scheme, fill = paste0(color_scheme, "90"), width = 0.75) +
    geom_text(aes_string(label = data[[label_var]]), vjust = -0.5, size = 5.5, fontface = "bold", color = color_scheme) +
    scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0.3), labels = function(x) paste0(x, "%")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
    labs(title = main_title,
         y = "",
         x = "",
         caption = source_info,
         subtitle = subtitle) +
    theme(text = element_text(size = 16, family = "inter"),
          plot.title = element_text(size = 20, family = "inter", face = "bold"),
          plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "inter", color = "#585860"),
          plot.subtitle = element_text(size = 15, family = "inter-light", color = "#242424"),
          axis.title.y = element_blank(),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          axis.ticks = element_blank(),
          axis.text = element_text(size = 16, family = "inter-light", color = "black"),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.x = element_blank())
}
