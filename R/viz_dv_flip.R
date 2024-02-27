#' Generate a horizontal dot-and-whisker plot with customizable appearance
#'
#' This function generates a horizontal dot-and-whisker plot using ggplot2 with customizable appearance settings.
#'
#' @param data A data frame containing the variables.
#' @param outcome_var The title of the outcome variable column in the data frame.
#' @param lower_bound The lower bound of the confidence interval.
#' @param vallabel The title of the categorical variable column in the data frame.
#' @param upper_bound The upper bound of the confidence interval.
#' @param label_var The title of the label variable column in the data frame.
#' @param ymin The minimum value for the y-axis.
#' @param ymax The maximum value for the y-axis.
#' @param lang Language setting for text elements (default: "en").
#' @param highlight A value to highlight in the plot.
#' @param main_title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param source_info Information about the data source.
#' @param sort The sorting order of the data (options: "hi-lo", "lo-hi", "alpha", default: "").
#' @param color_scheme Color scheme for the plot (default: "#784885").
#' @param label_size Size of the labels.
#' @import ggplot2
#' @return A ggplot object representing the horizontal dot-and-whisker plot.
#' @export
viz_dv_flip <- function(data, outcome_var = "prop", lower_bound = "prop_low", vallabel = "cat",
                        upper_bound = "prop_upp", label_var = "proplabel",
                        ymin = 0,
                        ymax = 100,
                        lang = "en",
                        highlight = "",
                        main_title = "",
                        source_info = "",
                        subtitle = "",
                        sort = "",
                        color_scheme = "#784885",
                        label_size = 5) {

  if (highlight != "") {
    data$hl_var <- factor(ifelse(data[[vallabel]] == highlight, 0, 1), labels = c("hl", "other"))
    fill_values <- c(paste0(color_scheme, "90"), paste0(color_scheme, "90"))
  } else {
    data$hl_var <- factor("other")
    fill_values <- paste0(color_scheme, "90")
  }

  if (sort == "hi-lo") {
    data <- data[order(-data[[outcome_var]]), ]
  } else if (sort == "lo-hi") {
    data <- data[order(data[[outcome_var]]), ]
  } else if (sort == "alpha") {
    data <- data[order(data[[vallabel]]), ]
  }

  ci_text <- ifelse(lang == "es",
                    paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                           "<span style='color:#585860; font-size:13pt'>95% intervalo de confianza </span>"),
                    paste0(" <span style='color:", color_scheme, "; font-size:18pt'> \u0131\u2014\u0131</span> ",
                           "<span style='color:#585860; font-size:13pt'>95% confidence </span>",
                           "<span style='color:#585860'>interval</span>"))

  update_geom_defaults("text", list(family = "inter"))

  ggplot(data = data, aes_string(x = factor(data[[vallabel]], levels = data[[vallabel]]), y = data[[outcome_var]], fill = hl_var)) +
    geom_bar(stat = "identity", color = color_scheme, width = 0.6) +
    geom_text(aes_string(label = label_var, y = upper_bound), vjust = 0.5, hjust = -0.15,
              size = label_size, fontface = "bold", color = color_scheme) +
    geom_errorbar(aes_string(ymin = lower_bound, ymax = upper_bound), width = 0.15, color = color_scheme, linetype = "solid") +
    scale_fill_manual(breaks = "other",
                      values = fill_values,
                      labels = paste0(" <span style='color:#585860; font-size:13pt'> ",
                                      subtitle,
                                      "<span style='color:#FFFFFF00'>-----------</span>",
                                      ci_text),
                      na.value = paste0(color_scheme, "90")) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(title = main_title,
         y = "",
         x = "",
         caption = paste0(source_info)) +
    theme(text = element_text(size = 16, family = "inter"),
          plot.title = element_text(size = 20, family = "inter", face = "bold"),
          plot.caption = element_text(size = 10.5, vjust = 2, hjust = 0, color = "#585860"),
          panel.background = element_blank(),
          axis.text = element_text(size = 16, family = "inter-light", color = "black"),
          axis.ticks = element_blank(),
          legend.position = "top",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.title = element_blank(),
          legend.justification = 'left',
          legend.margin = margin(t = 0, b = 0),
          legend.text = element_markdown(family = "inter-light", size = 15)) + coord_flip()
}
