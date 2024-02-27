#' Generate a grouped dot-and-whisker plot with customizable appearance
#'
#' This function generates a grouped dot-and-whisker plot using ggplot2 with customizable appearance settings.
#'
#' @param data A data frame containing the variables.
#' @param depvar The title of the dependent variable column in the data frame.
#' @param outcome_var The title of the outcome variable column in the data frame.
#' @param lower_bound The lower bound of the confidence interval.
#' @param upper_bound The upper bound of the confidence interval.
#' @param label_var The title of the label variable column in the data frame.
#' @param groupingvar The title of the grouping variable column in the data frame.
#' @param ymin The minimum value for the y-axis.
#' @param ymax The maximum value for the y-axis.
#' @param lang Language setting for text elements (default: "en").
#' @param main_title Title of the plot.
#' @param source_info Information about the data source.
#' @param subtitle Subtitle of the plot.
#' @param sort The sorting order of the data (options: "var1", "var2", "var3", "alpha", default: "").
#' @param y_label Label for the y-axis.
#' @param x_label Label for the x-axis.
#' @param color_scheme Color scheme for the plot (default: c("#D67619", "#264d5e", "#006848", "#4B2884")).
#' @param label_size Size of the labels.
#' @param text_position Position of the text labels.
#' @import dplyr
#' @import rlang
#' @import ggplot2
#' @return A ggplot object representing the grouped dot-and-whisker plot.
#' @export
viz_dv_group <- function(data,
                        depvar = "depvar",
                        outcome_var = "prop",
                        lower_bound = "prop_low",
                        upper_bound = "prop_upp",
                        label_var = "proplabel",
                        groupingvar = "groupingvar",
                        ymin = 0,
                        ymax = 100,
                        lang = "en",
                        main_title = "",
                        source_info = "",
                        subtitle = "",
                        sort = "",
                        y_label = "",
                        x_label = "",
                        color_scheme = c("#D67619", "#264d5e", "#006848", "#4B2884"),
                        label_size = 4.25,
                        text_position = 0.75) {

  fill_colors <- paste0(color_scheme, "")

  if (sort == "var1") {
    data <- data %>%
      group_by({{groupingvar}}) %>%
      mutate(rank = rank(-{{outcome_var}})) %>%
      arrange({{groupingvar}}, rank)
  } else if (sort == "var2") {
    data <- data %>%
      group_by({{groupingvar}}) %>%
      mutate(rank = rank(-{{outcome_var}})) %>%
      arrange(match({{groupingvar}}, unique({{groupingvar}})[2]), rank)
  } else if (sort == "var3") {
    data <- data %>%
      group_by({{groupingvar}}) %>%
      mutate(rank = rank(-{{outcome_var}})) %>%
      arrange(match({{groupingvar}}, unique({{groupingvar}})[3]), rank)
  } else if (sort == "alpha") {
    data <- data[order(data$depvar), ]
  }

  update_geom_defaults("text", list(family = "inter"))

  ggplot(data = data, aes(x = factor({{depvar}}, levels = unique({{depvar}})), y = {{outcome_var}}, fill = {{groupingvar}}, color = {{groupingvar}})) +
    geom_bar(position = "dodge", stat = "identity", width = 0.75) +
    geom_text(aes(label = {{label_var}}, y = {{outcome_var}}, group = {{groupingvar}}),
              position = position_dodge(width = text_position),
              vjust = -.5, size = label_size, fontface = "bold",
              show.legend = FALSE) +
    scale_fill_manual(values = fill_colors) +
    scale_color_manual(values = color_scheme) +
    scale_y_continuous(limits = c(ymin, ymax), expand = expansion(mult = 0.002)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 9)) +
    labs(title = main_title,
         y = y_label,
         x = x_label,
         caption = paste0(ifelse(lang == "es", "Fuente: ", "Source: "),
                          source_info)) +
    {if (subtitle != "") labs(subtitle = subtitle)}+
    theme(text = element_text(size = 16, family = "inter"),
          plot.title = element_text(size = 20, family = "inter", face = "bold"),
          plot.caption = element_text(size = 10.5, vjust = 2, hjust = 0.02, family = "inter", color = "#585860"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line.x = element_line(linewidth = 0.6, linetype = "solid", colour = "#dddddf"),
          axis.text = element_text(size = 16, family = "inter-light", color = "black"),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "top",
          plot.title.position = "plot",
          plot.caption.position = "plot",
          legend.title = element_blank(),
          legend.justification = 'left',
          legend.margin = margin(t = 0, b = 0),
          legend.text = element_markdown(family = "inter-light", size = 15))
}
