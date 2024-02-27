#'
#' This function creates a stacked bar plot
#'
#' @param data A data frame containing the agreement data.
#' @param outcome_var The outcome variable representing the proportion of agreement.
#' @param prop_labels Labels for the proportions.
#' @param var_labels Labels for the variables.
#' @param value_labels Labels for the values.
#' @param lang The language used for subtitles.
#' @param main_title The main title of the plot.
#' @param subtitle The subtitle of the plot.
#' @param source_info Information about the data source.
#' @param rev_values Logical indicating whether to reverse the order of value labels.
#' @param rev_variables Logical indicating whether to reverse the order of variable labels.
#' @param hide_small_values Logical indicating whether to hide small values in the plot.
#' @param order_bars Logical indicating whether to order the bars in the plot.
#' @param subtitle_h_just Horizontal justification of the subtitle.
#' @param fixed_aspect_ratio Logical indicating whether to maintain a fixed aspect ratio for the plot.
#' @param color_scheme A vector of colors for the plot.
#' @param label_size The size of the labels.
#' @param text_position The position of the text labels.
#' @import ggrepel
#' @return A ggplot object representing the agreement visualization.
#' @export
viz_agree <- function(data,
                      outcome_var = data$prop,
                      prop_labels = data$proplabel,
                      var_labels = data$varlabel,
                      value_labels = data$vallabel,
                      lang = "en",
                      main_title = "",
                      subtitle = "",
                      source_info = "",
                      rev_values = FALSE,
                      rev_variables = FALSE,
                      hide_small_values = TRUE,
                      order_bars = FALSE,
                      subtitle_h_just = 0,
                      fixed_aspect_ratio = TRUE,
                      color_scheme = c("#FF0000", "#FF6666",  "#FFCC33", "#338585", "#006666"),
                      label_size = 4.25,
                      text_position = 0.75) {

  # Convert labels to character if necessary
  if (!inherits(var_labels, "character") && !inherits(var_labels, "factor")) {
    var_labels <- as.character(var_labels)
    data$varlabels <- as.character(data$varlabel)
  }

  if (!inherits(value_labels, "character") && !inherits(value_labels, "factor")) {
    value_labels <- as.character(value_labels)
    data$vallabel <- as.character(data$vallabel)
  }

  # Reverse colors if specified
  mycolors <- if (rev_values) rev(color_scheme[seq_along(unique(value_labels))]) else color_scheme

  # Reverse order of value labels if specified
  value_labels <- ifelse(rev_values, factor(value_labels, levels = unique(value_labels)),
                         factor(value_labels, levels = rev(unique(value_labels))))

  # Prepare positions
  positions <- rev(unique(var_labels))

  # Order bars if specified
  if (order_bars) {
    var_labels <- factor(var_labels, levels = unique(var_labels))
    data <- data.frame(var_labels, value_labels, outcome_var, prop_labels)
    p <- ggplot(data, aes(y = outcome_var, x = var_labels,
                          fill = reorder(value_labels, outcome_var), label = prop_labels)) +
      geom_bar(position = "stack", stat = "identity", width = 0.6, data = data[data$var_labels == levels(data$var_labels)[1], ]) +
      geom_bar(position = "stack", stat = "identity", width = 0.6, data = data[data$var_labels == levels(data$var_labels)[2], ]) +
      geom_bar(position = "stack", stat = "identity", width = 0.6, data = data[data$var_labels == levels(data$var_labels)[3], ]) +
      geom_text(data = data[data$var_labels == levels(data$var_labels)[1], ],
                aes(label = prop_labels),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5)
    for (i in 2:length(unique(var_labels))) {
      p <- p + geom_text(data = data[data$var_labels == levels(data$var_labels)[i], ],
                         aes(label = ifelse(outcome_var >= 5, prop_labels, NA)),
                         position = position_stack(vjust = 0.5), color = "#FFFFFF",
                         fontface = "bold", size = 5)
    }
    p <- p + coord_flip() +
      scale_fill_manual(values = mycolors, guide = guide_legend(reverse = TRUE, nrow = 1)) +
      scale_x_discrete(limits = positions, expand = c(0, 0)) +
      scale_y_continuous(expand = c(0.02, 0)) +
      labs(title = main_title,
           y = "",
           x = " ",
           caption = source_info,
           subtitle = subtitle) +
      theme(text = element_text(size = 14, family = "inter"),
            plot.title = element_text(size = 20, family = "inter", face = "bold"),
            plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "inter", color = "#585860"),
            plot.subtitle = element_text(size = 20, family = "inter", face = "bold"),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(margin = margin(r = 0)),
            axis.ticks = element_blank(),
            axis.text = element_text(size = 14, family = "inter", color = "#585860", margin = margin(r = 5)),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            legend.position = "top",
            plot.title.position = "plot",
            plot.caption.position = "plot",
            legend.text = element_text(family = "inter", color = "#585860"),
            legend.title = element_blank(),
            legend.justification = 'left',
            legend.key.size = unit(1, "line"),
            legend.margin = margin(t = 5, b = 5, 0, subtitle_h_just))
    if (fixed_aspect_ratio) {
      p <- p + theme(aspect.ratio = 0.35)
    }
    return(p)
  } else {
    p <- ggplot(data, aes(fill = value_labels, y = outcome_var, x = var_labels, label = prop_labels)) +
      geom_bar(position = "stack", stat = "identity", width = 0.6) +
      geom_text(label = ifelse(outcome_var >= 5, prop_labels, NA),
                position = position_stack(vjust = 0.5), color = "#FFFFFF",
                fontface = "bold", size = 5) +
      coord_flip() +
      scale_fill_manual(values = mycolors, guide = guide_legend(reverse = TRUE, nrow = 1)) +
      scale_x_discrete(limits = positions, expand = c(0, 0)) +
      scale_y_continuous(expand = c(0.02, 0)) +
      labs(title = main_title,
           y = "",
           x = " ",
           caption = source_info,
           subtitle = subtitle) +
      theme(text = element_text(size = 14, family = "inter"),
            plot.title = element_text(size = 20, family = "inter", face = "bold"),
            plot.caption = element_text(size = 10.5, hjust = 0.02, vjust = 2, family = "inter-light", color = "#585860"),
            plot.subtitle = element_text(size = 20, family = "inter", color = "black", face = "bold"),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(margin = margin(r = 0)),
            axis.ticks = element_blank(),
            axis.text = element_text(size = 14, family = "inter", color = "#585860", margin = margin(r = 5)),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            legend.position = "top",
            plot.title.position = "plot",
            plot.caption.position = "plot",
            legend.text = element_text(family = "inter", color = "#585860"),
            legend.title = element_blank(),
            legend.justification = 'left',
            legend.key.size = unit(1, "line"),
            legend.margin = margin(t = 5, b = 5, 0, subtitle_h_just))
    if (fixed_aspect_ratio) {
      p <- p + theme(aspect.ratio = 0.35)
    }
    return(p)
  }
}
