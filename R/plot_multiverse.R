#' Plot multiverse
#'
#' Prepares data for a given use case, builds density plot and heatmap strips + stacks both.
#'
#' @param use_case String. Use case name.
#' @param output_path Optional string. File path to save combined plot.
#'
#' @return A ggplot/patchwork object with combined multiverse plot.
#' @examples
#' # p <- plot_multiverse("hurricane")
#' # p
#' @importFrom ggplot2 theme element_text ggsave is.ggplot
#' @importFrom patchwork plot_layout plot_annotation wrap_plots
#' @export
plot_multiverse <- function(use_case = "hurricane", output_path = NULL) {
  # Prepare data
  prep <- prep_data(use_case = use_case)
  df <- prep[[1]]
  outcome_var <- prep[[2]]
  outcome_var_label <- if (length(prep) >= 3) prep[[3]] else "Excess fatalities"
  strip_vars <- prep[[4]]
  variable_labels <- prep[[5]]

  # Density plot (legend was automatically moving so I put it on the top-right inside the panel)
  density_plot <- generate_density_plot(df, outcome_var, outcome_var_label) + theme(legend.position.inside = c(0.92, 0.92),
                                                                                    legend.justification = c("right","top"))

  # Heatmap strips (the function can return ggplot/list of ggplots so I also put else if for all cases)
  hm_out <- generate_heatmap_strips(
    data = df, outcome_var = outcome_var,
    strip_vars = strip_vars, variable_labels = variable_labels)

  if (ggplot2::is.ggplot(hm_out) || inherits(hm_out, "patchwork")) {
    heatmap_plot <- hm_out
  } else if (is.list(hm_out) && !is.null(hm_out$plot)) {
    heatmap_plot <- hm_out$plot
  } else if (is.list(hm_out)) {
    heatmap_plot <- patchwork::wrap_plots(hm_out, ncol = 1)
  } else {
    stop("Error: Function must return a ggplot / a list of ggplots.")}

  # Layout options for the combined plot (I changed heights_vector so the heatmap gets more vertical space, otherwise it looks bad, I hope it is ok this way)
  density_ratio <- 0.5
  strip_levels <- sapply(strip_vars, function(var) nlevels(factor(df[[var]])))
  total_strip_levels <- sum(strip_levels)
  # heights_vector <- c(density_ratio * total_strip_levels, strip_levels + 1)
  heights_vector <- c(1,2)

  # Plots combined + added title
  combined_plot <- density_plot / heatmap_plot +
    patchwork::plot_layout(heights = heights_vector) +
    patchwork::plot_annotation(title = "Task 2.4: Multiverse analysis of hurricane fatalities (All vs. Significant)") &
    theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5))

  # Optional save with specified sizes
  if (!is.null(output_path)) {
    ggsave(output_path, combined_plot, width = 11.50, height = 6.12)
  }

  combined_plot
}
