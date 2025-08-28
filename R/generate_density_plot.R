#' Generate density plot
#'
#' Makes density plot of outcome variable, showing all values + significant ones.
#'
#' @param data Data frame that has outcome + column `significant`.
#' @param outcome_var String. Name of outcome variable.
#' @param outcome_var_label String. Label for x-axis.
#'
#' @return A ggplot object with density plot.
#' @examples
#' # prep <- prep_data("hurricane")
#' # df <- prep[[1]]
#' # generate_density_plot(df, "edif", "Excess fatalities")
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual labs coord_cartesian
#'   theme_minimal theme element_text element_blank margin
#' @importFrom stats density
#' @importFrom grid unit
#' @export
#'
# Generates upper density plot for chosen outcome_var
generate_density_plot <- function(data, outcome_var, outcome_var_label){
  x <- data[[outcome_var]] # Extracts outcome_var from ds based on col_name

  # Min and max of outcome_var to set x-axis range!!
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)

  # Density for all specifications + conversion into dataframe
  dens_all <- density(x, na.rm = TRUE, from = x_min, to = x_max)
  df_dens_all <- data.frame(x = dens_all$x, y = dens_all$y,
                            type = factor("All", levels = c("All", "Significant")))

  # Density for significant specifications over same range
  sig_flag <- data$significant
  dens_sig <- density(x[sig_flag], na.rm = TRUE, from = x_min, to = x_max)
  # Rescale by proportion + ensure it does not exceed total density
  sig_share <- mean(sig_flag, na.rm = TRUE)
  dens_sig$y <- pmin(dens_sig$y * sig_share, dens_all$y)
  # Conversion to dataframe
  df_dens_sig <- data.frame(x = dens_sig$x, y = dens_sig$y, type = factor("Significant", levels = c("All", "Significant")))

  # Combines 2 curves into dataframe for plotting (buffer to avoid curve being cut off)
  df_dens <- rbind(df_dens_all, df_dens_sig)
  x_buffer <- 0.1 * (x_max - x_min)
  common_xlim <- c(x_min, x_max + x_buffer)

  # Creates density plot with same style as my previous example (Task 2.3)
  density_plot <- ggplot(df_dens, aes (x = x, y = y, color = type)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = c("All" = "steelblue", "Significant" = "tomato")) +
    labs(
      x = outcome_var_label, color = NULL) +
    coord_cartesian(xlim = common_xlim) +
    theme_minimal(base_size = 11) +
    theme(
      legend.key.height = grid::unit(0.3, "cm"),
      legend.text = element_text(size = 9),
      legend.position.inside = c(0.9, 0.95),
      legend.justification = c("right", "top"),
      legend.direction = "vertical",
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 10, margin = margin(t = 10)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank())

  return(density_plot) # Returns ggplot to be displayed
}
