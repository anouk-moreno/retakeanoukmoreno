#' Generate heatmap strips
#'
#' Builds heatmap strips for each decision variable across outcome bins.
#'
#' @param data Data frame with outcome + decision variables.
#' @param outcome_var String. Name of the outcome variable.
#' @param strip_vars Character vector with names of decision variables.
#' @param variable_labels Named vector/list with display labels for decision variables.
#'
#' @return A ggplot/patchwork object with the heatmap strips.
#' @examples
#' # prep <- prep_data("hurricane")
#' # df <- prep[[1]]
#' # strip_vars <- prep[[4]]
#' # variable_labels <- prep[[5]]
#' # hm <- generate_heatmap_strips(df, "edif", strip_vars, variable_labels)
#' @importFrom ggplot2 ggplot aes geom_rect geom_text scale_fill_gradient
#' @importFrom ggplot2 scale_x_continuous scale_y_discrete coord_cartesian
#' @importFrom ggplot2 theme_minimal theme element_blank element_text margin
#' @importFrom dplyr mutate count group_by ungroup filter arrange pull select bind_rows left_join
#' @importFrom dplyr distinct rename summarise desc
#' @importFrom tidyr complete
#' @importFrom tibble enframe
#' @importFrom rlang sym .data
#' @importFrom viridis viridis
#' @importFrom patchwork wrap_plots
#' @importFrom stats lm coef as.formula
#' @importFrom ggtext element_markdown
#' @export
#'
generate_heatmap_strips <- function(data, outcome_var, strip_vars, variable_labels) {
  n_bins <- 20
  x_min <- min(data[[outcome_var]], na.rm = TRUE)
  x_max <- max(data[[outcome_var]], na.rm = TRUE)
  x_buffer <- 0.1 * (x_max - x_min)
  common_xlim <- c(x_min, x_max + x_buffer)
  breaks <- seq(x_min, x_max, length.out = n_bins + 1)

  data <- data %>%
    mutate(outcome_bin = cut(.data[[outcome_var]], breaks = breaks, include.lowest = TRUE))

  reg_formula <- as.formula(paste(outcome_var, "~", paste(strip_vars, collapse = " + ")))
  lm_model <- lm(reg_formula, data = data)
  coefs <- coef(lm_model)

  tidy_coefs <- tibble::enframe(coefs, name = "term", value = "estimate") %>%
    filter(term != "(Intercept)") %>%
    rowwise() %>%
    mutate(
      var   = strip_vars[which.max(sapply(strip_vars, function(v) startsWith(term, v)))],
      level = trimws(sub(paste0("^", var), "", term))
    ) %>%
    ungroup()

  # Reorders levels FOR EACH variable in strip_vars (instead of just 1 variable)!!
  for (var in strip_vars) {
    ref_level <- levels(data[[var]])[1]
    coef_sub <- tidy_coefs %>%
      filter(var == !!var & level != ref_level) %>%
      arrange(desc(abs(estimate))) %>%
      pull(level)
    data[[var]] <- factor(data[[var]], levels = unique(c(ref_level, coef_sub)))
  }

  # Lookup table of labels FOR EVERY variable + adds "Ref." (for reference levels)
  label_lookup <- tidy_coefs %>%
    mutate(
      rounded    = round(estimate, 2),
      label_text = case_when(
        rounded > 0 ~ paste0("'+", formatC(rounded, format = "f", digits = 2), "'"),
        rounded < 0 ~ paste0("'",  formatC(rounded, format = "f", digits = 2), "'"),
        TRUE        ~ "'0.00'"
      )
    ) %>%
    select(var, level, label_text)

  ref_labels <- lapply(strip_vars, function(var) {
    base_level <- levels(data[[var]])[1]
    tibble(var = var, level = base_level, label_text = "Ref.")
  }) %>% bind_rows()

  label_lookup <- bind_rows(label_lookup, ref_labels)

  # Reorder ALL variables by influence in the model
  influence_scores <- tidy_coefs %>%
    group_by(var) %>%
    summarise(max_abs_effect = max(abs(estimate), na.rm = TRUE)) %>%
    arrange(desc(max_abs_effect))

  strip_vars <- influence_scores$var

  add_variable_label_row <- function(df_in, varname) {
    orig_levels <- levels(df_in[[varname]])
    new_levels  <- rev(c("label", orig_levels))
    df_in[[varname]] <- factor(df_in[[varname]], levels = new_levels)
    label_row <- data.frame(outcome_bin = NA, prop = NA, xmin = x_min, xmax = x_max, level = 0)
    label_row[[varname]] <- factor("label", levels = new_levels)
    for (col in setdiff(names(df_in), names(label_row))) label_row[[col]] <- NA
    bind_rows(label_row[, names(df_in)], df_in)
  }

  generate_heatmap_strip <- function(df_in, varname, outcome_var, breaks, base_color, var_label) {
    var_sym <- sym(varname)
    out_sym <- sym(outcome_var)
    x_label_pos <- x_max + 0.01 * (x_max - x_min)

    df_in <- df_in %>% mutate(!!var_sym := df_in[[varname]])
    original_levels <- levels(df_in[[varname]])
    first_level <- original_levels[1]

    heatmap_data <- df_in %>%
      mutate(outcome_bin = cut(!!out_sym, breaks = breaks, include.lowest = TRUE)) %>%
      count(outcome_bin, !!var_sym) %>%
      group_by(outcome_bin) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup() %>%
      complete(outcome_bin, !!var_sym, fill = list(prop = 0)) %>%
      mutate(
        bin_index = as.integer(outcome_bin),
        xmin = breaks[bin_index],
        xmax = breaks[bin_index + 1],
        !!var_sym := factor(!!var_sym, levels = levels(df_in[[varname]]))
      )

    heatmap_data <- add_variable_label_row(heatmap_data, varname)

    label_data <- heatmap_data %>%
      filter(!is.na(!!var_sym), !is.na(prop)) %>%
      distinct(!!var_sym) %>%
      mutate(var = varname) %>%
      rename(level_name = !!var_sym) %>%
      left_join(label_lookup, by = c("var", "level_name" = "level")) %>%
      mutate(
        label_text = ifelse(label_text == "Ref.", "italic('Ref.')", label_text),
        !!var_sym := factor(level_name, levels = levels(heatmap_data[[varname]]))
      )

    ggplot(heatmap_data, aes(y = !!var_sym, fill = prop)) +
      geom_rect(aes(xmin = xmin, xmax = xmax,
                    ymin = as.numeric(!!var_sym) - 0.5,
                    ymax = as.numeric(!!var_sym) + 0.5), color = NA) +
      geom_text(data = label_data, aes(y = !!var_sym, label = label_text),
                x = x_label_pos, inherit.aes = FALSE, hjust = 0, size = 3, parse = TRUE) +
      scale_fill_gradient(low = "white", high = base_color, na.value = NA) +
      scale_x_continuous(limits = c(x_min, x_max), expand = c(0, 0)) +
      scale_y_discrete(
        labels = function(labs) {
          labs <- ifelse(labs == "label", paste0("**", var_label, "**"),
                         ifelse(labs == first_level, paste0("*", labs, "*"), labs))
          labs
        },
        expand = c(0, 0)
      ) +
      coord_cartesian(xlim = common_xlim) +
      theme_minimal(base_size = 10) +
      theme(
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_markdown(),
        panel.grid = element_blank(),
        plot.margin = margin(0, 20, 0, 10)
      )
  }

  # Generates heatmap FOR EACH variable in strip_vars + combines them into single plot
  base_colors <- viridis(length(strip_vars), option = "D")
  heatmap_strips <- Map(function(varname, base_color) {
    var_label <- variable_labels[[varname]]
    generate_heatmap_strip(data, varname, outcome_var, breaks, base_color, var_label)
  }, varname = strip_vars, base_color = base_colors)

  wrap_plots(heatmap_strips, ncol = 1)
}
