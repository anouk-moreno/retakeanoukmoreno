test_that("generate_density_plot returns ggplot", {
  prep <- prep_data("hurricane")
  df <- prep[[1]]; outcome <- prep[[2]]; label <- prep[[3]]
  p <- generate_density_plot(df, outcome, label)
  expect_true(ggplot2::is.ggplot(p))
})

test_that("generate_heatmap_strips returns patchwork/ggplot", {
  prep <- prep_data("hurricane")
  df <- prep[[1]]; outcome <- prep[[2]]; strip_vars <- prep[[4]]; variable_labels <- prep[[5]]
  hm <- generate_heatmap_strips(df, outcome, strip_vars, variable_labels)
  ok <- ggplot2::is.ggplot(hm) || inherits(hm, "patchwork")
  expect_true(ok)
})

test_that("plot_multiverse returns a combined plot", {
  p <- plot_multiverse("hurricane")
  ok <- ggplot2::is.ggplot(p) || inherits(p, "patchwork")
  expect_true(ok)
})
