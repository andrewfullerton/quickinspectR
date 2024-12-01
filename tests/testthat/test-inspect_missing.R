test_that("inspect_missing handles inputs as intended", {
  # Generate data for testing
  df <- iris
  df[1:10, "Sepal.Length"] <- NA
  df_empty <- data.frame()

  # Test input handling
  expect_error(inspect_missing(123), "Input is not a data frame or a tibble.")
  expect_error(inspect_missing(df_empty), "The dataset is empty. No plots will be generated.")
  expect_error(inspect_missing(df, vars = c("Sepal.Length", "nonexistent_var")),
               "The following variables do not exist in the dataset: nonexistent_var")
})

test_that("inspect_missing produces ggplot object as intended", {
  # Generate data for testing
  df <- iris
  df[1:10, "Sepal.Length"] <- NA

  # Generate plot objects for testing (basic usage)
  test_plot <- inspect_missing(df)
  barplot_layer <- test_plot$layers[[1]]

  # Generate plot objects for testing (advanced usage)
  test_plot_manspec <- inspect_missing(df, vars = c("Sepal.Length"), na_colour = "blue", fill_colour = "green")
  barplot_layer_manspec <- test_plot_manspec$layers[[1]]

  # Basic usage
  expect_s3_class(test_plot_manspec, "ggplot")
  expect_s3_class(barplot_layer$geom, "GeomBar")

  # Advanced usage
  expect_s3_class(test_plot_manspec, "ggplot")
  expect_s3_class(barplot_layer_manspec$geom, "GeomBar")
})

