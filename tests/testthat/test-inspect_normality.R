test_that("inspect_normality issues errors and warning as intended", {
  # Generate data for use in testing
  empty_df <- data.frame()
  non_numeric_df <- iris |> select(Species)
  large_df <- iris |> bind_cols(as.data.frame(matrix(rnorm(150 * 20), ncol = 20)))

  # Test errors and warnings
  expect_error(inspect_normality(42),
               "Input must be a data frame or a tibble.")
  expect_error(inspect_normality("not_a_df"),
               "Input must be a data frame or a tibble.")
  expect_error(inspect_normality(empty_df),
               "The dataset is empty. No plots will be generated.")
  expect_error(inspect_normality(non_numeric_df),
               "No numeric columns found in the data.")
  expect_error(inspect_normality(iris, vars = c("NonExistentVar")),
               "Some of the specified variables do not exist")
  expect_error(inspect_normality(iris, vars = "Species"),
               "The following variables are not numeric: Species")
  expect_warning(inspect_normality(large_df),
                 "The dataset has more than 20 selected variables.")
})

test_that("basic usage of inspect_normality returns a ggplot object with correct layers and facets", {
  # Generate plot and objects for use in testing (basic usage)
  test_plot <- inspect_normality(iris)
  num_facets <- test_plot$data |> distinct(variable) |> nrow()
  histogram_layer <- test_plot$layers[[1]]

  # Generate plot and objects for use in testing (advanced usage)
  test_plot_manspec <- inspect_normality(iris, vars = c("Sepal.Length", "Sepal.Width"), bins = 10)
  num_facets_manspec <- test_plot_manspec$data |> distinct(variable) |> nrow()
  histogram_layer_manspec <- test_plot_manspec$layers[[1]]

  # Test ggplot object structure (basic usage)
  expect_s3_class(test_plot, "ggplot")
  expect_s3_class(histogram_layer$geom, "GeomBar")
  expect_equal(num_facets, 4)
  expect_equal(histogram_layer$stat_params$bins, 15)

  # Test ggplot object structure (advanced usage)
  expect_s3_class(test_plot_manspec, "ggplot")
  expect_s3_class(histogram_layer_manspec$geom, "GeomBar")
  expect_equal(num_facets_manspec, 2)
  expect_equal(histogram_layer_manspec$stat_params$bins, 10)
})



