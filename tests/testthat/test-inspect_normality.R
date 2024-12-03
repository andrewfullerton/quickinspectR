library(testthat)
library(dplyr)

# TEST INPUT ERROR AND WARNING HANDLING
test_that("inspect_normality issues errors and warnings as intended", {
  # Generate data for use in testing
  empty_df <- data.frame()
  non_numeric_df <- iris |> select(Species)
  large_df <- iris |> bind_cols(as.data.frame(matrix(rnorm(150 * 20), ncol = 20)))

  # Test basic input errors and warnings
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

  # Test colour and title errors and warnings
  expect_error(
    inspect_normality(mtcars, fill_colour = "invalid_color"),
    "Invalid color: invalid_color\\. Please provide a valid colour name or hex code\\."
  )
  expect_error(
    inspect_normality(mtcars, fill_colour = "#GGGGGG"),
    "Invalid color: #GGGGGG\\. Please provide a valid colour name or hex code\\."
  )
  expect_error(
    inspect_normality(mtcars, title = 123),
    "Invalid title: 123\\. Please provide a non-empty character string\\."
  )
  expect_error(
    inspect_normality(mtcars, title = ""),
    "Invalid title: \\. Please provide a non-empty character string\\."
  )
  expect_error(
    inspect_normality(mtcars, title = c("Title1", "Title2")),
    "Invalid title: Title1Title2\\. Please provide a non-empty character string\\."
  )
})

# TEST ROUTINE USAGE
test_that("basic usage of inspect_normality returns a ggplot object with correct layers and facets", {
  # Generate plot and objects for use in testing
  test_plot <- inspect_normality(iris)
  num_facets <- test_plot$data |> distinct(variable) |> nrow()
  histogram_layer <- test_plot$layers[[1]]

  # Test ggplot object structure
  expect_s3_class(test_plot, "ggplot")
  expect_s3_class(histogram_layer$geom, "GeomBar")
  expect_equal(num_facets, 4)
  expect_equal(histogram_layer$stat_params$bins, 15)
})

test_that("advanced usage of inspect_normality returns a ggplot object with correct layers and parameters", {
  # Generate plot for testing
  test_plot <- inspect_normality(
    iris,
    vars = c("Sepal.Length", "Sepal.Width"),
    bins = 10,
    fill_colour = "blue",
    title = "Custom Title"
  )

  # Generate objects for testing
  num_facets <- test_plot$data |> distinct(variable) |> nrow()
  histogram_layer <- test_plot$layers[[1]]
  plot_build <- ggplot2::ggplot_build(test_plot)
  fill_color <- plot_build$data[[1]]$fill

  # Test basic ggplot object structure
  expect_s3_class(test_plot, "ggplot")
  expect_s3_class(histogram_layer$geom, "GeomBar")
  expect_equal(num_facets, 2)
  expect_equal(histogram_layer$stat_params$bins, 10)

  # Test if custom fill colour is correctly applied
  expect_equal(histogram_layer$mapping$fill, NULL)
  expect_equal(fill_color[1], "blue")

  # Check if custom title is set correctly
  expect_equal(test_plot$labels$title, "Custom Title")
})


