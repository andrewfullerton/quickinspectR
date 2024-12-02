# BASIC INPUT HANDLING TESTS
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

# ROUTINE USAGE TESTS
test_that("Basic usage of inspect_missing produces ggplot object as intended", {
  # Generate data for testing
  df <- iris
  df[1:10, "Sepal.Length"] <- NA

  # Generate plot objects for testing
  test_plot <- inspect_missing(df)
  barplot_layer <- test_plot$layers[[1]]

  # Basic usage
  expect_s3_class(test_plot, "ggplot")
  expect_s3_class(barplot_layer$geom, "GeomBar")
})

test_that("Advanced usage of inspect_missing produces ggplot object as intended", {
  # Generate data for testing
  df <- iris
  df[1:10, "Sepal.Length"] <- NA

  # Generate plot objects for testing
  test_plot <- inspect_missing(df, vars = c("Sepal.Length"), na_colour = "blue", fill_colour = "green", alpha = 0.5)
  barplot_layer <- test_plot$layers[[1]]
  test_plot_data <- ggplot2::ggplot_build(test_plot)$data[[1]]
  missing_fill_color <- unique(test_plot_data$fill[test_plot_data$fill == "blue"])
  non_missing_fill_color <- unique(test_plot_data$fill[test_plot_data$fill == "green"])
  missing_alpha <- unique(test_plot_data$alpha[test_plot_data$fill == "blue"])
  non_missing_alpha <- unique(test_plot_data$alpha[test_plot_data$fill == "green"])

  # Test plot output
  expect_s3_class(test_plot, "ggplot")
  expect_s3_class(barplot_layer$geom, "GeomBar")
  expect_equal(missing_fill_color, "blue")  # Missing values should be filled with blue
  expect_equal(non_missing_fill_color, "green")  # Non-missing values should be filled with green
  expect_equal(missing_alpha, 0.5)  # Missing values should have an alpha of 0.5
  expect_equal(non_missing_alpha, 0.5)  # Non-missing values should have an alpha of 0.5
})

# EDGE CASE TESTS
test_that("Redundant variable names are handled", {
  # Generate data and objects for testing
  df <- data.frame(a = c(NA, 1), b = c(2, NA))
  plot <- inspect_missing(df, vars = c("a", "a"))

  # Test plot output
  expect_s3_class(plot, "ggplot")
})

test_that("Plot works when there are no missing values", {
  # Generate data and objects for testing
  df <- data.frame(a = c(1, 2), b = c(3, 4))
  test_plot <- inspect_missing(df)
  test_plot_data <- ggplot2::ggplot_build(test_plot)$data[[1]] # Inspect plot build
  non_missing_data <- test_plot_data |>
    dplyr::filter(fill == "darkgrey") # Assuming "darkgrey" represents non-missing
  total_non_missing <- sum(non_missing_data$y) # Sum the y values to get non-missing value count

  # Test plot output
  expect_s3_class(test_plot, "ggplot")
  expect_equal(total_non_missing, 2)
})

test_that("Plot works when all values in a variable are missing", {
  df <- data.frame(a = c(NA, NA), b = c(1, 2))
  test_plot <- inspect_missing(df)
  test_plot_data <- ggplot2::ggplot_build(test_plot)$data[[1]] # Inspect plot build
  missing_data <- test_plot_data |>
    dplyr::filter(fill == "red") # Assuming "red" represents missing
  total_missing <- sum(missing_data$y) # Sum the y values to get missing value count

  # Test plot output
  expect_s3_class(test_plot, "ggplot")
  expect_equal(total_missing, 2)
})

test_that("Plot handles NAs in mixed data types", {
  # Create a test data frame
  df <- data.frame(
    a = c(1, NA),
    b = c("text", NA),
    c = factor(c("A", NA))
  )

  # Generate objects for testing
  test_plot <- inspect_missing(df)
  test_plot_data <- ggplot2::ggplot_build(test_plot)$data[[1]] # Inspect plot build
  missing_data <- test_plot_data |>
    dplyr::filter(fill == "red") # Assuming "red" represents missing
  total_missing <- sum(missing_data$y) # Sum the y values to get missing value count

  # Test plot output
  expect_s3_class(test_plot, "ggplot")
  expect_equal(total_missing, 3) # Expect 3 because df has three NAs
})




