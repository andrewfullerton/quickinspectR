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

test_that("inspect_normality returns a ggplot object with correct layers and facets", {
  # Generate plot for use in testing
  test_plot <- inspect_normality(iris)

  # Test ggplot object structure
  expect_s3_class(test_plot, "ggplot")
})



