# INPUT HANDLING
test_that("inspect_balance handles inputs as intended", {
  # Generate data for use in testing
  df_no_categorical <- data.frame(a = 1:5, b = 6:10)
  df_empty <- data.frame()

  # Test input handling
  expect_error(inspect_balance(matrix(1:10, nrow = 5, ncol = 2)),
               "Input is not a data frame or a tibble.")
  expect_error(inspect_balance(df_empty),
               "The dataset is empty. No plots will be generated.")
  expect_error(inspect_balance(df_no_categorical),
               "No categorical columns found in the data. Try using inspect_normality instead.")
  expect_error(inspect_balance(iris, vars = c("Sepal.Length", "NonExistentVar")),
               "Some of the specified variables do not exist in the dataset.")
  expect_error(inspect_balance(iris, vars = c("Sepal.Length", "Species")),
               "The following variables are not categorical: Sepal.Length")
  expect_warning(inspect_balance(iris, vars = rep("Species", 21)),
                 "The dataset has more than 20 selected variables. The plot might be crowded.")
})

# ROUTINE USAGE
test_that("basic usage of inspect_balance produces a ggplot object with correct layers", {
  # Generate dataset for use in testing
  iris_cat3 <- iris |>
    mutate(
      SepalWidthCategory = case_when(
        Sepal.Width <= 3.0 ~ "Narrow",
        Sepal.Width > 3.0 & Sepal.Width <= 3.5 ~ "Medium",
        Sepal.Width > 3.5 ~ "Wide"
      ),
      SepalLengthCategory = case_when(
        Sepal.Length <= 5.0 ~ "Short",
        Sepal.Length > 5.0 & Sepal.Length <= 6.5 ~ "Medium",
        Sepal.Length > 6.5 ~ "Long"
      ))

  # Generate plot and objects for use in testing (basic usage)
  test_plot <- inspect_balance(iris_cat3)
  num_facets <- test_plot$data |>
    distinct(variable) |>
    nrow() # Count number of unique variables in the data
  barplot_layer <- test_plot$layers[[1]]

  # Test ggplot object structure (basic usage)
  expect_s3_class(test_plot, "ggplot")
  expect_s3_class(barplot_layer$geom, "GeomBar")
  expect_equal(num_facets, 3) # One facet for each numeric var in the data
})

test_that("advanced usage of inspect_balance produces a ggplot object with correct layers", {
  # Generate dataset for use in testing
  iris_cat3 <- iris |>
    mutate(
      SepalWidthCategory = case_when(
        Sepal.Width <= 3.0 ~ "Narrow",
        Sepal.Width > 3.0 & Sepal.Width <= 3.5 ~ "Medium",
        Sepal.Width > 3.5 ~ "Wide"
      ),
      SepalLengthCategory = case_when(
        Sepal.Length <= 5.0 ~ "Short",
        Sepal.Length > 5.0 & Sepal.Length <= 6.5 ~ "Medium",
        Sepal.Length > 6.5 ~ "Long"
      ))

  # Generate plot and objects for use in testing
  test_plot <- inspect_balance(iris_cat3, vars = c("Species", "SepalWidthCategory"), alpha = 0.5)
  num_facets <- test_plot$data |>
    distinct(variable) |>
    nrow() # Count number of unique variables in the data
  barplot_layer <- test_plot$layers[[1]]

  # Test ggplot object structure
  expect_s3_class(test_plot, "ggplot")
  expect_s3_class(barplot_layer$geom, "GeomBar")
  expect_equal(num_facets, 2) # One facet per variable specified
  expect_equal(barplot_layer$aes_params$alpha, 0.5)
})
