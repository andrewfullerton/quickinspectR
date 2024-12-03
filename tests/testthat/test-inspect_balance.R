# INPUT HANDLING
test_that("inspect_balance handles inputs as intended", {
  # Generate data for use in testing
  df_no_categorical <- data.frame(a = 1:5, b = 6:10)
  df_empty <- data.frame()

  # Test basic input errors and warnings
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

  # Test colour and title errors and warnings
  expect_error(
    inspect_balance(iris, fill_colour = "invalid_color"),
    "Invalid color: invalid_color\\. Please provide a valid colour name or hex code\\."
  )
  expect_error(
    inspect_balance(iris, fill_colour = "#GGGGGG"),
    "Invalid color: #GGGGGG\\. Please provide a valid colour name or hex code\\."
  )
  expect_error(
    inspect_balance(iris, title = 123),
    "Invalid title: 123\\. Please provide a non-empty character string\\."
  )
  expect_error(
    inspect_balance(iris, title = ""),
    "Invalid title: \\. Please provide a non-empty character string\\."
  )
  expect_error(
    inspect_balance(iris, title = c("Title1", "Title2")),
    "Invalid title: Title1Title2\\. Please provide a non-empty character string\\."
  )
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
  test_plot <- inspect_balance(data = iris_cat3,
                               vars = c("Species", "SepalWidthCategory"),
                               fill_colour = "blue",
                               title = "Custom Title")
  num_facets <- test_plot$data |>
    distinct(variable) |>
    nrow() # Count number of unique variables in the data
  plot_build <- ggplot2::ggplot_build(test_plot)
  barplot_layer <- test_plot$layers[[1]]
  fill_colour <- plot_build$data[[1]]$fill

  # Test ggplot object structure
  expect_s3_class(test_plot, "ggplot")
  expect_s3_class(barplot_layer$geom, "GeomBar")
  expect_equal(num_facets, 2) # One facet per variable specified

  # Test if custom fill colour is correctly applied
  expect_equal(barplot_layer$mapping$fill, NULL)
  expect_equal(fill_colour[1], "blue")

  # Check if custom title is set correctly
  expect_equal(test_plot$labels$title, "Custom Title")
})
