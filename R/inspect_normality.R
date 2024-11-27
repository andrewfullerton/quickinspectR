inspect_normality <- function(data, ...) {
  # Check if input is a data frame or tibble
  if (!is.data.frame(data)) {
    stop("Input must be a data frame or a tibble.")
  }

  # Check if the dataset is empty
  if (nrow(data) == 0) {
    stop("The dataset is empty. No plots will be generated.")
  }

  # Check if there are numeric columns in the data
  numeric_data <- data |> select(where(is.numeric))
  if (ncol(numeric_data) == 0) {
    stop("No numeric columns found in the data. Try using inspect_balance instead.")
  }

  # Check if pivot will be successful
  reshaped_data <- data |>
    select(where(is.numeric)) |>
    pivot_longer(everything(), names_to = "variable", values_to = "value")
  if (nrow(reshaped_data) == 0) {
    stop("Pivoting the data resulted in an empty dataset.")
  }

  # Issue warning if data has more than 20 numeric columns
  if (ncol(numeric_data) > 20) {
    warning("The dataset has more than 20 numeric columns. The plot might be crowded. Consider selecting a subset of variables.")
  }

  # Plot histograms for each numeric variable
  data |>
    select(where(is.numeric)) |>
    pivot_longer(everything(), names_to = "variable", values_to = "value") |>
    ggplot(aes(x = value)) +
    geom_histogram(bins = 15, fill = "blue", color = "black", alpha = 0.7, ...) +
    facet_wrap(~ variable, scales = "free", strip.position = "top") +
    theme_minimal() +
    labs(title = "Histograms of Numeric Variables", x = "Value", y = "Frequency")
}
