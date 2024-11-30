#' @title Inspect normality
#' @description
#' Quickly inspect the distribution of your numeric variables.
#'
#'
#' @param data A date frame or tibble with at least one numeric variable.
#' @param vars A vector of numeric variables contained in data. Default argument of `NULL` will produce plots for every numeric variable in the data.
#' @param ... Additional ggplot2 parameters to modify plot outputs.
#'
#' @return A faceted ggplot2 object with histograms for each numeric variable.
#' @import dplyr tidyr ggplot2
#' @export
#'
#' @examples
#' # Basic usage
#' inspect_normality(iris)
#'
#' # Advanced usage
#' # Manually specifies the variables to inspect and modifies plot transparency
#' inspect_normality(iris, c("Sepal.Length", "Sepal.Width"), alpha = 0.5)
inspect_normality <- function(data, vars = NULL, ...) {
  # Check if input is a data frame or a tibble
  if (!is.data.frame(data)) {
    stop("Input must be a data frame or a tibble.")
  }

  # Check if the dataset is empty
  if (nrow(data) == 0) {
    stop("The dataset is empty. No plots will be generated.")
  }

  # Select numeric columns
  numeric_data <- data |> dplyr::select(dplyr::where(is.numeric))

  # If no numeric columns, stop
  if (ncol(numeric_data) == 0) {
    stop("No numeric columns found in the data. Try using inspect_balance instead.")
  }

  # If 'vars' is NULL, select all numeric variables, otherwise use the provided ones
  if (is.null(vars)) {
    vars <- names(numeric_data)
  }

  # Check if the specified variables exist in the data
  if (!all(vars %in% names(data))) {
    stop("Some of the specified variables do not exist in the dataset.")
  }

  # Check if all specified variables are numeric
  non_numeric_vars <- vars[!vars %in% names(numeric_data)]
  if (length(non_numeric_vars) > 0) {
    stop(paste("The following variables are not numeric:", paste(non_numeric_vars, collapse = ", ")))
  }

  # Issue warning if selected variables exceed 20
  if (length(vars) > 20) {
    warning("The dataset has more than 20 selected variables. The plot might be crowded.")
  }

  # Plot histograms for each specified numeric variable
  plot_normality <- data |>
    dplyr::select(all_of(vars)) |>
    tidyr::pivot_longer(everything(), names_to = "variable", values_to = "value") |>
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 15, fill = "blue", color = "black", ...) +
    ggplot2::facet_wrap(~ variable, scales = "free", strip.position = "top") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Histograms of Numeric Variables", x = "Value", y = "Frequency")

  return(plot_normality)
}
