#' @title Inspect normality
#' @description
#' Quickly inspect the distribution of numeric variables using histograms.
#'
#' @param data A non-empty data frame or tibble containing at least one numeric variable.
#' @param vars A character vector of numeric variable names contained in `data`. Default value `NULL` will produce plots for every numeric variable in the dataset.
#' @param fill_colour An R-supported colour name or hex value used to fill the bars of the histogram. Default value is "grey30".
#' @param title A non-empty string for the title of the plot. Default value `NULL` results in no title being displayed.
#' @param bins A positive integer specifying the number of bins in the histogram. Default value is `15`.
#' @param ... Additional `ggplot2` arguments passed to `geom_histogram` layer for customizing the plot output.
#'
#' @return A ggplot2 object containing histograms for each specified numeric variable.
#' @details
#' `inspect_normality` uses `ggplot2` to produce histograms of numeric variables to visualize their distributions. Any valid arguments that can be passed to a `geom_histogram` layer in ggplot2 may also be passed to `inspect_normality` to modify the plot outputs.
#'
#' @import dplyr tidyr ggplot2 stringr rlang glue
#' @export
#'
#' @examples
#' # Basic usage
#' inspect_normality(iris)
#'
#' # Advanced usage
#' # Specify the variables to inspect and modify the plot appearance
#' inspect_normality(data = iris,
#'                   vars = c("Sepal.Length", "Sepal.Width"),
#'                   fill_colour = "blue",
#'                   title = "Distribution of Sepal Length & Width",
#'                   bins = 10)
inspect_normality <- function(data,
                              vars = NULL,
                              fill_colour = "grey30",
                              title = NULL,
                              bins = 15,
                              ...) {
  # Check if input is a data frame or a tibble
  if (!is.data.frame(data)) {
    stop("Input must be a data frame or a tibble.")
  }

  # Check if the dataset is empty
  if (nrow(data) == 0) {
    stop("The dataset is empty. No plots will be generated.")
  }

  # Select numeric columns
  numeric_data <- data |>
    dplyr::select(dplyr::where(is.numeric))

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
    rlang::abort("Some of the specified variables do not exist in the dataset.")
  }

  # Check if all specified variables are numeric
  non_numeric_vars <- vars[!vars %in% names(numeric_data)]
  if (length(non_numeric_vars) > 0) {
    rlang::abort(glue::glue("The following variables are not numeric: {paste(non_numeric_vars, collapse = ', ')}"))
  }

  # Check if fill colour input is valid
  valid_colours <- grDevices::colors()
  is_hex_colour <- stringr::str_detect("^#(?:[0-9a-fA-F]{3}){1,2}$", fill_colour)
  if (!(fill_colour %in% valid_colours || is_hex_colour)) {
    rlang::abort(glue::glue("Invalid colour: {fill_colour}. Please provide a valid colour name or hex code."))
  }

  # Check if title provided is valid
  if (!is.null(title) && (!is.character(title) || length(title) != 1 || title == "")) {
    rlang::abort(glue::glue("Invalid title: {title}. Please provide a non-empty character string."))
  }

  # Issue warning if selected variables exceed 20
  if (length(vars) > 20) {
    rlang::warn("The dataset has more than 20 selected variables. The plot might be crowded.")
  }

  # Plot histograms for each specified numeric variable
  plot_normality <- data |>
    dplyr::select(dplyr::all_of(vars)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "value") |>
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = bins, fill = fill_colour, ...) +
    ggplot2::facet_wrap(~ variable, scales = "free", strip.position = "top") +
    ggplot2::labs(title = title)

  return(plot_normality)
}
