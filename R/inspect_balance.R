#' @title Inspect class imbalance
#' @description
#' Quickly inspect the distribution of your categorical variables.
#'
#'
#' @param data A date frame or tibble with at least one categorical variable.
#' @param vars A vector of categorical variables contained in data. Default value `NULL` will produce plots for every categorical variable in the data.
#' @param fill_colour An R-supported colour or hex code to assign as the barplot colour. Default value is "grey30"
#' @param title A string. Default value `NULL` is no title.
#' @param ... Additional ggplot2 parameters to modify plot outputs.
#'
#' @return A ggplot2 object with bar plots for each categorical variable.
#' @details
#' `inspect_balance` uses ggplot2 to produce bar plots. Any valid arguments that may be passed to a `geom_bar` layer may also be passed to `inspect_balance` to modify plot outputs.
#'
#' @import dplyr tidyr ggplot2
#' @export
#'
#' @examples
#' # Basic usage
#' inspect_balance(iris)
#'
#' # Advanced usage
#' # Manually specifies the variable to inspect and modifies plot output
#' inspect_balance(data = iris,
#'                 c("Species"),
#'                 fill_colour = "blue",
#'                 title = "Class balance of Species")
inspect_balance <- function(data,
                            vars = NULL,
                            fill_colour = "grey30",
                            title = NULL,
                            ...) {
  # Check if input is a data frame
  if (!is.data.frame(data)) {
    stop("Input is not a data frame or a tibble.")
  }

  # Check if the dataset is empty
  if (nrow(data) == 0) {
    stop("The dataset is empty. No plots will be generated.")
  }

  # Check if there are any categorical columns in the data
  categorical_data <- data |> dplyr::select(dplyr::where(~ is.factor(.) || is.character(.)))
  if (ncol(categorical_data) == 0) {
    stop("No categorical columns found in the data. Try using inspect_normality instead.")
  }

  # If no variables are specified, select categorical columns by default
  if (is.null(vars)) {
    vars <- names(categorical_data)
  }

  # Check if the specified variables exist in the data
  if (!all(vars %in% names(data))) {
    stop("Some of the specified variables do not exist in the dataset.")
  }

  # Check if the specified variables exist in the categorical data
  non_categorical_vars <- vars[!vars %in% names(categorical_data)]
  if (length(non_categorical_vars) > 0) {
    stop("The following variables are not categorical: ", paste(non_categorical_vars, collapse = ", "))
  }

  # Check if fill colour input is valid
  valid_colors <- grDevices::colors()
  is_hex_color <- grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", fill_colour)
  if (!(fill_colour %in% valid_colors || is_hex_color)) {
    stop("Invalid color: ", fill_colour, ". Please provide a valid colour name or hex code.")
  }

  # Check if title provided is valid
  if (!is.null(title) && (!is.character(title) || length(title) != 1 || title == "")) {
    stop("Invalid title: ", title, ". Please provide a non-empty character string.")
  }

  # Issue warning if selected variables exceed 20
  if (length(vars) > 20) {
    warning("The dataset has more than 20 selected variables. The plot might be crowded.")
  }

  # Plot class balance for each categorical variable
  plot_balance <- data |>
    dplyr::select(dplyr::all_of(vars)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "value") |>
    ggplot2::ggplot(aes(x = value)) +
    ggplot2::geom_bar(show.legend = FALSE, fill = fill_colour, ...) +
    ggplot2::facet_wrap(~ variable, scales = "free_x", strip.position = "top") +
    ggplot2::theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(title = title)

  return(plot_balance)
}

