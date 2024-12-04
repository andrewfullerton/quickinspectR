#' @title Inspect missing values
#' @description
#' Quickly inspect the missing values in a dataset by producing a bar plot.
#'
#' @param data A non-empty data frame or tibble.
#' @param vars A character vector of column names to inspect. Default value `NULL` will produce plots for every variable in the data.
#' @param na_colour An R-supported colour or hex value to indicate missing values. Default value is "red".
#' @param fill_colour An R-supported colour or hex value to indicate non-missing values. Default value is "grey30".
#' @param title A non-empty string for the plot title. Default value `NULL` results in no title being displayed.
#' @param ... Additional `ggplot2` arguments passed to `geom_bar` layer to modify plot outputs.
#'
#' @return A ggplot2 object (a bar plot) showing the proportion of missingness for the specified variables.
#' @details
#' `inspect_missing` uses `ggplot2` to produce a bar plot representing the proportion of missing and non-missing values for each column in the dataset. Any valid arguments that may be passed to a `geom_bar` layer in `ggplot2` may also be passed to `inspect_missing` via `...` to customize the plot appearance.
#'
#' @import dplyr tidyr ggplot2 stringr rlang glue
#' @export
#'
#' @examples
#' # Basic usage
#' inspect_missing(airquality)
#'
#' # Advanced usage: Manually specify variables and modify plot output
#' inspect_missing(data = airquality,
#'                 vars = c("Ozone", "Solar.R"),
#'                 na_colour = "green",
#'                 fill_colour = "blue",
#'                 alpha = 0.5)
inspect_missing <- function(data,
                            vars = NULL,
                            na_colour = "red",
                            fill_colour = "grey30",
                            title = NULL,
                            ...) {
  # Check if input is a data frame
  if (!is.data.frame(data)) {
    rlang::abort("Input is not a data frame or a tibble.")
  }

  # Check if the dataset is empty
  if (nrow(data) == 0) {
    rlang::abort("The dataset is empty. No plots will be generated.")
  }

  # Use specified variables (if any), otherwise default to all variables
  selected_vars <- if (is.null(vars)) {
    names(data)
  } else {
    vars
  }

  # Check if all specified variables exist in the dataset
  invalid_vars <- setdiff(selected_vars, names(data))
  if (length(invalid_vars) > 0) {
    rlang::abort(glue::glue("The following variables do not exist in the dataset: {paste(invalid_vars, collapse = ', ')}"))
  }

  # Check if fill colour input is valid
  colour_validate <- function(colour) {
    valid_colours <- grDevices::colors()
    is_hex_colour <- stringr::str_detect("^#(?:[0-9a-fA-F]{3}){1,2}$", colour)

    if (!(colour %in% valid_colours || is_hex_colour)) {
      rlang::abort(glue::glue("Invalid colour: {colour}. Please provide a valid colour name or hex code."))
    }
  }

  colour_validate(na_colour)
  colour_validate(fill_colour)

  # Check if title provided is valid
  if (!is.null(title) && (!is.character(title) || length(title) != 1 || title == "")) {
    rlang::abort(glue::glue("Invalid title: {title}. Please provide a non-empty character string."))
  }

  # Calculate missing values count for each variable
  missing_values <- data |>
    dplyr::summarise(dplyr::across(dplyr::all_of(selected_vars), ~sum(is.na(.)))) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "missing") |>
    dplyr::mutate(non_missing = nrow(data) - missing)

  # Plot missingness
  plot_missing <- missing_values |>
    tidyr::pivot_longer(cols = c(missing, non_missing),
                 names_to = "status", values_to = "proportion") |>
    ggplot2::ggplot(aes(variable, proportion, fill = status)) +
    ggplot2::geom_bar(stat = "identity", position = "fill", ...) +
    ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggplot2::scale_fill_manual(values = c(na_colour, fill_colour)) +
    ggplot2::labs(title = title)

  return(plot_missing)
}

