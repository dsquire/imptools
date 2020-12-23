#' Plot density of observed and imputed values for a single variable
#'
#' This function returns density plots for the observed data (blue) and plots for
#' the imputed values of each imputation (red).
#'
#' @param data A data frame containing multiple imputations.
#' @param imp_id A grouping column that identifies the rows belonging to each
#'    imputation.
#' @param plot_var The variable you wish to plot.
#' @param kernel The kernel density type you want to use. "auto" will select
#'    Rectangular for variables with < 5 unique values and Gaussian otherwise.
#' @importFrom ggplot2 aes geom_density ggplot scale_color_manual
#' @importFrom glue glue
#' @importFrom rlang .data
#' @export
plot_density <- function(data, imp_id, plot_var, kernel = "auto") {

  imp_id_str <- deparse(substitute(imp_id))
  if (!(imp_id_str %in% colnames(data))) {
    stop("Error: imp_id must exist in data frame.")
  }

  plot_var_str <- deparse(substitute(plot_var))
  if (!(plot_var_str %in% colnames(data))) {
    stop("Error: plot_var must exist in data frame.")
  }

  if (kernel == "auto" & dplyr::n_distinct(.data[[plot_var]]) < 5) {
    kernel <- "rectangular"
  } else {
    kernel <- "gaussian"
  }

  na_var_str <- glue::glue("{plot_var_str}_NA")

  if (!(na_var_str %in% colnames(data))) {
    stop("Error: na_var must exist in data frame.")
  }

  ggplot2::ggplot(data,
                  ggplot2::aes({{ plot_var }},
                      color = .data[[na_var_str]],
                      group = interaction({{ imp_id }}, .data[[na_var_str]])
                      )
                  ) +
    ggplot2::scale_color_manual(values = c("#F03E3E", "#1C7ED6")) +
    ggplot2::geom_density(kernel = kernel, show.legend = FALSE)
}
