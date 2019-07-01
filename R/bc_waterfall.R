#' Waterfall plot
#'
#' Creates a waterfall plot showing sources of variance as rectangular segments connected by dotted lines
#'
#' @param .data The data frame or tibble containing your data
#' @param labels The column name that you want to use to label the x-axis
#' @param values The column name that you want to include in the plot
#' @param incremental Set to TRUE if the series contains incremental values to plot. Set to FALSE if the series contains actual values and incremental values must be calculated. Default is TRUE.
#' @param anchors (optional) A column name, logical vector, or numeric vector indicating which segments should touch the x-axis.
#' @param add.final Whether to add a final (anchored) bar to the end of the waterfall. Default is FALSE.
#'
#' @export
bc_waterfall <- function(.data, labels, values, incremental = TRUE, anchors = NULL, final = FALSE) {
  labels <- enquo(labels)
  values <- enquo(values)
  if (incremental == FALSE) {
  }

}

make_incremental <- function(.data, values) {
  values_q = enquo(values)
  .data %>%
    dplyr::mutate(lag.values = lag(!!values_q),
                  values = ifelse(is.na(lag.values), !!values_q, !!values_q - lag.values))
}
