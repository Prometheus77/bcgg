#' Mosaic plot
#'
#' Creates a mosaic plot showing rectangular segments sized by both height and width parameters.
#'
#' @param .data The data frame or tibble containing your data
#' @param x.group The column name used to label segments on the x-axis
#' @param y.group The column name used to label segments on the y-axis
#' @param values The column name showing the value to use to determine the size of the segments
#' @param fill.values Fill colors to use for y.group.
#' @param segment.label Show numerical values for each segment? Default is `TRUE`.
#' @param text.format Format to use to display text values as a text string. Uses `sprintf()`. Default is '%.1f', which rounds the number to one decimal place.
#'
#' @export
bc_mosaic <- function(.data, x.group, y.group, values, fill.values = NULL, segment.label = TRUE, text.format = '%.1f') {
  x.group_q <- enquo(x.group)
  y.group_q <- enquo(y.group)
  values_q <- enquo(values)

  x.name = quo_name(x.group_q)

  .data = dplyr::mutate(.data, .x.group = !!x.group_q)
  .data = dplyr::group_by(.data, .x.group)
  .data = dplyr::mutate(.data, .yheight = !!values_q / sum(!!values_q))
  .data = dplyr::mutate(.data,
                        .ymax = cumsum(.yheight),
                        .ymin = .ymax - .yheight,
                        .ymid = (.ymax + .ymin) / 2)

  row_totals = dplyr::group_by(.data, .x.group)
  row_totals = dplyr::summarise(row_totals, .xwidth = sum(!!values_q))
  row_totals = dplyr::mutate(row_totals,
                             .xmax = cumsum(.xwidth),
                             .xmin = .xmax - .xwidth,
                             .xmid = (.xmax + .xmin) / 2)

  .data = dplyr::left_join(.data, row_totals, by = ".x.group")

  .plot = ggplot(.data) +
    geom_rect(aes(xmin = .xmin, xmax = .xmax, ymin = .ymin, ymax = .ymax, fill = !!y.group_q), color = "black") +
    geom_text(aes(x = .xmid, y = .ymid, label = !!values_q)) +
    scale_x_continuous(breaks = row_totals$.xmid, labels = row_totals$.x.group, name = x.name) +
    scale_y_continuous(labels = scales::percent)

  .plot
}

make_incremental <- function(.data, values) {
  values_q = enquo(values)
  dplyr::mutate(.data,
                lag.values = dplyr::lag(!!values_q, 1),
                .incr.values = ifelse(is.na(lag.values), !!values_q, !!values_q - lag.values))
}

make_absolute <- function(.data, values) {
  values_q = enquo(values)
  dplyr::mutate(.data,
                .abs.values = cumsum(!!values_q))
}
