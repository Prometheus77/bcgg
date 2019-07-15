#' Mosaic plot
#'
#' Creates a mosaic plot showing rectangular segments sized by both height and width parameters.
#'
#' @param .data The data frame or tibble containing your data
#' @param x.group The column name used to label segments on the x-axis
#' @param y.group The column name used to label segments on the y-axis
#' @param values The column name showing the value to use to determine the size of the segments
#' @param fill.values Fill colors to use for y.group.
#' @param segment.label For each segment, show 'value', 'percent', or 'none'? Default is 'value'.
#' @param segment.format Format to use to display segment values as a text string. Uses `sprintf()`.
#' @param total.label Show the total for each x.group? Default is `FALSE`.
#' @param total.format Format to use to display totals as a text string. Uses `sprintf()`.
#'
#' @export
bc_mosaic <- function(.data, x.group, y.group, values, fill.values = NULL, segment.label = 'value', segment.format = NULL, total.label = FALSE, total.format = '%0.0f') {
  x.group_q <- enquo(x.group)
  y.group_q <- enquo(y.group)
  values_q <- enquo(values)

  x.name = quo_name(x.group_q)
  y.name = quo_name(y.group_q)

  .data = dplyr::group_by(.data, !!x.group_q)
  .data = dplyr::mutate(.data, .yheight = !!values_q / sum(!!values_q))
  .data = dplyr::mutate(.data,
                        .ymax = cumsum(.yheight),
                        .ymin = .ymax - .yheight,
                        .ymid = (.ymax + .ymin) / 2)

  row_totals = dplyr::group_by(.data, !!x.group_q)
  row_totals = dplyr::summarise(row_totals, .xwidth = sum(!!values_q))
  row_totals = dplyr::mutate(row_totals,
                             .xmax = cumsum(.xwidth),
                             .xmin = .xmax - .xwidth,
                             .xmid = (.xmax + .xmin) / 2)

  .data = dplyr::left_join(.data, row_totals, by = x.name)

  .plot = ggplot(.data) +
    geom_rect(aes(xmin = .xmin, xmax = .xmax, ymin = .ymin, ymax = .ymax, fill = !!y.group_q), color = "black")

  if (segment.label == 'value') {
    if (!hasArg(segment.format)) segment.format = '%0.1f'
    .plot = .plot + geom_text(aes(x = .xmid, y = .ymid, label = sprintf(segment.format, !!values_q)))
  } else if (segment.label == 'percent') {
    if (!hasArg(segment.format)) segment.format = '%0.1f%%'
    .plot = .plot + geom_text(aes(x = .xmid, y = .ymid, label = sprintf(segment.format, .yheight * 100)))
  }

  if (total.label == TRUE) {
    .plot = .plot + geom_text(.data = row_totals, aes(x = .xmid, y = 1.05, label = sprintf(total.format, .xwidth)))
  }

  .plot = .plot +
    scale_x_continuous(breaks = row_totals$.xmid, labels = row_totals[[x.name]], name = x.name) +
    scale_y_continuous(labels = scales::percent, name = y.name)

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
