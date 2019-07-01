#' Waterfall plot
#'
#' Creates a waterfall plot showing sources of variance as rectangular segments connected by dotted lines
#'
#' @param .data The data frame or tibble containing your data
#' @param labels The column name that you want to use to label the x-axis
#' @param values The column name that you want to include in the plot
#' @param incremental Set to TRUE if the series contains incremental values to plot. Set to FALSE if the series contains actual values and incremental values must be calculated. Default is TRUE.
#' @param anchors (optional) A column name, logical vector, or numeric vector indicating which segments should touch the x-axis.
#' @param anchor.final Whether to anchor the last bar in the waterfall. Default is TRUE.
#' @param width Bar width in units, where 1 means the edges of the bars touch. Default = 0.8.
#' @param fill.values Fill colors to use on waterfall, in order of 'pos', 'neg', 'anchor'. Default is `c('green', 'red', 'navy')`.
#' @param segment.label Show numerical values as text for each non-anchor column? Default is `TRUE`.
#' @param anchor.label Show numerical values as text for each anchor column? Default is `TRUE`.
#' @param text.format Format to use to display text values as a text string. Uses `sprintf()`. Default is '%.1f', which rounds the number to one decimal place.
#'
#' @export
bc_waterfall <- function(.data, labels, values, incremental = TRUE, anchors = NULL, anchor.final = TRUE, width = 0.8,
                         fill.values = c('green', 'red', 'navy'), segment.label = TRUE, anchor.label = TRUE, text.format = '%.1f') {
  labels_q <- enquo(labels)
  values_q <- enquo(values)

  # ggplot2 needs a numeric x-axis
  .data = dplyr::mutate(.data, .count = 1, .label.order = cumsum(.count))

  # ggplot2 needs to display labels in the right order
  .data = dplyr::mutate(.data, .labels = factor(!!labels_q))

  if (hasArg("anchors")) {
    anchors_q <- enquo(anchors)
    .data = dplyr::mutate(.data, .anchors = !!anchors_q)
  } else {
    lastrow <- nrow(.data)
    .data = dplyr::mutate(.data, .anchors = (.label.order %in% c(1, lastrow)))
  }

  if (incremental == FALSE) {
    .data = make_incremental(.data, !!values_q)
    .data = dplyr::rename(.data, .abs.values = !!values_q)
  } else {
    .data = make_absolute(.data, !!values_q)
    .data = dplyr::rename(.data, .incr.values = !!values_q)
  }

  .data = dplyr::mutate(.data,
                        .lag.abs = dplyr::lag(.abs.values, 1),
                        .lag.anchor = ifelse(.anchors, 0, .lag.abs),
                        .ymin = pmin(ifelse(is.na(.lag.anchor), 0, .lag.anchor), .abs.values),
                        .ymax = pmax(ifelse(is.na(.lag.anchor), 0, .lag.anchor), .abs.values),
                        .xmin = .label.order - width / 2,
                        .xmax = .label.order + width / 2,
                        .direction = dplyr::case_when(.anchors == TRUE ~ 'anchor',
                                                      .incr.values < 0 ~ 'neg',
                                                      TRUE ~ 'pos'),
                        .direction = factor(.direction, levels = c('pos', 'neg', 'anchor')),
                        .segment.label = ifelse(.anchors, NA, sprintf(text.format, .incr.values)),
                        .anchor.label = ifelse(!.anchors, NA, sprintf(text.format, .abs.values)))

  .plot = ggplot(.data, aes(x = .label.order)) +
    geom_step(aes(y = .abs.values), linetype = "dashed") +
    geom_rect(aes(xmin = .xmin, xmax = .xmax, ymin = .ymin, ymax = .ymax, fill = .direction), color = "black") +
    scale_fill_manual(values = fill.values) +
    scale_x_continuous(breaks = .data$.label.order, labels = .data$.labels, name = quo_name(labels_q)) +
    scale_y_continuous(name = quo_name(values_q)) +
    theme(legend.position = 'none')

  if (segment.label) {
    .plot = .plot + geom_label(aes(label = .segment.label, y = (.ymax + .ymin) / 2))
  }

  if (anchor.label) {
    .plot = .plot + geom_text(aes(label = .anchor.label, y = .abs.values * 1.05))
  }

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
