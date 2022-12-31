#' Percentages
#'
#' @param labels vector of labels
#'
#' @return formatted labels
#' @export
scale_percent_labels <- function(labels){

  labels <- labels*100

  labels[length(labels)] <- paste0(labels[length(labels)], "%")

  return(labels)

}

#' Years
#'
#' @param labels vector of labels
#'
#' @return formatted labels
#' @export
scale_years_labels <- function(labels){

  labels <- labels

  labels[length(labels)] <- paste0(labels[length(labels)], "anos")

  return(labels)

}

#' Currency
#'
#' @param labels vector of labels
#'
#' @return formatted labels
#' @export
scale_dollar_labels <- function(labels){

  labels <- labels

  labels[length(labels)] <- paste0(labels[length(labels)], "$")

  return(labels)

}

#' Longitude
#'
#' @param labels vector of labels
#'
#' @return formatted labels
#' @export
scale_x_longitude <- function(xmin=-180, xmax=180, step=1, ...) {
    ewbrks <- seq(xmin,xmax,step)
    ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste(x, "W"), ifelse(x > 0, paste(x, "E"),x))))
    return(scale_x_continuous("Longitude", breaks = ewbrks, labels = ewlbls, expand = c(0, 0), ...))
}

#' Latitude
#'
#' @param labels vector of labels
#'
#' @return formatted labels
#' @export
scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
    nsbrks <- seq(ymin,ymax,step)
    nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(x, "S"), ifelse(x > 0, paste(x, "N"),x))))
    return(scale_y_continuous("Latitude", breaks = nsbrks, labels = nslbls, expand = c(0, 0), ...))
}

#' X & Y scales with opinionated pre-sets for percent & comma label formats
#'
#' The `_percent` ones set precent format for axis text and `expand=c(0,0)` (you need to set limits).
#' @param name The name of the scale. Used as axis or legend title. If
#'   `waiver()`, the default, the name of the scale is taken from the first
#'   mapping used for that aesthetic. If `NULL`, the legend title will be
#'   omitted.
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks computed by the
#'     transformation object
#'   - A numeric vector of positions
#'   - A function that takes the limits as input and returns breaks
#'     as output
#' @param minor_breaks One of:
#'   - `NULL` for no minor breaks
#'   - `waiver()` for the default breaks (one minor break between
#'     each major break)
#'   - A numeric vector of positions
#'   - A function that given the limits returns a vector of minor breaks.
#' @param guide	guide	A function used to create a guide or its name. See [guides()] for more information.
#' @param n.breaks	An integer guiding the number of major breaks. The algorithm may choose a
#'        slightly different number to ensure nice break labels. Will only have an effect if
#'        `breaks = waiver()`. Use NULL to use the default number of breaks given by the transformation.
#' @param labels Specifying overrides the default format (i.e. you really don't
#'   want to do that). `NULL` means no labels.
#' @param limits A numeric vector of length two providing limits of the scale.
#'   Use `NA` to refer to the existing minimum or maximum.
#' @param na.value If `na.translate = TRUE`, what value aesthetic
#'   value should missing be displayed as? Does not apply to position scales
#'   where `NA` is always placed at the far right.
#' @param expand same as in ggplot2
#' @param trans Either the name of a transformation object, or the
#'   object itself. Built-in transformations include "asn", "atanh",
#'   "boxcox", "exp", "identity", "log", "log10", "log1p", "log2",
#'   "logit", "probability", "probit", "reciprocal", "reverse" and "sqrt".
#' @param position The position of the axis. "left" or "right" for vertical
#' scales, "top" or "bottom" for horizontal scales
#' @param sec.axis specify a secondary axis
#' @export
scale_x_percent <- function(name = waiver(),
                            breaks = waiver(),
                            minor_breaks = waiver(),
                            guide = waiver(),
                            n.breaks = NULL,
                            labels,
                            limits = NULL,
                            expand = c(0.01,0),
                            na.value = NA_real_,
                            trans = "identity",
                            position = "bottom",
                            sec.axis = waiver(),
                            accuracy = 1,
                            scale = 100,
                            prefix = "",
                            suffix = "%",
                            big.mark = " ",
                            decimal.mark = ".",
                            trim = TRUE, ...) {

  if (missing(labels)) {
    scales::percent_format(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      ...
    ) -> labels
  }

  ggplot2::continuous_scale(
    aesthetics = c(
      "x", "xmin", "xmax", "xend", "xintercept", "xmin_final",
      "xmax_final", "xlower", "xmiddle", "xupper", "x0"
    ),
    scale_name = "position_c",
    palette = identity,
    name = name,
    breaks = breaks,
    n.breaks = n.breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    limits = limits,
    expand = expand,
    na.value = na.value,
    trans = trans,
    guide = guide,
    position = position,
    super = ScaleContinuousPosition
  ) -> sc

  sc

}

#' @export
scale_y_percent <- function(name = waiver(),
                            breaks = waiver(),
                            minor_breaks = waiver(),
                            guide = waiver(),
                            n.breaks = NULL,
                            labels,
                            limits = NULL,
                            expand = c(0.01,0),
                            na.value = NA_real_,
                            trans = "identity",
                            position = "left",
                            sec.axis = waiver(),
                            accuracy = 1,
                            scale = 100,
                            prefix = "",
                            suffix = "%",
                            big.mark = " ",
                            decimal.mark = ".",
                            trim = TRUE, ...) {

  if (missing(labels)) {
    scales::percent_format(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      ...
    ) -> labels
  }

  ggplot2::continuous_scale(
    aesthetics = c(
      "y", "ymin", "ymax", "yend", "yintercept",
      "ymin_final", "ymax_final", "lower", "middle", "upper"
    ),
    scale_name = "position_c",
    palette = identity,
    name = name,
    breaks = breaks,
    n.breaks = n.breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    limits = limits,
    expand = expand,
    na.value = na.value,
    trans = trans,
    guide = guide,
    position = position,
    super = ScaleContinuousPosition
  ) -> sc


  sc

}