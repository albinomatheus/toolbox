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