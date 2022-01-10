#' Personal ggplot theme
#'
#' A precise, clean and minimal theme for ggplot2. Influenced by @@hrbrmstr and @@jkaupp.
#'
#' @param base_family Base font family
#' @param base_size Base font size
#' @param strip_text_family Facet label font family
#' @param strip_text_size Facet label text size
#' @param plot_title_family Plot tilte family
#' @param plot_title_size Plot title font size
#' @param plot_title_margin Plot title margin
#' @param subtitle_family Plot subtitle family
#' @param subtitle_size Plot subtitle size
#' @param subtitle_margin Plot subtitle margin
#' @param caption_family Plot caption family
#' @param caption_size Plot caption size
#' @param caption_margin Plot caption margin
#' @param axis_title_family Axis title font family
#' @param axis_title_size Axis title font size
#' @param axis_title_just Axis title font justification \code{blmcrt}
#' @param grid Panel grid (\code{TRUE}, \code{FALSE}, or a combination of
#'        \code{X}, \code{x}, \code{Y}, \code{y})
#' @param axis Axis \code{TRUE}, \code{FALSE}, [\code{xy}]
#' @param ticks Ticks \code{TRUE}, \code{FALSE}
#' @param dark Dark mode \code{TRUE}, \code{FALSE}
#' @param markdown Enabled ggtext markdown styling  \code{TRUE}, \code{FALSE}
#'
#' @export

theme_simple <- function(base_family="HelveticaNowDisplay Light",
                     base_size = 16,
                     strip_text_family = base_family,
                     strip_text_size = 16,
                     plot_title_family = "HelveticaNowDisplay Bold",
                     plot_title_size = 20,
                     plot_title_margin = 10,
                     subtitle_family = "HelveticaNowDisplay Light",
                     subtitle_size = 16,
                     subtitle_margin = 10,
                     caption_family = "HelveticaNowDisplay Light",
                     caption_size = 12,
                     caption_margin = 10,
                     axis_title_family = "HelveticaNowDisplay Medium",
                     axis_title_size = 12,
                     axis_title_just = "mm",
                     dark = FALSE,
                     grid = TRUE,
                     axis = FALSE,
                     ticks = FALSE,
                     markdown = TRUE) {

  ret <- ggplot2::theme_minimal(base_family = base_family, base_size = base_size)
  ret <- ret + ggplot2::theme(legend.background = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.key = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.title = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(plot.title.position = 'plot')
  ret <- ret + ggplot2::theme(legend.position = 'plot')
  ret <- ret + ggplot2::coord_cartesian(expand = FALSE)

  if (dark == TRUE) {

    ret <- ret + ggplot2::theme(plot.background = ggplot2::element_rect(fill ="#2E3440"),
                                text = ggplot2::element_text(color = "white"),
                                strip.text = ggplot2::element_text(color = "white"))

    grid_color <- "#E5E9F0"
    tick_color = "#E5E9F0"

  } else {

    grid_color <- "#cccccc"
    tick_color <- "#4d4d4d"
  }

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_line(color = grid_color, size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = grid_color, size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = grid_color, size = 0.05))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    }

  } else {
    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_blank())
  }

  if (inherits(axis, "character") | axis  ==  TRUE) {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_line(color = grid_color, size = 0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = grid_color, size = 0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = grid_color, size = 0.15))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = grid_color, size = 0.15))
      ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = grid_color, size = 0.15))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_blank())
  }

  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks  =  ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.x  =  ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.y  =  ggplot2::element_blank())
  } else {
    ret <- ret + ggplot2::theme(axis.ticks  =  ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.x  =  ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.y  =  ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.length  =  grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)

  ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(t = base_size/2)))
  ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(r = base_size/2))) + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj, size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj, size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = strip_text_size, family = strip_text_family))

  if(!markdown) {

    ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(color = tick_color,
                                                                    margin = ggplot2::margin(t = base_size/2)))
    ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(color = tick_color,
                                                                    margin = ggplot2::margin(r = base_size/2))) + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj,
                                                                     size = axis_title_size,
                                                                     family = axis_title_family,
                                                                     margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)))
    ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj,
                                                                     size = axis_title_size,
                                                                     family = axis_title_family,
                                                                     margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)))
    ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0,
                                                                   size = strip_text_size,
                                                                   family = strip_text_family))

    ret <- ret + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                                   size = plot_title_size,
                                                                   margin = ggplot2::margin(b = plot_title_margin),
                                                                   family = plot_title_family))
    ret <- ret + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0,
                                                                      size = subtitle_size,
                                                                      margin = ggplot2::margin(b = subtitle_margin),
                                                                      family = subtitle_family))
    ret <- ret + ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 1,
                                                                     size = caption_size,
                                                                     margin = ggplot2::margin(t = caption_margin),
                                                                     family = caption_family))

  } else {

    ret <- ret + ggplot2::theme(axis.text.x = ggtext::element_markdown(color = tick_color,
                                                                       margin = ggplot2::margin(t = base_size/2)))
    ret <- ret + ggplot2::theme(axis.text.y = ggtext::element_markdown(color = tick_color,
                                                                       margin = ggplot2::margin(r = base_size/2))) + ggplot2::theme(axis.title = ggtext::element_markdown(size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.x = ggtext::element_markdown(hjust = xj,
                                                                        size = axis_title_size,
                                                                        family = axis_title_family,
                                                                        color = tick_color,
                                                                        margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)))
    ret <- ret + ggplot2::theme(axis.title.y = ggtext::element_markdown(hjust = yj,
                                                                        size = axis_title_size,
                                                                        family = axis_title_family,
                                                                        color = tick_color,
                                                                        margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)))
    ret <- ret + ggplot2::theme(strip.text = ggtext::element_markdown(hjust = 0,
                                                                      size = strip_text_size,
                                                                      family = strip_text_family,
                                                                      color = tick_color))

    ret <- ret + ggplot2::theme(plot.title = ggtext::element_markdown(hjust = 0,
                                                                      size = plot_title_size,
                                                                      margin = ggplot2::margin(b = plot_title_margin),
                                                                      family = plot_title_family,
                                                                      color = tick_color))
    ret <- ret + ggplot2::theme(plot.subtitle = ggtext::element_markdown(hjust = 0,
                                                                         size = subtitle_size,
                                                                         margin = ggplot2::margin(b = subtitle_margin),
                                                                         family = subtitle_family,
                                                                         color = tick_color))
    ret <- ret + ggplot2::theme(plot.caption = ggtext::element_markdown(hjust = 1,
                                                                        size = caption_size,
                                                                        margin = ggplot2::margin(t = caption_margin),
                                                                        family = caption_family,
                                                                        color = tick_color))

  }

  ret <- ret + ggplot2::theme(plot.margin = ggplot2::margin(25, 25, 10, 25))

  ret

}