#' Layer with Grid or Function
#'
#' Draw ggplot2 layer with a grod or function.
#'
#' @param draw Either a [grob][grid::grob] object or a function which accepts
#'   two arguments (\code{data} and \code{coords}) and returns a
#'   [grob][grid::grob]. \cr \cr \code{data} contains value from parameter
#'   \code{data}, coords is value that have already been transformed to the plot
#'   scales. \cr \cr It is important to note that the columns names for the data
#'   and coords come from the aesthetic mappings in the \code{ggplot2} plot.
#' @param type \code{group} or \code{panel}, group which draws geom with
#'   \code{draw_group} draws collective geoms that display multiple observations
#'   with one geometric object and \code{panel} which draws geom with
#'   \code{draw_panel} draws individual geoms that display a distinct graphical
#'   object for each observation (row). Default: `group`
#' @inheritParams ggplot2::layer
#' @param ... Other arguments passed on to [ggplot2::layer()]. These are often
#'   aesthetics, used to set an aesthetic to a fixed value, like `colour =
#'   "red"` or `size = 3`. They may also be parameters to the paired geom/stat.
#'   Other arguments of function `draw` can be passed here.
#' @return a ggplot2 layer object
#' @details If you want to combine the functionality of multiple geoms it can
#'   usually be achieved by preparing the data for each of the geoms inside the
#'   `draw_*()` call and send it off to the different geoms, collecting the
#'   output in a [`grid::gList`] (a list of grobs) if the call is `draw_group()`
#'   or a [`grid::gTree`] (a grob containing multiple children grobs) if the
#'   call is `draw_panel()`.
#' @examples
#' gggrid_text <- grid::textGrob(
#'   "gggrid",
#'   x = c(0, 0, 0.5, 1, 1),
#'   y = c(0, 1, 0.5, 0, 1),
#'   hjust = c(0, 0, 0.5, 1, 1),
#'   vjust = c(0, 1, 0.5, 0, 1)
#' )
#' ggplot2::ggplot(data.frame(x = 1, y = 2))+
#'   geom_draw(gggrid_text)
#' @export
geom_draw <- function(draw = grid::nullGrob(), type = NULL,
                      mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", ..., inherit.aes = TRUE) {

  if (is.null(type)) type <- "group" else {
    type <- tolower(as.character(type))
    type <- match.arg(type, c("panel", "group"))
  }

  test_draw <- grid::is.grob(draw) || rlang::is_function(draw) || rlang::is_scalar_character(draw) || rlang::is_symbol(draw)

  if (!test_draw) {
    stop("Invalid 'draw' argument; draw must be a grob ",
         "or a function to call (can be a string, ",
         "symbol, call, or a function).",
         call. = FALSE)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = switch (type,
                   panel = GeomDrawPanel,
                   group = GeomDrawGroup
    ),
    position = position,
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params = list(draw = draw, ...)
  )

}

draw_fn <- function(data, panel_params, coord, draw, ...) {

  if (grid::is.grob(draw)) {

    draw

  } else {

    coords <- coord$transform(data, panel_params)
    rlang::exec(draw, data = data, coords = coords, ...)

  }
}

#' @inherit ggplot2::Geom title seealso
#' @inheritSection  ggplot2::Geom Geoms
#' @inheritSection  ggplot2::Coord Coordinate systems
#' @import ggplot2
#' @import rlang
#' @name ggplot2-ggproto
NULL

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDrawPanel <- ggplot2::ggproto(
  "GeomDrawPanel", ggplot2::Geom,
  ## No required_aes
  ## No default_aes
  ## No draw_key
  draw_panel = draw_fn)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDrawGroup <- ggplot2::ggproto(
  "GeomDrawGroup", ggplot2::Geom,
  ## No required_aes
  ## No default_aes
  ## No draw_key
  draw_group = draw_fn)
