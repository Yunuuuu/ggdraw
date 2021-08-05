#' Debug ggplot2
#'
#' Debug ggplot2 by operate data with a function.
#'
#' @param debug a function which conducted with argument \code{coords}. \cr \cr
#'   coords is value that have already been transformed to the plot scales. \cr
#'   \cr It is important to note that the columns names for the data and coords
#'   come from the aesthetic mappings in the \code{ggplot2} plot. Default:
#'   `head`
#' @param type \code{group} or \code{panel}, group which draws geom with
#'   \code{draw_group} draws collective geoms that display multiple observations
#'   with one geometric object and \code{panel} which draws geom with
#'   \code{draw_panel} draws individual geoms that display a distinct graphical
#'   object for each observation (row). Default: `group`
#' @inheritParams ggplot2::layer
#' @param ... Other arguments passed on to [ggplot2::layer()]. These are often
#'   aesthetics, used to set an aesthetic to a fixed value, like colour = "red"
#'   or size = 3. They may also be parameters to the paired geom/stat. other
#'   arguments of function `debug` can be passed here.
#' @export
geom_debug <- function(debug = "head", type = NULL,
                       mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", ..., inherit.aes = TRUE) {

  if (is.null(type)) type <- "group" else {
    type <- tolower(as.character(type))
    type <- match.arg(type, c("panel", "group"))
  }

  test_debug <- rlang::is_function(debug) || rlang::is_scalar_character(debug) || rlang::is_symbol(debug)

  if (!test_debug) {
    stop("Invalid 'debug' argument; debug must be a function to call ",
         "(can be a string, symbol, call, or a function).",
         call. = FALSE)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = switch (type,
                   panel = GeomDebugPanel,
                   group = GeomDebugGroup
    ),
    position = position,
    show.legend = FALSE,
    inherit.aes = inherit.aes,
    params = list(debug = debug, ...)
  )

}

draw_debug <- function(data, panel_params, coord, debug, ...) {

  coords <- coord$transform(data, panel_params)
  debug(coords, ...)

}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDebugPanel <- ggplot2::ggproto(
  "GeomDebugPanel", ggplot2::Geom,
  ## No required_aes
  ## No default_aes
  ## No draw_key
  draw_panel = draw_debug)

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDebugGroup <- ggplot2::ggproto(
  "GeomDebugGroup", ggplot2::Geom,
  ## No required_aes
  ## No default_aes
  ## No draw_key
  draw_group = draw_debug)
