#' Plot segments and arrows
#'
#' Add segments or arrows to plot from prepare_segments.
#'
#' Input is a data frame with `x0`, `y0`, `x`, `y` columns as per [segments]
#' @param d data frame
#' @param ... arguments passed to [segments()]
plot_segments <- function(d, ...) {
  segments(d$x0, d$y0, d$x1, d$y1, ...)
}
#'
#' @name plot_segments
#' @name plot_segments
plot_arrows <- function(d, ...) {
  arrows(d$x0, d$y0, d$x1, d$y1, ...)
}

#' Path to segments
#'
#' Array logic trick to turn a path into segments.
#'
#' It's assumed that `x` is a vector of IDs to other records, but this function
#' only does the reshaping to paired indexes from path of indexes.
#' @param x vector
path_to_seg <- function (x)
{
  head(suppressWarnings(matrix(x, nrow = length(x) + 1, ncol = 2,
                               byrow = FALSE)), -2L)
}
#' Paths as segments
#'
#' Prepare paths of coordinates by Group as segments
#'
#' This is really specific to X, Y, G inputs but can be made more general to any set of grouped (and implicitly arranged) records.
#' @param X input X coordinate
#' @param Y input Y coordinate
#' @param G input grouping index
prepare_path_segments <- function(X, Y, G) {
  d <- data.frame(X = X, Y = Y, G = G, ROW_ID = seq_along(X))
  grouped_seg_id <- lapply(split(d$ROW_ID, d$G), function(x) path_to_seg(x))
  ds <- dplyr::bind_rows(lapply(grouped_seg_id, function(x) data.frame(x0 = d$X[x[, 1]], y0 = d$Y[x[, 1]],
                                                                       x1 = d$X[x[, 2]], y1 = d$Y[x[, 2]])), .id = "G")
  ds
}


#' Segments
#'
#' Prepare segments from two-layer u-v brick.
#'
#' The input is currently assumed to be a two-layer raster brick with horizontal (U) and vertical (V) components to vectors
#' at each pixel.
#' @param x [raster::brick] with two layers of vector field components (u, v)
#' @param scale multiplication factor for u-v
#' @param ... ignored
#' @export
prepare_segments <- function(x, scale = 3600, ...) {
  u <- raster::values(x[[1]]) * scale
  v <- raster::values(x[[2]]) * scale
  bad <- is.na(u)
  xy <- sp::coordinates(x)[!bad, ]
  u <- u[!bad]
  v <- v[!bad]
  data.frame(x0 = xy[, 1], y0 = xy[, 2], x1 = xy[, 1] + u, y1 = xy[,2] + v)
}
