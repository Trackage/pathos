#' Find path/s.
#'
#' Input segments as parallel vertex pairs to find individual paths.
#'
#' @param .v0
#' @param .v1
#'
#' @return
#' @export
#' @importFrom dplyr %>% mutate
#' @importFrom tibble tibble
#' @examples
#' library(tibble)
#' d <- tribble(
#'  ~vertex0, ~vertex1,
#'  "a", "b",
#'  "l", "m",
#'  "d", "e",
#'  "w", "x",
#'  "n", "o",
#'  "b", "c",
#'  "v", "w",
#'  "c", "d",
#'  "u", "v",
#'  "m", "n",
#'  "f", "g",
#'  "x", "z",
#'  "g", "a",
#'  "e", "f"
#')
#' ord2 <- order_paths(d$vertex0, d$vertex1)
#' d[ord2, ]
#'
#' find_paths(d$vertex0, d$vertex1)
find_paths <- function(.v0, .v1) {
  ## do not run this line within mutate below, as the local scope overrides
  ord <- order_paths(.v0, .v1)
  ## make sure classify paths is run on the ordered pairs ...
  cdpath <- classify_paths(.v0[ord], .v1[ord])

  ## todo input .data frame and use quosures for user-specificed start/end vertex id
  tibble::tibble(.v0 = .v0, .v1 = .v1)[ord, ] %>%
    dplyr::mutate(path = cdpath)
}

order_paths <- function(.v0, .v1) {
  ## put paths in order one after the other
  l <- factor(as.vector(t(cbind(.v0, .v0))))
  order(as.integer(l)[seq(2, length(l), by = 2)])
}
## then find breaks in the last/first sequence
oddminusone <- function(x) {
  sub <- x %% 2 != 0
  x[sub] <- x[sub] - 1
  x
}
classify_paths <- function(.v0, .v1) {
  path <- oddminusone(c(0, cumsum(abs(diff(head(.v1, -1) != tail(.v0, -1))))) )
  c(path, tail(path, 1))
}


