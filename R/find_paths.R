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

#' find_paths(d$vertex0, d$vertex1)
#'
#'
find_paths <- function(.v0, .v1) {
  cycs <- cycles(cbind(.v0, .v1))
  tibble::tibble(.v0 = .v0[cycs$row], .v1 = .v1[cycs$row], path = cycs$cycle)
}

cycles <- function(aa) {
  ii <- 1
  set0 <- ii
  visited <- logical(nrow(aa))
  while(!all(visited)) {
    i0 <- ii
    repeat {
      ii <- which(aa[,1] == aa[ii, 2])
      if (length(ii) < 1 | ii[1] == i0) {
        set0 <- c(set0, NA_integer_)
        break;
      }
      set0 <- c(set0, ii)
    }
    visited <- seq(nrow(aa)) %in% na.omit(set0)
    ii <- which(!visited)[1L]
    if (!is.na(ii)) set0 <- c(set0, ii)
  }
  l <- split(set0, c(0, cumsum(abs(diff(is.na(set0))))))
  bind_rows(lapply(l[!unlist(lapply(l, function(x) all(is.na(x))))], function(x) tibble(row = x)), .id = "cycle")
}


## whoops, nothing to see here
# find_paths0 <- function(.v0, .v1) {
#   ## do not run this line within mutate below, as the local scope overrides
#   ord <- order_paths(.v0, .v1)
#   ## make sure classify paths is run on the ordered pairs ...
#   cdpath <- classify_paths(.v0[ord], .v1[ord])
#
#   ## todo input .data frame and use quosures for user-specificed start/end vertex id
#   tibble::tibble(.v0 = .v0, .v1 = .v1)[ord, ] %>%
#     dplyr::mutate(path = cdpath)
# }
#
# ## then find breaks in the last/first sequence
# oddminusone <- function(x) {
#   sub <- x %% 2 != 0
#   x[sub] <- x[sub] - 1
#   x
# }
# classify_paths <- function(.v0, .v1) {
#   path <- oddminusone(c(0, cumsum(abs(diff(head(.v0, -1) != tail(.v1, -1))))) )
#   c(path, tail(path, 1))
# }

