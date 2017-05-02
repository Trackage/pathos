#' plot segments from two-layer u-v brick
prepare_segments <- function(x, scale = 3600, ...) {
  u <- values(x[[1]]) * scale
  v <- values(x[[2]]) * scale
  bad <- is.na(u)
  xy <- coordinates(x)[!bad, ]
  u <- u[!bad]
  v <- v[!bad]
  data.frame(x0 = xy[, 1], y0 = xy[, 2], x1 = xy[, 1] + u, y1 = xy[,2] + u)
}
#' add segments to plot from prepare_segments
plot_segments <- function(d, ...) {
  segments(d$x0, d$y0, d$x1, d$y1, ...)
}
#' add arrows to plot from prepare_segments
plot_arrows <- function(d, ...) {
  arrows(d$x0, d$y0, d$x1, d$y1, ...)
} 
#' array logic trick to turn a path into segments
path_to_seg <- function (x) 
{
  head(suppressWarnings(matrix(x, nrow = length(x) + 1, ncol = 2, 
                               byrow = FALSE)), -2L)
}
#' prepare paths of coordinates by Group as segments
prepare_path_segments <- function(X, Y, G) {
   d <- data.frame(X = X, Y = Y, G = G, ROW_ID = seq_along(X))  
   grouped_seg_id <- lapply(split(d$ROW_ID, d$G), function(x) path_to_seg(x))
   ds <- dplyr::bind_rows(lapply(grouped_seg_id, function(x) data.frame(x0 = d$X[x[, 1]], y0 = d$Y[x[, 1]], 
                                                 x1 = d$X[x[, 2]], y1 = d$Y[x[, 2]])), .id = "G")
   #ds$G <- unique(G)[as.integer(ds$G)]
   
   ds
}

library(readxl)

x <- read_excel("CapePetData.xlsx", sheet = 2)
x <- subset(x, !is.na(Longitude))
x <- subset(x, !on_nest == "at.nest")
x$tripID <- paste0(x$Ind_ID, x$on_nest)
x$gmt <- ISOdatetime(x$Year, x$Month, x$Day, x$Hour, x$Minute, x$Second, tz = "GMT")

library(raadtools)
dummywind <- readamps_d1wind(min(x$gmt))

## create Eastings/Northings from longlat
XY <- rgdal::project(cbind(x$Longitude, x$Latitude), projection(dummywind))
x$X <- XY[, 1]
x$Y <- XY[, 2]

dummywind <- crop(dummywind, extent(XY))
#' Calculate wind magnitude from two-layer u-v brick
vlen <- function(x) sqrt(x[[1]]^2 + x[[2]]^2)
## small test
library(dplyr)

x$day_seq <- cut(x$gmt, "1 day")

#plot_segments(prepare_segments(dummywind))


day1 <- x %>% filter(day_seq == unique(x$day_seq)[1])
ps <- prepare_path_segments(day1$X, day1$Y, day1$tripID)
x %>% filter(tripID == "CapPet01trip.01") %>% plot(Y ~ X, data = .)
plot(vlen(dummywind), add = TRUE, col = viridis::viridis(100))
x %>% filter(tripID == "CapPet01trip.01", day_seq == x$day_seq[1]) %>% lines(Y ~ X, data = .)
plot_arrows(prepare_segments(dummywind), len = 0.1)
plot_arrows(ps, len = 0.1, col = rainbow(length(unique(ps$G)))[factor(ps$G)])

# 
dummywind <- readamps_d1wind(min(x$gmt) + 24 * 3600)
dummywind <- crop(dummywind, extent(XY))

day2 <- x %>% filter(day_seq == unique(x$day_seq)[2])
ps <- prepare_path_segments(day2$X, day2$Y, day2$tripID)
x %>% filter(tripID == "CapPet01trip.01") %>% plot(Y ~ X, data = .)
plot(vlen(dummywind), add = TRUE, col = viridis::viridis(100))
x %>% filter(tripID == "CapPet01trip.01", day_seq == x$day_seq[1]) %>% lines(Y ~ X, data = .)
plot_arrows(prepare_segments(dummywind), len = 0.1)
plot_arrows(ps, len = 0.1, col = rainbow(length(unique(ps$G)))[factor(ps$G)])



