prim_valid_only <- function(prim) {
  ## which segments have to go? find where interchanging vertex pairs is not unique
  prim$segment$useg <-  apply(cbind(prim$segment$.vertex0, prim$segment$.vertex1), 1, function(x) paste(sort(x), collapse = "-"))
  prim$segment$bad <- duplicated(prim$segment$useg)

  ## these are the ids of the vertex pairs to be kept, one row per segment
  #bb <- prim$segment %>% dp %>% dplyr::select(.vertex0, .vertex1) %>% as.matrix()
  # # I'm failing to get anti_join to work today for some reason
  anti <- prim$segment %>% filter(bad)  %>% inner_join(prim$segment %>% select(useg))
  segs <- prim$segment %>% filter( !prim$segment$useg %in% anti$useg)

  ## processed segment edges, could be in any configuration
  ## but are expected to link together in order
  prim$segment %>% filter( !prim$segment$useg %in% anti$useg) %>%
    dplyr::transmute(.v0 = .vertex0, .v1 =  .vertex1)
}

tfile <- sprintf("%s.rda", tempfile())
download.file("https://github.com/mdsumner/scsf/raw/master/data/minimal_mesh.rda",
              tfile, mode = "wb")
load(tfile)
library(scsf)
library(sc)
library(dplyr)
library(sf)

## minimal mesh with inner boundary in one feature
## 1 is the rhs
## 2 is the lhs
## 3 is the hole in 2
#ib_mmesh <- st_sf(a = 1, geometry =
#                    st_sfc(st_multipolygon(unlist(lapply(st_geometry(minimal_mesh), unclass), recursive = FALSE))))
#ib_mmesh <- st_sf(a = 1, geometry =
#                    st_sfc(st_multipolygon(lapply(unlist(lapply(st_geometry(minimal_mesh), unclass), recursive = FALSE),
#                                                  `[`, 1))))


library(spex)
two <- st_cast(spex::qm_rasterToPolygons(raster::raster(matrix(1:2, ncol = 2))), "MULTIPOLYGON")
two <- st_sf(a = 1, geometry =
                    st_sfc(st_multipolygon(unlist(lapply(st_geometry(two), unclass), recursive = FALSE))))


## generic primitive form (simple features are entirely path-based,
## so segments is sufficient topology, though other types will
## provide higher level primitives)
px <- PRIMITIVE(two)

pe <- prim_valid_only(px)
edges <- pathos::find_paths(pe$.v0, pe$.v1)

check_prim <- function(sfeat, edges, prim) {
  ## plot the original
  plot(st_geometry(sfeat))
  ## add the segments, assuming they in path-order
 lapply(split(edges, edges$path),
        function(x) lines(prim$vertex[match(c(x$.v0, tail(x$.v1, 1)), prim$vertex$vertex_), c("x_", "y_")], lwd = 4))
}

check_prim(two, mutate(pe, path = 1), px)
check_prim(two, pathos::find_paths(pe$.v0, pe$.v1), px)


px#library(rnaturalearth)
#spdf_france_country <- ne_states(country = 'france') %>% head(-5)
#set.seed(2)
#d2 <-  st_as_sf(spdf_france_country) %>% sample_frac(0.5)
#plot(d2[1])
#inner_ring_touching <- st_sf(geometry  = st_sfc(st_multipolygon(unlist(lapply(st_geometry(d2), unclass), recursive = FALSE))), name = "internal boundaried abomination")
# devtools::install_github("mdsumner/scsf")
library(scsf)
library(sc)
prim <- PRIMITIVE(inner_ring_touching)

## which segments have to go? find where interchanging vertex pairs is not unique
prim$segment$useg <-  apply(cbind(prim$segment$.vertex0, prim$segment$.vertex1), 1, function(x) paste(sort(x), collapse = "-"))
prim$segment$bad <- duplicated(prim$segment$useg)
library(dplyr)

## these are the ids of the vertex pairs to be kept, one row per segment
#bb <- prim$segment %>% dp %>% dplyr::select(.vertex0, .vertex1) %>% as.matrix()
## I'm failing to get anti_join to work today for some reason
anti <- prim$segment %>% filter(bad)  %>% inner_join(prim$segment %>% select(useg))
segs <- prim$segment %>% filter( !prim$segment$useg %in% anti$useg)

## processed segment edges, could be in any configuration
## but are expected to link together in order
pe <- prim$segment %>% filter( !prim$segment$useg %in% anti$useg) %>%
  dplyr::transmute(.v0 = .vertex0, .v1 =  .vertex1)

