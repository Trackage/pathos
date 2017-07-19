#tfile <- sprintf("%s.rda", tempfile())
#download.file("https://github.com/mdsumner/scsf/raw/master/data/minimal_mesh.rda", tfile, mode = "wb")
tfile <- "/tmp/Rtmp0NDplJ/file2f6e4511788f.rda"
load(tfile)
library(sf)
library(sc)
library(scsf)
library(dplyr)
#example(st_read)
sf1 <- st_sf(a = 1, geometry = st_sfc(st_multipolygon(unlist(lapply(st_geometry(minimal_mesh), unclass), recursive = FALSE))))
set.seed(1)
prim <- PRIMITIVE(sf1)

prim_valid_only <- function(prim) {
  ## which segments have to go? find where interchanging vertex pairs is not unique
  prim$segment$useg <-  apply(cbind(prim$segment$.vertex0, prim$segment$.vertex1), 1, function(x) paste(sort(x), collapse = "-"))
  prim$segment$bad <- duplicated(prim$segment$useg)

  ## these are the ids of the vertex pairs to be kept, one row per segment
  #bb <- prim$segment %>% dp %>% dplyr::select(.vertex0, .vertex1) %>% as.matrix()
  # # I'm failing to get anti_join to work today for some reason
  anti <- prim$segment %>% filter(bad)  %>% inner_join(prim$segment %>% dplyr::select(useg))
  segs <- prim$segment %>% filter( !prim$segment$useg %in% anti$useg)

  ## processed segment edges, could be in any configuration
  ## but are expected to link together in order
  prim$segment %>% filter( !prim$segment$useg %in% anti$useg) %>%
    dplyr::transmute(.v0 = .vertex0, .v1 =  .vertex1)
}

pe <- prim_valid_only(prim)
plot(match(pe$.v0, pe$.v1),
match(pe$.v1, pe$.v0) )
