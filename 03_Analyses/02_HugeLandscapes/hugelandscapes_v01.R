


## Create a large roadmap from the jambi roadmap:
library(sf)
library(sp)
library(raster)
library(dplyr)

## Load shapefile:
jambiroads <- sf::st_read("D:/owncloud/CRC/10_Geodata/GIS/roads/JAMBI_road.shp") %>% dplyr::select()
extent(jambiroads)

## Function to find the correct UTM crs system for projection:
lonlat2UTM = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}

epsg_utm = lonlat2UTM(c(mean(c(extent(jambiroads)[1], extent(jambiroads)[2])), mean(c(extent(jambiroads)[3], extent(jambiroads)[4]))))
#st_crs(epsg_utm)$proj4string

## Set UTM projection:
jambiroads <- st_transform(jambiroads, epsg_utm)
extent(jambiroads)

## Select boundaries:
boundary <- c(xmin=190000, xmax=240000, ymin=9830000, ymax=9880000)
#boundary <- c(xmin=170000, xmax=200000, ymin=9750000, ymax=9800000)
jambiroads_sm <- st_intersection(jambiroads, st_as_sfc(st_bbox(boundary, crs=st_crs(jambiroads))))
jambiroads_sm_bb <- st_make_grid(jambiroads_sm, n = 1)
jambiroads_sm_bb <- sf::st_as_sf(sf::as_Spatial(jambiroads_sm_bb))

plot(jambiroads_sm)
plot(jambiroads_sm_bb)
plot(jambiroads_sm, add=TRUE)



## Save:
sf::st_write(jambiroads_sm, file.path("02_EFForTS-LGraf/1_Model/EFForTS-LGraf/input/LGraf_roadmaps/jambiLarge4_road.shp"))
sf::st_write(jambiroads_sm_bb, file.path("02_EFForTS-LGraf/1_Model/EFForTS-LGraf/input/LGraf_roadmaps/jambiLarge4_area.shp"))

## Read and check:
jambiroads_sm <- sf::st_read("02_EFForTS-LGraf/1_Model/EFForTS-LGraf/input/LGraf_roadmaps/jambiLarge1_road.shp")
jambiroads_sm_bb <- sf::st_read("02_EFForTS-LGraf/1_Model/EFForTS-LGraf/input/LGraf_roadmaps/jambiLarge1_area.shp")

plot(jambiroads_sm_bb)
plot(jambiroads_sm, add=TRUE)





####
## Try to create huge landscapes:

library(nlrx)

# Windows default NetLogo installation path (adjust to your needs!):
netlogopath <- file.path("C:/Program Files/NetLogo 6.0.3")
modelpath <- "02_EFForTS-LGraf/1_Model/EFForTS-LGraf/EFForTS-LGraf.nlogo"
outpath <- "03_Analyses/02_HugeLandscapes"

nl <- nl(nlversion = "6.0.3",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 8024)


nl@experiment <- experiment(expname="LGraf",
                            outpath=outpath,
                            repetition=1,
                            tickmetrics="false",
                            idrunnum="foldername",
                            idsetup=c("setup"),
                            idgo=c("establish_fields", "assign-land-uses"),
                            idfinal=NA_character_,
                            metrics.patches=c("pxcor", "pycor", "p_landuse-type", "p_road"),
                            metrics = c("count patches"),
                            constants = list("reproducable?" = "FALSE",   ## random seed is set via nlrx
                                             "rnd-seed" = -1570439526,
                                             "width" = 1000,
                                             "height" = 1000,
                                             "cell-length-meter" = 50,
                                             "road.algorithm" = "\"real.shapefile\"",
                                             "road-map-id" = "\"jambiLarge4\"", 
                                             "total-road-length" = 8000,  # not in use
                                             "min-dist-roads" = 5, # not in use
                                             "perlin-octaves" = 2, # not in use
                                             "perlin-persistence" = 0.1, # not in use
                                             "cone-angle" = 90, # not in use
                                             "dist-weight" = 0.1, # not in use
                                             "counter-nr" = 500,
                                             "households-per-cell" = 5,
                                             "setup-model" = "\"agricultural-area\"",
                                             "number-of-households" = 3000, # not in use
                                             "number-of-villages" = 62, # not in use
                                             "proportion-agricultural-area" = 0.3, 
                                             "hh-area-distribution" = "\"log-normal\"",
                                             "hh-area-mean-ha" = 1,
                                             "hh-area-sd-ha" = 0.5,
                                             "vlg-area-distribution" = "\"normal\"",
                                             "vlg-area-mean" = 80,
                                             "vlg-area-sd" = 50,
                                             "min-distance" = 10,
                                             "occ-probability" = 0,
                                             "inaccessible-area-fraction" = 0,
                                             "field-size-distribution" = "\"log-normal\"",
                                             "use-field-size-percentage?" = "FALSE",
                                             "field-size-percentage" = 0,
                                             "field-size-mean-ha" = 0.49,
                                             "field-size-sd-ha" = 0.77,
                                             "set-field-strategies-by-id?" = "TRUE",
                                             "field-strategies-id" = 1,
                                             "change-strategy" = 10,
                                             "field.shape.factor" = 1,
                                             "LUT-1-name" = "\"oilpalm\"",
                                             "LUT-2-name" = "\"rubber\"",
                                             "LUT-1-fraction" = 0.5,
                                             "LUT-2-fraction" = 0.5,
                                             "LUT-3-fraction" = 0,
                                             "LUT-4-fraction" = 0,
                                             "LUT-5-fraction" = 0,
                                             "LUT-1-specialize" = 0,
                                             "LUT-2-specialize" = 0,
                                             "LUT-3-specialize" = 0,
                                             "LUT-4-specialize" = 0,
                                             "LUT-5-specialize" = 0,
                                             "LUT-fill-up" = "\"LUT-2-fraction\"",
                                             "land-use-types" = "\"landscape-level-fraction\"",
                                             "default.maps" = "\"landuse-type\"", 
                                             "write.param.file?" = "FALSE", ## useful for debugging
                                             "print-messages?" = "TRUE",
                                             "write-household-ids" = "\"only-first-households\""))

nl@simdesign <- simdesign_simple(nl, nseeds=1)

res <- run_nl_all(nl)
nl@simdesign@simoutput <- res
saveRDS(nl, file.path(outpath, "hugeLandscapes_50km_v1.rds"))

resraster <- nl_to_raster(nl)
library(raster)
rst1_mod <- overlay(resraster$spatial.raster[[1]][[1]], resraster$spatial.raster[[1]][[2]], fun = function(x, y) {
  x[(y[] == 1)] <- NA
  return(x)
})


png(filename = file.path(outpath, "hugeLandscapes_50km_v1.png"), width = 1600, height=1600)
plot(rst1_mod, colNA="black")  
dev.off()



