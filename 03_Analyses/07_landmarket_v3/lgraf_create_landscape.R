

### generate landscape
#
# We want to create one landscape for the analysis
#
#
#####################################
## Load libraries:
library(tidyverse)
library(nlrx)

set.seed(748352)

## Setup nl object:
netlogopath <- file.path("C:Program Files/NetLogo 6.1.0")
modelpath <- file.path("02_EFForTS-LGraf/1_Model/EFForTS-LGraf/EFForTS-LGraf.nlogo")
outpath <- file.path("02_EFForTS-LGraf/1_Model/EFForTS-LGraf/output")

nl <- nl(nlversion = "6.1.0",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nl@experiment <- experiment(expname = "landmarket",
                            outpath = outpath,
                            repetition = 1,
                            tickmetrics = "false",
                            idsetup = "setup",
                            idgo = c("establish_fields", "assign-land-uses"),
                            idfinal = "write-output",
                            runtime = 1,
                            constants = list("reproducable?" = "false",
                                             "width" = 100,
                                             "height" = 100,
                                             "cell-length-meter" = 50,
                                             "road.algorithm" = "\"real.shapefile\"",
                                             "road-map-id" = "\"jambi1\"",
                                             "total-road-length" = 500,
                                             "min-dist-roads" = 5,
                                             "perlin-octaves" = 3,
                                             "perlin-persistence" = 0.1,
                                             "cone-angle" = 90,
                                             "dist-weight" = 0,
                                             "counter-nr" = 500,
                                             "households-per-cell" = 1,
                                             "setup-model" = "\"agricultural-area\"",
                                             "number-of-households" = 300,
                                             "number-of-villages" = 93,
                                             "proportion-agricultural-area" = 0.4,
                                             "hh-area-distribution" = "\"log-normal\"",
                                             "hh-area-mean-ha" = 1.02,
                                             "hh-area-sd-ha" = 0.91,
                                             "vlg-area-distribution" = "\"normal\"",
                                             "vlg-area-mean" = 50,
                                             "vlg-area-sd" = 3,
                                             "min-distance" = 15,
                                             "occ-probability" = 0,
                                             "hh-distribution" = "\"normal\"",
                                             "hh-type-mean" = 0.56,
                                             "hh-type-sd" = 0.24,
                                             "inaccessible-area-location" = "\"road-connected\"",
                                             "inaccessible-area-distribution" = "\"normal\"",
                                             "inaccessible-area-fraction" = 0.0,
                                             "inacc-area-mean" = 0.5,
                                             "inacc-area-sd" = 19,
                                             "field-size-distribution" = "\"log-normal\"",
                                             "field-size-mean-ha" = 0.49,
                                             "field-size-sd-ha" = 0.77,
                                             "use-field-size-percentage?" = "false",
                                             "field-size-percentage" = 0,
                                             "field.shape.factor" = 1,
                                             "s1.homebase" = "true",
                                             "s2.fields" = "true",
                                             "s3.nearby" = "true",
                                             "s4.avoid" = "true",
                                             "set-field-strategies-by-id?" = "false",
                                             "field-strategies-id" = 1,
                                             "change-strategy" = 2,
                                             "LUT-1-name" = "\"oilpalm\"",
                                             "LUT-1-fraction" = 0.2,
                                             "LUT-1-specialize" = 0,
                                             "LUT-2-name" = "\"rubber\"",
                                             "LUT-2-fraction" = 0.8,
                                             "LUT-2-specialize" = 0,
                                             "LUT-3-name" = "\"na\"",
                                             "LUT-3-fraction" = 0,
                                             "LUT-3-specialize" = 0,
                                             "LUT-4-name" = "\"na\"",
                                             "LUT-4-fraction" = 0,
                                             "LUT-4-specialize" = 0,
                                             "LUT-5-name" = "\"na\"",
                                             "LUT-5-fraction" = 0,
                                             "LUT-5-specialize" = 0,
                                             "LUT-fill-up" = "\"LUT-1-fraction\"",
                                             "land-use-types" = "\"landscape-level-fraction\"",
                                             "default.maps" = "\"field-patches\"",
                                             "paint-cells" = "\"id\"",
                                             "label-cells" = "\"\"",
                                             "paint-homebase-cells?" = "false",
                                             "paint-road-cells?" = "false",
                                             "show-households?" = "false",
                                             "write.param.file?" = "false",
                                             "write-household-ids" = "\"only-first-households\"",
                                             "foldername" = "\"output2\"",
                                             "apply-gis-envelope?" = "false",
                                             "gis-envelope" = "\"[238244.58 243244.58 9789775.28 9794775.28]\"",
                                             "apply-projection?" = "false",
                                             "projection-file" = "\"input/LGraf_roadmaps/jambi1_road.prj\""
                                             ))
eval_variables_constants(nl)


nl@simdesign <- simdesign_simple(nl, nseeds=1)
print(nl)

run_nl_all(nl)
