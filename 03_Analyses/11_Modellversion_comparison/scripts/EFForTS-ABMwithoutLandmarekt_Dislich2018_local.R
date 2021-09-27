## Example 05: EFForTS-ABM clustermq

#####################################
## Load libraries:
library(Refforts)
library(tidyverse)
#library(clustermq)
library(nlrx)

#set.seed(6835624)
#####################################
## Setup nl object:
netlogopath <- file.path("C:/Program Files (x86)/NetLogo 6.1.1")
netlogoversion <- "6.1.1"
modelpath <- file.path("C:/Users/JuliaHenzler/Documents/01_GitHub/EFForTS-ABM/01_EFForTS-ABM/EFForTS-ABM.nlogo")
outpath <- file.path("C:/Users/JuliaHenzler/Documents/03_EFForTS/1_Biodiversity_Submodel/Modellversion_comparison/data")

nl <- nl(nlversion = netlogoversion,
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nl@experiment <- experiment(expname="C0_with_hhage",
                            outpath=outpath,
                            repetition=20,
                            tickmetrics="true",
                            idsetup="setup-with-external-maps",
                            idgo="go",
                            idrunnum = "idrunnum",
                            idfinal = "write-lut-map",
                            runtime=50,
                            metrics= c(get.abm.metrics(),
                                       "area_under_agriculture"),
                            constants = get.abm.defaults())

## Use Refforts to set variables:
#nl <- set.nl.variable(nl = nl,
#                      varname = "LUT-0-price",
#                      values = c(80,90,100))

#nl <- set.nl.variable(nl = nl,
#                      varname = "LUT-1-price",
#                      values = c(700,800,900))

## Use Refforts to set some constants
nl <- set.nl.constant(nl, "reproducable?", "false")
nl <- set.nl.constant(nl, "price_scenario", "\"constant_prices\"")
nl <- set.nl.constant(nl, "heterogeneous-hhs?", "false")
nl <- set.nl.constant(nl, "learning-spillover?", "false")
nl <- set.nl.constant(nl, "setup-hh-network", "\"hh-nw-kernel-distance\"")
nl <- set.nl.constant(nl, "landmarket?", "false")
nl <- set.nl.constant(nl, "rent_rate_capital_borrow", 0.1)
nl <- set.nl.constant(nl, "biodiv_plants", "\"none\"")



# Then add a simulation design:
nl@simdesign <- simdesign_simple(nl, nseeds=1)
print(nl)

#####################################
## copy model files to server:
#hpc.upload(from=file.path(getwd(), "01_EFForTS-ABM"),
#           to=file.path(netlogopath, "app/models/"),
#          user="jsaleck", key="/home/jan/.ssh/id_rsa")
#####################################


#####################################
## Prepare jobs and execute on the HPC:
#maxjobs.hpc <- 2000
#njobs <- min(nrow(nl@simdesign@siminput) * length(nl@simdesign@simseeds), maxjobs.hpc)
#siminputrows <- rep(seq(1:nrow(nl@simdesign@siminput)), length(nl@simdesign@simseeds))
#rndseeds <- rep(nl@simdesign@simseeds, each=nrow(nl@simdesign@siminput))

#simfun <- function(nl, siminputrow, rndseed, writeRDS=FALSE)
#{
# library(nlrx)
#res <- run_nl_one(nl = nl, siminputrow = siminputrow, seed = rndseed, writeRDS = writeRDS)
#return(res)
#}


### RUN ON SERVER:
results <- run_nl_all(nl)

## Attach output:
setsim(nl, "simoutput") <- results

##Write to csv
write_simoutput(nl) 

###Analyse
analyze_nl(nl) #not for simdesign_simple
saveRDS(nl, file = file.path("final_output.rds"))


########  ATTENTION!! THIS DELETS THE RAW FILES:
## Delete local files
if (askYesNo("Do you really want to delete local raw simulation results?")){
  file.remove(list.files(nl.results.raw, full.names = TRUE))
}

