## Example 05: EFForTS-ABM clustermq

#####################################
## Load libraries:
library(Refforts)
library(tidyverse)
library(clustermq)
library(nlrx)

set.seed(6835624)
#####################################
## Setup nl object:
netlogopath <- file.path("/home/dockerj/nl")
netlogoversion <- "6.1.1"
modelpath <- file.path(netlogopath, "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/EFForTS-ABM.nlogo")
outpath <- file.path(netlogopath, "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/output")

nl <- nl(nlversion = netlogoversion,
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nl@experiment <- experiment(expname="test",
                            outpath=outpath,
                            repetition=1,
                            tickmetrics="true",
                            idsetup="setup-with-external-maps",
                            idgo="go",
                            idrunnum = "idrunnum",
                            idfinal = "write-lut-map",
                            runtime=50,
                            metrics=c(get.abm.metrics(),
                                      "abandoned.land",
                                      "lm_new",
                                      "lm.seller.wealth", "lm.buyer.wealth",
                                      "lm.seller.area", "lm.buyer.area",
                                      "lm.seller.lut0.ineff", "lm.buyer.lut0.ineff",
                                      "lm.seller.lut1.ineff", "lm.buyer.lut1.ineff"),
                            constants = get.abm.defaults())

## Use Refforts to set variables:
nl <- set.nl.variable(nl = nl,
                      varname = "LUT-0-price",
                      values = c(80,90,100))

nl <- set.nl.variable(nl = nl,
                      varname = "LUT-1-price",
                      values = c(700,800,900))

## Use Refforts to set some constants
nl <- set.nl.constant(nl, "which-map", "\"landmarkets1\"")
nl <- set.nl.constant(nl, "price_scenario", "\"constant_prices\"")
nl <- set.nl.constant(nl, "heterogeneous-hhs?", "true")
nl <- set.nl.constant(nl, "learning-spillover?", "true")
nl <- set.nl.constant(nl, "setup-hh-network", "\"hh-nw-distance\"")
nl <- set.nl.constant(nl, "hh-nw-param1", 20)
nl <- set.nl.constant(nl, "min-wealth", 3000) # 3 years of base consumption!
nl <- set.nl.constant(nl, "time-horizon", 20)
nl <- set.nl.constant(nl, "buyer_pool_n", 20)
nl <- set.nl.constant(nl, "immigrant_probability", 0.25)
nl <- set.nl.constant(nl, "immigrant-wealth-factor", 10)

# Then add a full factorial design:
nl@simdesign <- simdesign_ff(nl, nseeds=3)
print(nl)

#####################################
## copy model files to server:
hpc.upload(from=file.path(getwd(), "01_EFForTS-ABM"),
           to=file.path(netlogopath, "app/models/"),
           user="jsaleck", key="/home/jan/.ssh/id_rsa")
#####################################


#####################################
## Prepare jobs and execute on the HPC:
maxjobs.hpc <- 2000
njobs <- min(nrow(nl@simdesign@siminput) * length(nl@simdesign@simseeds), maxjobs.hpc)
siminputrows <- rep(seq(1:nrow(nl@simdesign@siminput)), length(nl@simdesign@simseeds))
rndseeds <- rep(nl@simdesign@simseeds, each=nrow(nl@simdesign@siminput))

simfun <- function(nl, siminputrow, rndseed, writeRDS=FALSE)
{
  library(nlrx)
  res <- run_nl_one(nl = nl, siminputrow = siminputrow, seed = rndseed, writeRDS = writeRDS)
  return(res)
}


### RUN:
results <- clustermq::Q(fun = simfun,
                        siminputrow = siminputrows,
                        rndseed = rndseeds,
                        const = list(nl = nl,
                                     writeRDS = TRUE),
                        export = list(),
                        seed = 42,
                        n_jobs = njobs,
                        template = list(job_name = "prices", # define jobname
                                        log_file = "prices.log", # define logfile name
                                        queue = "medium",  # define HPC queue
                                        service = "normal", # define HPC service
                                        walltime = "16:00:00", # define walltime
                                        mem_cpu = "4000")) # define memory per cpu

#####################################
#####################################
### collect results from remote HPC:
hpc.download(from=file.path(nl@experiment@outpath),
             to=file.path(getwd(), "ssh_download"),
             user="jsaleck", key="/home/jan/.ssh/id_rsa")

## Store the folder destination:
nl.results.raw <- file.path(getwd(), "ssh_download", "output")

## Delet files on HPC:
hpc.del(folder = nl@experiment@outpath,
        user="jsaleck", key="/home/jan/.ssh/id_rsa")

#####################################

#####################################
## Either load results from ssh_download, or bind list from Q function is successful:
# Either:
results.sim <- purrr::map_dfr(list.files(nl.results.raw, pattern = "rds", full.names = TRUE), function(x) {
  res.x <- readRDS(x)
  return(res.x)
})
# Or:
results.sim <- dplyr::bind_rows(results)
#####################################

## Attach output:
setsim(nl, "simoutput") <- results.sim
saveRDS(nl, file = file.path("final_output.rds"))


########  ATTENTION!! THIS DELETS THE RAW FILES:
## Delete local files
if (askYesNo("Do you really want to delete local raw simulation results?")){
  file.remove(list.files(nl.results.raw, full.names = TRUE))
}

