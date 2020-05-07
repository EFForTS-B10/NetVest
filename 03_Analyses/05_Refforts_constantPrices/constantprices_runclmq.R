

#####################################
# With this scenario, we want to check the influence of prices on various output metrics!
# We use constant prices scenarios
# For example, we might want to check at which price levels land-use are more prominent
# At the same time we can evaluate the effects on consumption, carbon, etc.
#
# We vary the prices for OP and rubber by defining a price interval
# This interval is derived from the mean world bank price and the standard-deviation.
# Then, we create a number of steps in between and use a full factorial design!
#
#
#####################################
## Load libraries:
library(Refforts)
library(tidyverse)
library(raster)
library(landscapemetrics)
library(clustermq)
library(nlrx)

set.seed(6835624)
#####################################

## Define price scenarios:
op.min <- round(median(prices.wb$oilpalm) - sd(prices.wb$oilpalm))
op.max <- round(median(prices.wb$oilpalm) + sd(prices.wb$oilpalm))
rb.min <- round(median(prices.wb$rubber) - sd(prices.wb$rubber))
rb.max <- round(median(prices.wb$rubber) + sd(prices.wb$rubber))

n.random.seeds <- 3 # 3
steps <- 50 # 40-50
op.int <- (op.max - op.min) / (steps - 1)
op <- seq(op.min, op.max, by=op.int)
rb.int <- (rb.max - rb.min) / (steps - 1)
rb <- seq(rb.min, rb.max, by=rb.int)

## Setup nl object:
netlogopath <- file.path("/home/uni08/jsaleck/NetLogo_6.1.0")
modelpath <- file.path(netlogopath, "app/models/01_EFForTS-ABM/EFForTS-ABM.nlogo")
outpath <- file.path(netlogopath, "app/models/01_EFForTS-ABM/output")

nl <- nl(nlversion = "6.1.0",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nl@experiment <- experiment(expname="invest",
                            outpath=outpath,
                            repetition=1,
                            tickmetrics="true",
                            idsetup="setup-with-external-maps",
                            idgo="go",
                            idrunnum = "idrunnum",
                            idfinal = "write-lut-map",
                            runtime=50,
                            metrics=get.abm.metrics(),
                            constants = get.abm.defaults())


nl <- set.nl.variable(nl = nl, 
                      varname = "LUT-0-price", 
                      values = op)                            

nl <- set.nl.variable(nl = nl, 
                      varname = "LUT-1-price", 
                      values = rb)     

nl <- set.nl.constant(nl, "which-map", "\"landmarkets1\"")
nl <- set.nl.constant(nl, "price_scenario", "\"constant_prices\"")
nl <- set.nl.constant(nl, "heterogeneous-hhs?", "true")

# Then add a full factorial design:
nl@simdesign <- simdesign_ff(nl, nseeds=n.random.seeds)
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

#####################################
#### Calculate LANDSPACE METRICS:
results.lsm <- purrr::map_dfr(list.files(nl.results.raw, pattern = "asc", full.names = TRUE), function(x) {
  x.split <- strsplit(x, "_")[[1]]
  x.tick <- as.numeric(strsplit(x.split[[length(x.split)]], "\\.")[[1]][[1]])
  x.siminputrow <- as.numeric(x.split[[length(x.split) - 1]][[1]])
  x.seed <- as.numeric(x.split[[length(x.split) - 2]][[1]])
  x.raster <- raster(x)
  
  ## Calculate landscape metrics:
  metrics <- c("lsm_l_ed", "lsm_l_shdi", "lsm_l_lsi", "lsm_l_lpi", "lsm_l_area_mn")
  x.metrics <- landscapemetrics::calculate_lsm(x.raster, what=metrics) %>% 
    dplyr::select(metric, value) %>% 
    tidyr::pivot_wider(names_from=metric, values_from = value)
  
  x.final <- tibble::tibble(siminputrow = x.siminputrow,
                            `[step]` = x.tick,
                            `random-seed` = x.seed)
  
  x.final <- cbind(x.final, x.metrics)
  return(x.final)
})
#####################################

#####################################
## Combine results with lsm and store:
results.lsm.test <- results.lsm %>% dplyr::select(-`[step]`)
results <- results.sim %>% left_join(results.lsm.test, by = c("siminputrow", "random-seed"))

## Attach output:
setsim(nl, "simoutput") <- results
saveRDS(nl, file = file.path("03_Analyses/constantprices_ff_ineff.rds"))


########  ATTENTION!! THIS DELETS THE RAW FILES:
## Delete local files  
if (askYesNo("Do you really want to delete local raw simulation results?")){
  file.remove(list.files(nl.results.raw, full.names = TRUE))  
}


