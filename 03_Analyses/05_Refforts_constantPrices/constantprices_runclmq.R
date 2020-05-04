
## We want to check at which prices, people switch:
## We run this with complete heterogeneity:

## First we load Refforts and derive min and max prices:

library(Refforts)
library(tidyverse)
library(raster)
library(landscapemetrics)
library(clustermq)

#op.min <- round(min(prices.wb$oilpalm))
#op.max <- 100 #round(max(prices.wb$oilpalm))
#rb.min <- round(min(prices.wb$rubber))
#rb.max <- round(max(prices.wb$rubber) / 3)

op.min <- round(mean(prices.wb$oilpalm) - sd(prices.wb$oilpalm))
op.max <- round(mean(prices.wb$oilpalm) + sd(prices.wb$oilpalm))
rb.min <- round(mean(prices.wb$rubber) - sd(prices.wb$rubber))
rb.max <- round(mean(prices.wb$rubber) + sd(prices.wb$rubber))

steps <- 10
op.int <- (op.max - op.min) / (steps - 1)
op <- seq(op.min, op.max, by=op.int)
rb.int <- (rb.max - rb.min) / (steps - 1)
rb <- seq(rb.min, rb.max, by=rb.int)


library(nlrx)

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
nl@simdesign <- simdesign_ff(nl, nseeds=5)
print(nl)

#####################################
## copy model files to server:
hpc.upload(from=file.path(getwd(), "01_EFForTS-ABM"),
           to=file.path(netlogopath, "app/models/"),
           user="jsaleck",
           host="login.gwdg.de",
           key="/home/jan/.ssh/id_rsa")
#####################################


#####################################
## Prepare jobs and execute on the HPC:
njobs <- nrow(nl@simdesign@siminput) * length(nl@simdesign@simseeds)
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
                        template = list(job_name = "abm_prices_v1", # define jobname
                                        log_file = "abm_prices.log", # define logfile name
                                        queue = "medium",  # define HPC queue
                                        service = "normal", # define HPC service
                                        walltime = "04:00:00", # define walltime
                                        mem_cpu = "4000")) # define memory per cpu   

#####################################
#####################################
### collect results from remote HPC:
hpc.download(from=file.path(nl@experiment@outpath),
             to=file.path(getwd(), "ssh_download"),
             user="jsaleck",
             host="login.gwdg.de",
             key="/home/jan/.ssh/id_rsa")

## Store the folder destination:
nl.results.raw <- file.path(getwd(), "ssh_download", "output")
#####################################

#####################################
## Either load results from ssh_download, or bind list from Q function is successful:
# Either:
results <- purrr::map_dfr(list.files(nl.results.raw, pattern = "rds", full.names = TRUE), function(x) {
  res.x <- readRDS(x)
  return(res.x)
})
# Or:
results <- dplyr::bind_rows(results)
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

results <- results %>% left_join(results.lsm)

## Attach output:
setsim(nl, "simoutput") <- results
saveRDS(nl, file = file.path("03_Analyses/constantprices_ff_ineff.rds"))


