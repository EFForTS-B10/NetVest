
## We want to check at which prices, people switch:
## We run this with complete heterogeneity:

## First we load Refforts and derive min and max prices:

library(Refforts)
library(tidyverse)

op.min <- round(min(prices.wb$oilpalm))
op.max <- 100 #round(max(prices.wb$oilpalm))
rb.min <- round(min(prices.wb$rubber))
rb.max <- round(max(prices.wb$rubber) / 3)

steps <- 40
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
nl <- set.nl.constant(nl, "heterogeneous-hhs?", "false")


# Then add a full factorial design:
nl@simdesign <- simdesign_ff(nl, nseeds=1)
print(nl)

#####################################
## copy model files to server:
hpc.upload(from=file.path(getwd(), "01_EFForTS-ABM"),
           to=file.path(netlogopath, "app/models/"),
           user="jsaleck",
           host="login.gwdg.de",
           key="/home/jan/.ssh/id_rsa")
#####################################

library(clustermq)
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

###
### If Q was not succesful, collect results from remote HPC:

hpc.download(from=file.path(nl@experiment@outpath),
             to=file.path(getwd(), "ssh_download"),
             user="jsaleck",
             host="login.gwdg.de",
             key="/home/jan/.ssh/id_rsa")

results <- purrr::map_dfr(list.files(file.path(getwd(), "ssh_download", "output"), pattern = "rds", full.names = TRUE), function(x) {
  res.x <- readRDS(x)
  return(res.x)
})

# The Q function reports the individual results of each job as a list
# Thus, we convert the list results to tibble format:
results <- dplyr::bind_rows(results)

## Attach output:
setsim(nl, "simoutput") <- results
saveRDS(nl, file = file.path("03_Analyses/sims_constantprices_ff.rds"))

