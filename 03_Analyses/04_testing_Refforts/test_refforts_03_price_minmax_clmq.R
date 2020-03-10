
## We want to check at which prices, people switch:
## We run this with complete heterogeneity:

## First we load Refforts and derive min and max prices:

library(Refforts)

op.min <- round(min(prices.wb$oilpalm))
op.max <- round(max(prices.wb$oilpalm))
rb.min <- round(min(prices.wb$rubber))
rb.max <- round(max(prices.wb$rubber))

steps <- 40
op.int <- (op.max - op.min) / (steps - 1)
op <- seq(op.min, op.max, by=op.int)
rb.int <- (rb.max - rb.min) / (steps - 1)
rb <- seq(rb.min, rb.max, by=rb.int)


library(nlrx)

netlogopath <- file.path("/home/uni08/jsaleck/NetLogo_6.1.0")
modelpath <- file.path(netlogopath, "app/models/01_EFForTS-ABM/EFForTS-ABM.nlogo")
outpath <- file.path(netlogopath, "app/models/01_EFForTS-ABM")

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

#####################################
#####################################


library(clustermq)
njobs <- nrow(nl@simdesign@siminput) * length(nl@simdesign@simseeds)
siminputrows <- rep(seq(1:nrow(nl@simdesign@siminput)), length(nl@simdesign@simseeds))
rndseeds <- rep(nl@simdesign@simseeds, each=nrow(nl@simdesign@siminput))

simfun <- function(nl, siminputrow, rndseed, writeRDS=FALSE)
{
  unixtools::set.tempdir("/home/uni08/jsaleck/tmp")
  library(nlrx)
  res <- run_nl_one(nl = nl, siminputrow = siminputrow, seed = rndseed)
  if (isTRUE(writeRDS))
  {
    saveRDS(res, file=file.path(nl@experiment@outpath, paste0("nlrx_sim_", siminputrow, "_seed_", rndseed, ".rds")))
  }
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
                                        log.file = "abm_prices.log", # define logfile name
                                        queue = "medium",  # define HPC queue
                                        service = "normal", # define HPC service
                                        walltime = "00:60:00", # define walltime
                                        mem_cpu = "4000")) # define memory per cpu   

# The Q function reports the individual results of each job as a list
# Thus, we convert the list results to tibble format:
results_t <- dplyr::bind_rows(results)

## Attach output:
setsim(nl, "simoutput") <- results
saveRDS(nl, file = file.path(outpath, "sims_constantprices_ff.rds"))



results_50 <- results %>% dplyr::filter(`[step]` == 49)

ggplot(results_50, aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=lut1.fraction)) +
  geom_tile() +
  scale_fill_viridis_c()



mean.prices <- series.cl.mu %>% 
  dplyr::group_by(series, crop) %>% 
  summarise(price.mu = mean(price)) %>%
  ungroup() %>% 
  mutate(folder = paste0("GBM/", crop, "_prices_", series)) %>% 
  dplyr::select(folder, price.mu)

results_50 <- results_50 %>% 
  left_join(mean.prices, by=c("LUT-0-folder" = "folder")) %>% 
  rename(op.price.mu = price.mu)

results_50 <- results_50 %>% 
  left_join(mean.prices, by=c("LUT-1-folder" = "folder")) %>% 
  rename(rb.price.mu = price.mu)


ggplot(results_50, aes(x=hh.consumption.mean, y=p.sar, size=op.price.mu, color=rb.price.mu)) +
  geom_point() +
  scale_color_viridis_c()

ggplot(results_50, aes(x=hh.consumption.mean, y=lut0.carbon, size=op.price.mu, color=rb.price.mu)) +
  geom_point() +
  ylab("oilpalm carbon") +
  scale_color_viridis_c()

ggplot(results_50, aes(x=hh.consumption.mean, y=lut1.carbon, size=op.price.mu, color=rb.price.mu)) +
  geom_point() +
  ylab("rubber carbon") +
  scale_color_viridis_c()

