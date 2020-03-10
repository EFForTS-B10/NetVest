
## We want to check at which prices, people switch:
## We run this with complete heterogeneity:

## First we load Refforts and derive min and max prices:

library(Refforts)
library(nlrx)
library(future)
library(future.batchtools)
library(debugme)
Sys.setenv(DEBUGME='batchtools')
library(batchtools)

## Price vectors:
op.min <- round(min(prices.wb$oilpalm))
op.max <- round(max(prices.wb$oilpalm))
rb.min <- round(min(prices.wb$rubber))
rb.max <- round(max(prices.wb$rubber))

steps <- 2
op.int <- (op.max - op.min) / (steps - 1)
op <- seq(op.min, op.max, by=op.int)
rb.int <- (rb.max - rb.min) / (steps - 1)
rb <- seq(rb.min, rb.max, by=rb.int)


## set up nl object:
netlogopath <- file.path("C:/Program Files/NetLogo 6.1.0")
modelpath <- "01_EFForTS-ABM/EFForTS-ABM.nlogo"
outpath <- "03_Analyses/"

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


## Run simulations:
options(future.makeNodePSOCK.rshopts = c("-i", "/home/jan/.ssh/id_rsa"))
login <- tweak(remote, workers="gwdu101.gwdg.de", user="jsaleck")

## Define plan for future environment:
sbatch <- tweak(batchtools_slurm, template = 'future_slurm.tmpl',
              resources = list(job_name = "abm_prices_v1",
                               n_cpu = 1,
                               queue = "medium",
                               service = "normal",
                               walltime = "02:00:00",   #12:00
                               mem_cpu = "4000",   # mb RAM per core
                               log_file = "abm_prices.log"))   #8

plan(list(login,
          sbatch,
          multisession))

results %<-% run_nl_all(nl, split=nrow(nl@simdesign@siminput))

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

