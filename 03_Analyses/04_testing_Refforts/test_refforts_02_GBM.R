

### Price data generation:
library(Refforts)
library(tidyverse)

## generate n series:
set.seed(123)
years <- 52
crops <- c("rubber", "oilpalm")
n.series <- 10
price.series <- purrr::map_dfr(1:n.series, function(x){
  prices.GBM.one.series(years=years, id=x)
})

# plot:
ggplot(price.series, aes(x=time, y=price, color=factor(series))) +
  facet_wrap(~crop, scales="free") +
  geom_line() +
  guides(color="none") +
  scale_y_log10() +
  theme_minimal()


#### CLUSTER SERIES:
# get list of cluster objects for each crop:
clusterlist <- prices.GBM.cluster(price.series, k=3)
# add cluster ids to series data:
series.cl <- prices.GBM.cluster.series(price.series, clusterlist)

# plot cluster:
ggplot(series.cl, aes(x=time, y=price, color=factor(series))) +
  facet_grid(crop~cluster, scales="free") +
  geom_line() +
  guides(color="none") +
  scale_y_log10() +
  theme_minimal()

## convert month to years:
series.cl.mu <- series.cl %>% 
  dplyr::mutate(year = rep(rep(1:years, each = length(crops)*12), length(unique(series.cl$series)))) %>% 
  group_by(series, crop, year) %>% 
  summarise(price = mean(price))


# Define folder of abm:
abmfolder <- "01_EFForTS-ABM"

# Loop, create skeletons, modify and write to disk:
for(i in crops)
{
  for(j in unique(series.cl.mu$series))
  {
    ## current prcie series
    series.ij <- series.cl.mu %>% dplyr::filter(crop == i, series == j)
    main.prices <- paste0("[", paste0(series.ij$price, collapse=" "), "]")
    
    ## write to main
    main <- get.abm.main.default(crop = i)
    main[["prices"]] <- main.prices
    
    ## define default management
    management <- get.abm.management.default(crop=i, n=1)
    
    # Write to abmfolder:
    param.set.name <- paste0("GBM/", i, "_prices_", j)
    set.abm.parfiles(abmfolder, param.set.name, main, management, overwrite = TRUE)
    
  }
}
  
param.sets.rubber <- paste0("\"GBM/", crops[1], "_prices_", unique(series.cl.mu$series), "\"")
param.sets.oilpalm <- paste0("\"GBM/", crops[2], "_prices_", unique(series.cl.mu$series), "\"")

library(nlrx)

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
                      varname = "LUT-0-folder", 
                      values = param.sets.oilpalm)                            

nl <- set.nl.variable(nl = nl, 
                      varname = "LUT-1-folder", 
                      values = param.sets.rubber)     


nl <- set.nl.constant(nl, "which-map", "\"landmarkets1\"")


# Then add a full factorial design:
nl@simdesign <- simdesign_ff(nl, nseeds=1)

## Run simulations:
library(future)
plan(multisession)
results <- run_nl_all(nl, split=10)

## Attach output:
setsim(nl, "simoutput") <- results
saveRDS(nl, file = file.path(outpath, "sims_prices_GBM.rds"))

# remove step 0:
results_1 <- results %>% dplyr::filter(`[step]` > 0)
setsim(nl, "simoutput") <- results_1

timeplot <- doplot.abm.timeseries(nl, metrics=nl@experiment@metrics)

timeplot
plotly::ggplotly(timeplot)


results_1$hh.consumption.mean
ggplot(results_1, aes(x=`[step]`, y=p.sar)) +
         facet_grid(`LUT-0-folder`~`LUT-1-folder`) +
         geom_line()




## last tick:

results_50 <- results %>% dplyr::filter(`[step]` == 49)

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




results_50$


#  right_join(results_50, by=c("folder" = "LUT-0-folder"))




dplyr::filter(series.cl.mu, series==2 & crop == "oilpalm") %>% 
  rbind(dplyr::filter(series.cl.mu, series==1 & crop == "rubber")) %>% 
  ggplot(., aes(x=year, y=price)) +
  facet_wrap(~crop, scales="free") +
  geom_line()
