
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
## Load prices from historic data from NetLogo parameter folders:
prices <- tibble(year=1:52,
                 rubber=as.numeric(strsplit("936.2777856 868.6767338 885.5715161 797.7484154 654.2350013 646.4374475 806.4138059 614.6540206 477.3383328 436.5650795 769.280586 699.7738209 470.2251997 640.7769637 624.1525409 649.7782415 745.8799753 765.3942667 602.6007523 474.4264919 604.6883223 556.4034636 445.4618454 411.7198385 458.67955 518.3415747 426.8280796 366.5359797 353.2880792 361.8482403 337.4771345 471.1947481 602.3906065 541.6913328 415.0078713 307.7394107 273.2871506 293.8815267 263.1991193 354.1826328 476.5637424 527.7216356 594.5683233 810.0952362 831.0592075 881.364227 697.7814902 1280.478609 1551.532108 1100 923.3988399 647.6232218", " ")[[1]]),
                 oilpalm=as.numeric(strsplit("110.4265186 117.2560383 132.1780035 110.2258817 103.6134211 78.97439936 80.41511905 108.5456568 103.5659118 79.04707803 118.4444829 172.2406675 100.6414827 93.09292488 112.279975 109.4334153 106.8430507 86.71566971 84.68391343 68.07561194 78.78228157 117.0887798 81.26734749 36.27539499 44.12632553 52.87703564 42.63976658 33.96151143 40.10286268 45.69597558 42.40370398 61.15327854 66.22869382 57.05514457 61.55720572 79.13746494 52.4229838 37.78704493 36.14812655 49.96542135 53.94628324 53.71553556 46.63382959 51.54510381 79.22715037 89.38074506 68.59332381 87.29077124 100.102921 90 78.2906484 75.17813221", " ")[[1]]),
)

## Plot prices:
plot(prices$year, prices$oilpalm, pch=16, cex=2)
plot(prices$year, prices$rubber, pch=16, cex=2)

## Derive upper and lower quantile: These will be used as price range for our full facotrial design:
op.min <- round(quantile(prices$oilpalm)[2])
op.max <- round(quantile(prices$oilpalm)[4])
rb.min <- round(quantile(prices$rubber)[2])
rb.max <- round(quantile(prices$rubber)[4])

# Set general information
n.random.seeds <- 5
price.interval.steps <- 5 # 50
op.int <- (op.max - op.min) / (price.interval.steps - 1)
op <- seq(op.min, op.max, by=op.int)
rb.int <- (rb.max - rb.min) / (price.interval.steps - 1)
rb <- seq(rb.min, rb.max, by=rb.int)


# Generate vectors for dynamic creation of parameter sets:
crops <- c("rubber", "oilpalm")
p.rubber <- c("\"rubber_wages_1\"", "\"rubber_wages_2\"")
p.oilpalm <- c("\"oilpalm_wages_2\"", "\"oilpalm_wages_2\"")


#############
## Now we have 4 vectors with variable parameters:
# op, rb -> vectors with prices (10)
# p.oilpalm, p.rubber -> with parameter folders (11)
# 
# However, we do not want to have a full facotiral design exploring each difference with each other
# Instead we want to:
# 1. have all price combinations in full factorial
# 2. repeat that distinctively with each p.oilpalm/p.rubber folder
rb.distinct <- rep(expand.grid(rb,op)$Var1, length(p.rubber))
op.distinct <- rep(expand.grid(rb,op)$Var2, length(p.rubber))
p.rubber.distinct <- rep(p.rubber, each=length(expand.grid(rb,op)$Var1))
p.oilpalm.distinct <- rep(p.oilpalm, each=length(expand.grid(rb,op)$Var1))

## Setup nl object:
#netlogopath <- file.path("/home/uni08/jsaleck/NetLogo_6.1.0")
#modelpath <- file.path(netlogopath, "app/models/01_EFForTS-ABM/EFForTS-ABM.nlogo")
#outpath <- file.path(netlogopath, "app/models/01_EFForTS-ABM/output")
# local:
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
                            idrunnum = "idrunnum",
                            idfinal = "write-lut-map",
                            runtime=50,
                            metrics=c(get.abm.metrics(), 
                                      "abandoned.land", 
                                      "lm_new",
                                      "lm.seller.wealth", "lm.buyer.wealth",
                                      "lm.seller.area", "lm.buyer.area",
                                      "lm.seller.lut0.ineff", "lm.buyer.lut0.ineff",
                                      "lm.seller.lut1.ineff", "lm.buyer.lut1.ineff",
                                      "hh.lut0.ineff.mean", "hh.lut1.ineff.mean"),
                            constants = get.abm.defaults())


nl <- set.nl.variable(nl = nl, 
                      varname = "LUT-0-price", 
                      values = op.distinct)                            

nl <- set.nl.variable(nl = nl, 
                      varname = "LUT-1-price", 
                      values = rb.distinct)     

nl <- set.nl.variable(nl = nl, 
                      varname = "LUT-0-folder", 
                      values = p.oilpalm.distinct)                            

nl <- set.nl.variable(nl = nl, 
                      varname = "LUT-1-folder", 
                      values = p.rubber.distinct)     

nl <- set.nl.constant(nl, "which-map", "\"landmarkets2\"")
nl <- set.nl.constant(nl, "price_scenario", "\"constant_prices\"")
nl <- set.nl.constant(nl, "heterogeneous-hhs?", "true")
nl <- set.nl.constant(nl, "learning-spillover?", "true")
nl <- set.nl.constant(nl, "setup-hh-network", "\"hh-nw-distance\"")
nl <- set.nl.constant(nl, "invest_plantdiv?", "true")

nl <- set.nl.constant(nl, "invest-habitatquality?", "false")
nl <- set.nl.constant(nl, "research-objective", "\"generell-biodiv?\"")

nl <- set.nl.constant(nl, "hh-nw-param1", 20)
nl <- set.nl.constant(nl, "min-wealth", 30)
nl <- set.nl.constant(nl, "time-horizon", 20)
nl <- set.nl.constant(nl, "buyer_pool_n", 20)
nl <- set.nl.constant(nl, "immigrant_probability", 0.25)
nl <- set.nl.constant(nl, "immigrant-wealth-factor", 10)


# Then add a full factorial design:
nl@simdesign <- simdesign_distinct(nl, nseeds=n.random.seeds)
print(nl)

## Run simulations:
library(future)
plan(multisession)
results <- run_nl_all(nl, split=10)

## Add landscapemetrics:
ascdir <- file.path(dirname(nl@modelpath), "output")

results.lsm <- purrr::map_dfr(list.files(ascdir, pattern = "asc", full.names = TRUE), function(x) {
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
results <- results %>% left_join(results.lsm.test, by = c("siminputrow", "random-seed"))

## Attach output:
setsim(nl, "simoutput") <- results
saveRDS(nl, file = file.path(outpath, "sims_wages_policy.rds"))


