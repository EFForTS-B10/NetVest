
## We want to check at which prices, people switch:
## We run this with complete heterogeneity:

## First we load Refforts and derive min and max prices:

library(Refforts)
library(tidyverse)

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

netlogopath <- file.path("C:/Program Files/NetLogo 6.1.0")
modelpath <- file.path("01_EFForTS-ABM/EFForTS-ABM.nlogo")
outpath <- file.path("01_EFForTS-ABM/output")

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
                            runtime=3,
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
nl@simdesign <- simdesign_simple(nl, nseeds=1)
print(nl)

res <- run_nl_all(nl)
