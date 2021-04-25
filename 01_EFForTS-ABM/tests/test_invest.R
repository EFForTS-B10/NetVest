library(Refforts)
library(testthat)
library(nlrx)



######################################
## Setup nl object:
netlogopath <- file.path("../../../NetLogo 6.1.1")#/usr/users/beyer35/netlogofolder
modelpath <- file.path("../EFForTS-ABM.nlogo")#EFForTS-ABM/
outpath <- file.path("output")


nl <- nl(nlversion = "6.1.1",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)


nl@experiment <- experiment(expname="test",
                           outpath=outpath,
                           repetition=1,
                           tickmetrics="true",
                           idsetup="test-invest",
                           idgo="",
                           idrunnum = "idrunnum",
                           idfinal = "write-lut-map",
                           runtime=1,
                           metrics=c(get.abm.metrics()),
                           constants = get.abm.defaults())


## Set random-seed to reproducable
nl <- set.nl.constant(nl, "reproducable?", "true")

## Add simple simdesign
nl@simdesign <- simdesign_simple(nl, nseeds=1)
print(nl)

## Run simulations:
results <- run_nl_all(nl)

## Attach output:
setsim(nl, "simoutput") <- results



## Result tests:

#testthat::test_that( habitat quality, ueberall 1)
