library(Refforts)
library(testthat)
library(nlrx)



######################################
## Setup nl object:
#netlogopath <- file.path("~/NetLogo 6.1.1")
netlogopath <- file.path("/home/ecomod/NetLogo 6.1.1")
netlogoversion <- "6.1.1"


if (file.exists(netlogopath)){
  print('exists')
}else{
  stop('Please specify the folder that contains Netlogo')
}

modelpath <- file.path("../EFForTS-ABM.nlogo")#/EFForTS-ABM/01_EFForTS-ABM/
outpath <- file.path("output")#/EFForTS-ABM/01_EFForTS-ABM/tests/

nl <- nl(nlversion = "6.1.1",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)


nl@experiment <- experiment(expname="test",
                           outpath=outpath,
                           repetition=1,
                           tickmetrics="true",
                           idsetup="setup-with-external-maps",
                           idgo="test-invest",
                           idrunnum = "idrunnum",
                           idfinal = "write-lut-map",
                           runtime=1,
                           metrics=c(get.abm.metrics()),
                           constants = get.abm.defaults())

nl <- set.nl.constant(nl, "biodiv_invest_objective", "general ")


## Add simple simdesign
nl@simdesign <- simdesign_simple(nl, nseeds=1)
print(nl)

## Run simulations:
results <- run_nl_all(nl)

## Attach output:
setsim(nl, "simoutput") <- results



## Result tests:

#testthat::test_that( habitat quality, ueberall 1)
