library(Refforts)
library(testthat)
library(nlrx)

## Set R random seed
set.seed(457348) # we dont need a seed, but util_gather_results(nl, outfile, seed, siminputrow) does


#to fix Temporary output file not found
dir.create(t <- paste(tempdir(), Sys.getpid(), sep='-'), FALSE, TRUE, "0700")
unixtools::set.tempdir(t)
message(tempdir())

######################################
## Setup nl object:
#netlogopath <- file.path("~/NetLogo 6.1.1")
netlogopath <- file.path("/home/ecomod/NetLogo 6.1.1")
netlogoversion <- "6.1.1"


if (file.exists(netlogopath)){
  print('netlogopath exists')
}else{
  stop('Please specify the folder that contains Netlogo')
}

modelpath <- file.path("EFForTS-ABM/01_EFForTS-ABM/EFForTS-ABM.nlogo")#/EFForTS-ABM/01_EFForTS-ABM/

if (file.exists(modelpath)){
  print('modelpath exists')
}else{
  stop('Please specify the folder that contains the model')
}

outpath <- file.path("EFForTS-ABM/01_EFForTS-ABM/tests/output")#/EFForTS-ABM/01_EFForTS-ABM/tests/

if (file.exists(outpath)){
  print('outpath exists')
}else{
  stop('Please specify the folder that contains the outpath')
}


nl <- nl(nlversion = netlogoversion,
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)


nl@experiment <- experiment(expname="test",
                           outpath=outpath,
                           repetition=2,
                           tickmetrics="true",
                           idsetup="setup-with-external-maps",
                           idgo="go-biodiversity",#test-invest
                           idrunnum = "idrunnum",
                           idfinal = "write-lut-map",#
                           runtime=2,
                           metrics=c(get.abm.metrics()),
                           constants = get.abm.defaults())

nl <- set.nl.constant(nl, "biodiv_invest_objective", "general")


## Add simple simdesign
nl@simdesign <- simdesign_simple(nl, nseeds=1)
print(nl)



## Run simulations:
results <- run_nl_all(nl)

## Attach output:
setsim(nl, "simoutput") <- results

write_simoutput(nl, outpath = "EFForTS-ABM/01_EFForTS-ABM/tests/output")

## Result tests:

#testthat::test_that( habitat quality, ueberall 1)
