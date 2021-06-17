library(Refforts)
library(testthat)
library(nlrx)

## Set R random seed
set.seed(457348) # we dont need a seed, but util_gather_results(nl, outfile, seed, siminputrow) does


#to fix Temporary output file not found
#dir.create(t <- paste(tempdir(), Sys.getpid(), sep='-'), FALSE, TRUE, "0700")
#unixtools::set.tempdir(t)
#message(tempdir())

######################################
## Setup nl object:


netlogopath <- file.path("/usr/users/beyer35/nl")



#netlogopath <- file.path("/home/ecomod/nl")

#netlogopath <- file.path("/usr/users/beyer35/nl")
#netlogopath <- file.path("/usr/users/henzler1/nl")
#netlogopath <- file.path("/home/julia/netlogofolder")

netlogoversion <- "6.1.1"


if (file.exists(netlogopath)){
  print('netlogopath exists')
}else{
  stop('Please specify the folder that contains Netlogo')
}
#eigentlich solltest du hier nicht /home/julia davor schreiben muessen, da das dein working directory fuer Rstudio ist
modelpath <- file.path("01_EFForTS-ABM/EFForTS-ABM.nlogo")#/home/julia/

if (file.exists(modelpath)){
  print('modelpath exists')
}else{
  stop('Please specify the folder that contains the model')
}
#hier genauso
outpath <- file.path(".") #/home/julia/EFForTS-ABM/01_EFForTS-ABM/tests/output

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
                           repetition=1,
                           tickmetrics="true",
                           idsetup="test-setup", #setup-with-external-maps #test-invest
                           idgo="do-nothing",#test-invest #go-biodiversity
                           idrunnum = "idrunnum",
                           idfinal = "do-nothing",#write-lut-map
                           runtime=1,
                           #metrics=c(get.abm.metrics()),
                           constants = get.abm.defaults())


nl <- set.nl.constant(nl, "biodiv_invest_objective", "\"general\"")
#nl <- set.nl.constant(nl, "which-machine?", "\"server\"")
nl <- set.nl.constant(nl, "which-machine?", "\"local-linux\"")


## Add simple simdesign
nl@simdesign <- simdesign_simple(nl, nseeds=1)
#print(nl)



## Run simulations:
message('starting netlogo simulation')
results <- run_nl_all(nl)
message('finished netlogo simulation')
## Attach output:
#setsim(nl, "simoutput") <- results

write_simoutput(nl, outpath = "01_EFForTS-ABM/tests/output")


## Result tests:

#testthat::test_that( habitat quality, ueberall 1)
