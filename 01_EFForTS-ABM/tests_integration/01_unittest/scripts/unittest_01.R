###unit test for Natcap Invest Integration
## specify experiment

library(nlrx)
experiment <- "test1"
invtest <- paste("\"",experiment,"\"",sep="")
netlogopath <- file.path("/home/dockerj/nl")
modelpath <- file.path("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/EFForTS-ABM.nlogo")
outpath <- file.path(paste("/home/dockerj/",experiment,"/output",sep=""))
netlogoversion <- "6.1.1"


# test nlrx
# test_nlrx(nlpath=netlogopath, nlversion="6.1.1")
#
nl <- nl(nlversion = netlogoversion,
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)
nl@experiment <- experiment(expname=experiment,
                            outpath=outpath,
                            repetition=1,
                            tickmetrics="false",
                            idsetup="test-invest",
                            idgo="do-nothing",
                            runtime=1,
                            #metrics=c("edu-calc-index"),
                            variables = list(),
                            constants = list("inv-test"=invtest))

nl@simdesign <- simdesign_simple(nl=nl,
                                 nseeds=1)

eval_variables_constants(nl)
print(nl)


# results <- run_nl_one(nl, seed = 1, siminputrow = 1)
# for debuggin, keep interim output in \tmp
results <- run_nl_one(nl,seed = 1, siminputrow = 1, cleanup.csv = FALSE, cleanup.xml = FALSE, cleanup.bat = FALSE)

# additional possible steps to analyze results via NLRX - not required for unit test
# Attach results to nl object:
# setsim(nl, "simoutput") <- results
# Write output to outpath of experiment within nl
# write_simoutput(nl)
# Do further analysis:
# analyze_nl(nl)

