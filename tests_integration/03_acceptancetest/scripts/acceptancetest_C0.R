#####################################################
##### Acceptancetest for NL-InVEST: C0 Scenario #####
#####################################################

# specify experiment 
# specify hsc (half-saturation-constant), default: 0.05. Has to be adapted after first simulation to ca. 1/2 of Dmax
# Input folder of ncinv/habitatquality has to include sensitivitytable.txt, impacttable.txt  
# adapt netlogopath, modelpath, outpath and netlogoversion

#needed libraries
library(nlrx)
library(Refforts) 

# Set JAVA_HOME according to your needs
# (see details: https://github.com/ropensci/nlrx/issues/32#issuecomment-555475538)
Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-11-openjdk-amd64")

### 1) Acceptancetest execution
experiment <- "C0"
invtest <- paste("\"",experiment,"\"",sep="")
natcapinvestexperiment <- invtest
hsc <- 0.05
netlogopath <- file.path("{HOME}/netlogofolder6.2.1")
modelpath <- file.path("{HOME}/EFForTS-ABM/01_EFForTS-ABM/EFForTS-ABM.nlogo")
outpath <- file.path("{HOME}/EFForTS-ABM/01_EFForTS-ABM/ncinv/habitatquality/output")
netlogoversion <- "6.2.1"

nl <- nl(nlversion = netlogoversion,
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nl@experiment <- experiment(expname=experiment,
                            outpath=outpath,
                            repetition=20,
                            tickmetrics="true",
                            idsetup=c("ca","setup-with-external-maps"),
                            idgo=c("go", "update-time"),
                            idrunnum = "idrunnum",
                            runtime=51,
                            metrics= get.abm.metrics(),
                            constants = get.abm.defaults())

nl <- set.nl.constant(nl, "heterogeneous-hhs?", "false")
nl <- set.nl.constant(nl, "learning-spillover?", "false")
nl <- set.nl.constant(nl, "ncinv_test", invtest)
nl <- set.nl.constant(nl, "biodiv_ncinv_k", hsc)
nl <- set.nl.constant(nl, "ncinv_experiment", natcapinvestexperiment)
nl <- set.nl.constant(nl, "sim-time", 51)
nl <- set.nl.constant(nl, "write-maps?", TRUE)

nl@simdesign <- simdesign_simple(nl=nl,
                                 nseeds=1)

eval_variables_constants(nl)
print(nl)

results <- run_nl_all(nl)

## Attach output:
setsim(nl, "simoutput") <- results

##Write to csv
write_simoutput(nl) 
