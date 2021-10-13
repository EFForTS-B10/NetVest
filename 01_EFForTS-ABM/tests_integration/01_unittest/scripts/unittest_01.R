#### Unit test for Natcap Invest Integration ####
## specify experiment and adapt netlogopath, modelpath, outpath and netlogoversion

library(nlrx)
library(raster)
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

#### Validation of results: Comparison of expected result  and result quality_c_test1.asc ####
## Input
#lulc <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/input/lulc.asc" ,sep=""))
#plot(lulc)
#op <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/input/oilpalm_c.asc" ,sep=""))
#plot(op)
#rb <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/input/rubber_c.asc" ,sep="")) 
#plot(rb)

## InVEST result
investmap <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/quality_c_", experiment, ".asc" ,sep=""))
# Creating dataframe of rasterdata
#investmap_df <- as.data.frame(investmap, xy = TRUE)
# Plot raster
#str(investmap_df)
#res(investmap)
#plot(investmap)

## Expected result
expectedmap <- raster(ncol=100, nrow= 100, xmn=212461, xmx=217461, ymn=9753255, ymx=9758255)
projection(expectedmap) <- "+proj=utm +zone=48 +south +datum=WGS84"
values(expectedmap) <- 1

## Comparison
invest_vs_expected <- function(investmap=investmap,expectedmap=expectedmap){
 x <- cellStats(investmap, mean)
 y <- cellStats(expectedmap, mean)
 if (x == y) {print ("Unittest validated")}  else {"Unittest not validated"}
}

invest_vs_expected(investmap,expectedmap)  

# additional possible steps to analyze results via NLRX - not required for unit test
# Attach results to nl object:
# setsim(nl, "simoutput") <- results
# Write output to outpath of experiment within nl
# write_simoutput(nl)
# Do further analysis:
# analyze_nl(nl)

