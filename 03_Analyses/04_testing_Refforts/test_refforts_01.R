


### test the new refforts package:
library(Refforts)

# Define folder of abm:
abmfolder <- "01_EFForTS-ABM"

# Generate vectors for dynamic creation of parameter sets:
crops <- c("rubber", "oilpalm")
wages <- seq(1, 2, 0.1)

# Loop, create skeletons, modify and write to disk:
for(i in crops)
{
  main <- get.abm.main.default(crop = i)
  management <- get.abm.management.default(crop=i, n=1)
  
  for(j in wages)
  {
    # increase wages of management:
    management[[1]][["wages"]] <- j
    
    # Write to abmfolder:
    param.set.name <- paste0(i, "_wages_", j)
    set.abm.parfiles(abmfolder, param.set.name, main, management, overwrite = TRUE)
  }
}


param.sets.rubber <- paste0("\"", crops[1], "_wages_", wages, "\"")
param.sets.oilpalm <- paste0("\"", crops[2], "_wages_", wages, "\"")

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
nl@simdesign <- simdesign_distinct(nl, nseeds=1)

## Run simulations:
library(future)
plan(multisession)
results <- run_nl_all(nl, split=11)

## Attach output:
setsim(nl, "simoutput") <- results
saveRDS(nl, file = file.path(outpath, "sims_wages_distinct.rds"))

# remove step 0:
results_1 <- results %>% dplyr::filter(`[step]` > 0)
setsim(nl, "simoutput") <- results_1

timeplot <- doplot.abm.timeseries(nl, metrics=nl@experiment@metrics)

timeplot
plotly::ggplotly(timeplot)
