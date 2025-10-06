############################################
##### Unittest for NL-ncINV: no impact ####
############################################

# specify experiment and create folder named after experiment with one input folder and one output folder nested
# specify hsc (half-saturation-constant), default: 0.5
# Input folder has to include sensitivitytable.txt, impacttable.txt, lulc.asc, oilpalm_c.asc and rubber_c.asc 
# adapt netlogopath, modelpath, outpath and netlogoversion

#needed libraries
library(nlrx)
library(raster)
library(ggplot2) 
library(patchwork)
library(ggpubr)

# Set JAVA_HOME according to your needs
# (see details: https://github.com/ropensci/nlrx/issues/32#issuecomment-555475538)
Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-11-openjdk-amd64")


### 1) Unittest execution
experiment <- "noimpact"
invtest <- paste("\"",experiment,"\"",sep="")
hsc <- 0.5
netlogopath <- file.path("{HOME}/netlogo/6.2.1")
modelpath <- file.path("{HOME}/NetVest/EFForTS-ABM.nlogo")
outpath <- file.path(paste("{HOME}/NetVest/tests_integration/01_unittest/",experiment,"/output",sep=""))
netlogoversion <- "6.2.1"

nl <- nl(nlversion = netlogoversion,
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nl@experiment <- experiment(expname=experiment,
                            outpath=outpath,
                            repetition=1,
                            tickmetrics="false",
                            idsetup="unittest-biodiv-ncinv",
                            idgo="do-nothing",
                            runtime=1,
                            variables = list(),
                            constants = list("ncinv_test"=invtest,
                                             "biodiv_ncinv_k"=hsc))

nl@simdesign <- simdesign_simple(nl=nl,
                                 nseeds=1)

eval_variables_constants(nl)
print(nl)


results <- run_nl_one(nl, seed = 1, siminputrow = 1)

###############################################################################
### 2) Validation of results

## Aim 1: Correct execution of unittest: [Success] [Success]
##        Correct file in output folder
fileexist <- function(qualitymap){
  qualitymap <- qualitymap
  print ("Checking existence of habitat-quality map in output folder")
  if (qualitymap == TRUE) {print ("Habitat-quality map exists")}  else {print ("Habitat-quality map is missing")}
}

fileexist(qualitymap=file.exists((paste(outpath,"/quality_c_", experiment, ".asc" ,sep=""))))

## Aim 2: Comparison of expected result with result of InVEST
## Generation of expected result
expectedmap <- raster(ncol=100, nrow= 100, xmn=212461, xmx=217461, ymn=9753255, ymx=9758255)
projection(expectedmap) <- "+proj=utm +zone=48 +south +datum=WGS84"
values(expectedmap) <- 1

# Comparison
validation_maps <- function(investmap,expectedmap){
  equalmaps <- all.equal(investmap, expectedmap)
  print ("Comparison of expected result and result of InVEST")
  if (equalmaps == TRUE) {print ("Validating expected result")}  else {print ("Unexpected result")}
}

validation_maps(investmap=raster(paste(outpath,"/quality_c_", experiment, ".asc" ,sep="")),
                expectedmap=expectedmap)

###############################################################################
### 3) Plotting

# quality_c_noimpact.asc
invest <- raster(paste(outpath,"/quality_c_", experiment, ".asc" ,sep=""))
invest_df <- as.data.frame(invest, xy=TRUE)

mycol_quality <- "#00441b"

invest_map <- ggplot(data=invest_df) + 
          geom_raster(aes(x=x,y=y,fill=factor(quality_c_noimpact)))+  
          scale_fill_manual(values = mycol_quality, name="Habitat-Quality Scores") +
          xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("A") +
          theme (plot.title = element_text(hjust = 0.1, size = 10),
                 axis.title.x = element_text(hjust=1, vjust= -1, size = 10),
                 axis.title.y = element_text(hjust=1, vjust= 2, size = 10),
                 axis.text = element_text(size = 5),
                 axis.ticks = element_line(size = 0),
                 legend.title = element_text(size = 10, face = "bold"),
                 legend.text = element_text(size = 10),
                 legend.key.width = unit(0.3, "cm"),
                 legend.key.height = unit(0.3, "cm"),
                 panel.background = element_rect(fill = "white"),
                 panel.grid.major = element_line(color = "gray90", size = 0.2),
                 panel.grid.minor = element_line(color = "gray90", size = 0.2),
                 aspect.ratio = 1,
                 plot.margin = margin(c(5,-25,5,1)),
                 legend.spacing.x = unit(0.2, "cm"),
                )

# expected result
expected_df <- as.data.frame(expectedmap, xy=TRUE)

expected_map <-  ggplot(data=expected_df) + 
             geom_raster(aes(x=x,y=y,fill=factor(layer))) + 
             scale_fill_manual(values = mycol_quality, name="Habitat-Quality Scores") +
             xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("B") +
             theme (plot.title = element_text(hjust = 0.1, size = 10),
             axis.title.x = element_text(hjust=1, vjust= -1, size = 10),
             axis.title.y = element_text(hjust=1, vjust= 2, size = 10),
             axis.text = element_text(size = 5),
             axis.ticks = element_line(size = 0),
             legend.title = element_text(size = 10, face = "bold"),
             legend.text = element_text(size = 10),
             legend.key.width = unit(0.3, "cm"),
             legend.key.height = unit(0.3, "cm"),
             panel.background = element_rect(fill = "white"),
             panel.grid.major = element_line(color = "gray90", size = 0.2),
             panel.grid.minor = element_line(color = "gray90", size = 0.2),
             aspect.ratio = 1,
             plot.margin = margin(c(5,1,5,-25)),
             legend.spacing.x = unit(0.2, "cm"),
            )           

invest_map + expected_map + plot_layout(guides = 'collect')& theme(legend.position = 'bottom')

ggsave("quality_comp_forest.png", path = "{HOME}/NetVest/tests_integration/01_unittest/Plots/" )
