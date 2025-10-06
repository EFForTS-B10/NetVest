#########################################
##### Unittest for NL-ncINV: local #####
#########################################

# specify experiment and create folder named after experiment with one input folder and one output folder nested
# specify hsc (half-saturation-constant), default: 0.5
# Input folder has to include sensitivitytable.txt, impacttable.txt, lulc.asc, oilpalm_c.asc and rubber_c.asc 
# adapt netlogopath, modelpath, outpath and netlogoversion

# needed libraries
library(nlrx)
library(raster)
library(tidyverse)
library(patchwork)

# Set JAVA_HOME according to your needs
# (see details: https://github.com/ropensci/nlrx/issues/32#issuecomment-555475538)
Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-11-openjdk-amd64")

### 1) Unittest execution
experiment <- "localimpact"
invtest <- paste("\"",experiment,"\"",sep="")
hsc <- 0.5
netlogopath <- file.path("{HOME}/netlogofolder6.2.1")
modelpath <- file.path("{HOME}/EFForTS-ABM/01_EFForTS-ABM/EFForTS-ABM.nlogo")
outpath <- file.path(paste("{HOME}/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/",experiment,"/output",sep=""))
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
                            #metrics=c("edu-calc-index"),
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

## Aim 2: Checking if transformation from asc to tif and from tif to asc is correct: Location of impact on the correct quadrant of landscape
validation_transformation <- function(inputmap,outputmap){
  transformation <- all.equal(inputmap, outputmap)
  print ("Checking transformation from asc to tif and from tif to asc")
  if (transformation == TRUE) {print ("Correct transformation")}  else {print ("No correct transformation")}
}

# asc to tif
validation_transformation(inputmap=raster(paste("{HOME}EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/input/oilpalm_c.asc" ,sep="")),
                          outputmap=raster(paste(outpath,"/oilpalm_c.tif" ,sep="")))
# tif to asc
validation_transformation(inputmap=raster(paste(outpath,"/quality_c_", experiment, ".tif" ,sep="")),
                          outputmap=raster(paste(outpath,"/quality_c_", experiment, ".asc" ,sep="")))


### Aim 3: Comparison of expected result with result of InVEST
## Generation of expected result

# Extract dimension of impact location from oilpalm_c.asc
asc <- raster(paste("{HOME}EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/input/oilpalm_c.asc" ,sep=""))
rowColimpact <- rowColFromCell(asc, which(asc[] == 1 ))

# Function for calculation of habitat-quality score
hqcalculation <- function(ry, wr, sumwr, dxy, drmax, irxy=irxy,bx=bx, Sjr, k, z){
  ry <- ry
  wr <- wr
  sumwr <- sumwr
  dxy <- dxy 
  drmax <- drmax
  irxy <- 1 - (dxy/drmax)
  bx <- 1
  Sjr <- Sjr
  k <- k
  z <- z
  Dxj <- (wr/sumwr) * ry * irxy * bx * Sjr 
  Qxj <- 1* (1-((Dxj^z)/((Dxj^z)+(k^z))))
  print (paste("Impact existence ry" , ry))
  print (paste("Impact weighting wr is" , wr))
  print (paste("Sum of impact weightings is" , sumwr))
  print (paste("Distance between habitat and impact dxy is" , dxy))
  print (paste("Greatest maximum distance of impact drmax is" , drmax))
  print (paste("Impact of impact r in parcel y on parcel x irxy is " , irxy))
  print (paste("Level of accessibility of parcel x bx is " , bx))
  print (paste("Sensitivity of habitat type j to impact r Sjr is " , Sjr))
  print (paste("Scaling parameter k is" , k))
  print (paste("Scaling parameter z is" , z))
  print (paste("Degradation score Dxj is " , Dxj))
  print (paste("!Habitat quality score Qxj is " , Qxj, "!"))
  return(Qxj)
}

if (experiment == "forest") {ry <- 0} else {ry <- 1}

Qxj <- hqcalculation(ry <- ry,
                     wr=read_csv(paste(outpath,"/impact_table.csv" ,sep=""))%>%
                       select(WEIGHT) %>%
                       .[[1,1]],
                     sumwr= read_csv(paste(outpath,"/impact_table.csv" ,sep=""))%>%
                       select (WEIGHT) %>%
                       summarise_all(sum) %>%
                       .[[1,1]],
                     dxy= 0 ,
                     drmax= read_csv(paste(outpath,"/impact_table.csv" ,sep=""))%>%
                       select(MAX_DIST) %>%
                       .[[1,1]],
                     irxy=irxy,
                     bx=bx,
                     Sjr=read_csv(paste(outpath,"/sensitivity_table.csv" ,sep="")) %>%
                       select(oilpalm)%>%
                       .[[3,1]],
                     k=0.5,
                     z=2.5)

# Generation of map
expectedmap <- raster(ncol=100, nrow= 100, xmn=212461, xmx=217461, ymn=9753255, ymx=9758255)
projection(expectedmap) <- "+proj=utm +zone=48 +south +datum=WGS84"
values(expectedmap) <- 1
expectedmap[rowColimpact] <- Qxj

# Comparison
validation_maps <- function(investmap,expectedmap){
  equalmaps <- all.equal(investmap, expectedmap)
  print ("Comparison of expected result and result of InVEST")
  if (equalmaps == TRUE) {print ("Valdating expected result")}  else {print ("Unexpected result")}
}

validation_maps(investmap=raster(paste(outpath,"/quality_c_", experiment, ".asc" ,sep="")),
                expectedmap=expectedmap)

################################################################################
### 3) Plotting
##Plots for verification of correct conversion

mycol_impact <- c(NA, "#ec7014")

# Impact: asc-file
asc_df <- as.data.frame(asc, xy=TRUE)
asc_map <- ggplot(data=asc_df) + 
           geom_raster(aes(x=x,y=y,fill=factor(oilpalm_c))) + 
           scale_fill_manual(labels = c("False", "True"), values = mycol_impact, name="Impact location (oilpalm)") +
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

# Impact:tif-file
tif <- raster(paste(outpath,"/oilpalm_c.tif" ,sep=""))
tif_df <- as.data.frame(tif, xy=TRUE)
tif_map <- ggplot(data=tif_df) + 
           geom_raster(aes(x=x,y=y,fill=factor(oilpalm_c))) + 
           scale_fill_manual(labels = c("False", "True"), values = mycol_impact, name="Impact location (oilpalm)") +
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

## Plots for verification of result 

mycol_quality <- c("#ffffe5","#00441b")

#invest: habitat-quality map
invest <- raster(paste(outpath,"/quality_c_", experiment, ".asc" ,sep=""))
invest_df <- as.data.frame(invest, xy=TRUE)
invest_map <- ggplot(data=invest_df) + 
              geom_raster(aes(x=x,y=y,fill=factor(quality_c_localimpact))) + 
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

# expected-map
expected_df <- as.data.frame(expectedmap, xy=TRUE)

expected_map <- ggplot(data=expected_df) + 
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

asc_map + tif_map + plot_layout(guides = 'collect')& theme(legend.position = 'bottom')
ggsave("impact_comp_local.png", path = "/homer/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/Plots/" )

invest_map + expected_map+ plot_layout(guides = 'collect')& theme(legend.position = 'bottom')
ggsave("result_comp_local.png", path = "/homer/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/Plots/" )
