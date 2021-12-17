#########################################################
##### Unittest for Natcap Invest Integration: no impact #
#########################################################

# specify experiment and create folder named after experiment with one input folder and one output folder within
# specify hsc (half-saturation-constant), default: 0.5
# Input folder has to include sensitivitytable.txt, impacttable.txt, lulc.asc, oilpalm_c.asc and rubber_c.asc 
# adapt netlogopath, modelpath, outpath and netlogoversion

### 1) Unittest execution
library(nlrx)
experiment <- "noimpact"
invtest <- paste("\"",experiment,"\"",sep="")
hsc <- 0.5
netlogopath <- file.path("/home/dockerj/nl")
modelpath <- file.path("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/EFForTS-ABM.nlogo")
outpath <- file.path(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/",experiment,"/output",sep=""))
netlogoversion <- "6.1.1"

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
                            constants = list("inv-test"=invtest,
                                             "k"=hsc))

nl@simdesign <- simdesign_simple(nl=nl,
                                 nseeds=1)

eval_variables_constants(nl)
print(nl)


results <- run_nl_one(nl, seed = 1, siminputrow = 1)


### 2) Validation of results

## Aim 1: Correct execution of unittest: [Success] [Success]
##        Correct file in output folder
fileexist <- function(qualitymap){
  qualitymap <- qualitymap
  print ("Checking existence of habitat-quality map in output folder")
  if (qualitymap == TRUE) {print ("Habitat-quality map exists")}  else {print ("Habitat-quality map is missing")}
}

fileexist(qualitymap=file.exists((paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/quality_c_", experiment, ".asc" ,sep=""))))

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

validation_maps(investmap=raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/quality_c_", experiment, ".asc" ,sep="")),
                expectedmap=expectedmap)

### 3) Plots

# quality_c_noimpact.asc
invest <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/quality_c_", experiment, ".asc" ,sep=""))
invest_df <- as.data.frame(invest, xy=TRUE)

mycol_quality <- "#00441b"
     
invest_map <- ggplot(data=invest_df) + 
          geom_raster(aes(x=x,y=y,fill=factor(quality_c_noimpact)))+  
          scale_fill_manual(values = mycol_quality, name="Habitat-Quality Scores") +
          xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Habitat-quality map of InVEST") +
          theme (plot.title = element_text(hjust = 0.1, size = 6),
                 axis.title.x = element_text(hjust=1, vjust= -1, size = 3),
                 axis.title.y = element_text(hjust=1, vjust= 2, size = 3),
                 axis.text = element_text(size = 2.5),
                 axis.ticks = element_line(size = 0),
                 legend.title = element_text(size = 4, face = "bold"),
                 legend.text = element_text(size = 4),
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
             xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Expected habitat-quality map") +
             theme (plot.title = element_text(hjust = 0.1, size = 6),
             axis.title.x = element_text(hjust=1, vjust= -1, size = 3),
             axis.title.y = element_text(hjust=1, vjust= 2, size = 3),
             axis.text = element_text(size = 2.5),
             axis.ticks = element_line(size = 0),
             legend.title = element_text(size = 4, face = "bold"),
             legend.text = element_text(size = 4),
             legend.key.width = unit(0.3, "cm"),
             legend.key.height = unit(0.3, "cm"),
             panel.background = element_rect(fill = "white"),
             panel.grid.major = element_line(color = "gray90", size = 0.2),
             panel.grid.minor = element_line(color = "gray90", size = 0.2),
             aspect.ratio = 1,
             plot.margin = margin(c(5,1,5,-25)),
             legend.spacing.x = unit(0.2, "cm"),
            )           

### PLOTTING
library(ggpubr)
ggarrange(invest_map, expected_map, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

ggsave("quality_comp_forest.png", path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/Plots/" )#, plot="plot1",device = png) #, path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/Plots/")
