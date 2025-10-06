#############################################
##### Integrationtest for NetVest #####
#############################################

# specify experiment 
# specify hsc (half-saturation-constant), default: 0.05. Has to be adapted after first simulation to ca. 1/2 of Dmax
# Input folder of ncinv/habitatquality has to include sensitivitytable.txt, impacttable.txt  
# adapt netlogopath, modelpath, outpath and netlogoversion

#needed libraries
library(nlrx)
library(Refforts)
library(raster)
library(ggplot2)

# ensure Java 11 for NL 6.x
Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-11-openjdk-amd64")
Sys.setenv(PATH = paste(file.path(Sys.getenv("JAVA_HOME"), "bin"),
                        Sys.getenv("PATH"), sep = .Platform$path.sep))

# add the --add-opens flags (append to whatever is already set)
flags <- c(
  "--add-opens=java.base/java.lang.ref=ALL-UNNAMED",
  "--add-opens=java.base/java.lang=ALL-UNNAMED"
)
Sys.setenv(JAVA_TOOL_OPTIONS = paste(c(Sys.getenv("JAVA_TOOL_OPTIONS"), flags), collapse = " "))

### 1) Integrationtest execution
experiment <- "integrationtest"
invtest <- paste("\"",experiment,"\"",sep="")
natcapinvestexperiment <- invtest
hsc <- 0.05
netlogopath <- file.path("{NetLogo}/6.2.1")
modelpath <- file.path("{NetVest}/EFForTS-ABM.nlogo")
outpath <- file.path("{NetVest}/ncinv/habitatquality/output")
netlogoversion <- "6.2.1"

nl <- nl(nlversion = netlogoversion,
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nl@experiment <- experiment(expname=experiment,
                            outpath=outpath,
                            repetition=1,
                            tickmetrics="false",
                            idsetup="integrationtest-biodiv-ncinv",
                            idgo="do-nothing",
                            runtime=1,
                            variables = list(),
                            constants = get.abm.defaults()
                                             )
                                             
nl <- set.nl.constant(nl, "ncinv_test", invtest)
nl <- set.nl.constant(nl, "biodiv_ncinv_k", hsc)
nl <- set.nl.constant(nl, "ncinv_experiment", natcapinvestexperiment)
nl <- set.nl.constant(nl, "write-maps?", TRUE)

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

## Aim 2: LULC-map: Correct translation of land-use type values 

# land-use map within EFForTS-ABM before translation (lut_abm)
lut_abm <- raster("{NetVest}/output/lut__000.asc")

# land-use map within EFFOrTS-ABM after translation (lut_trans)
lut_trans <- raster(paste("{NetVest}/output/lut_invest_",experiment,"_1_000.asc",sep=""))

# Function for comparison of maps: Substract rasters with absoulte values to validate that they are equal
validation_translation <- function (inputmap, outputmap) {
  translation <- abs(inputmap) - outputmap
  translation_df <- as.data.frame(translation)
  print ("Comparing maps")
  if (sum(translation_df$layer) == 0) {print (TRUE)} else {print (FALSE)}
}
# Comparison
validation_translation(lut_abm, lut_trans)


## Aim 2.2: Correct generation of LULC map
# lulc map for InVEST
lulc <- raster(paste(outpath,"/lulc.asc",sep=""))

# Comparison
validation_translation(lut_trans, lulc)


## Aim 3: Impact-map
## Aim 3.1: Correct translation of impact values
# land-use map within EFForTS-ABM for oilpalm before translation (op_abm_map)
lutabm_df <- as.data.frame(lut_abm, xy=TRUE) 
op_abm <- lutabm_df$lut__000 == 0
op_abm_map <- raster(ncol=100, nrow= 100, xmn=212461, xmx=217461, ymn=9753255, ymx=9758255)
projection(op_abm_map) <- "+proj=utm +zone=48 +south +datum=WGS84"
values(op_abm_map) <- op_abm

# land-use map within EFForTS-ABM for oilpalm after translation (op_trans_map)
luttrans_df <- as.data.frame(lut_trans, xy=TRUE)
op_trans<- luttrans_df$lut_invest_integrationtest_1_000 == 0
op_trans_map <- raster(ncol=100, nrow= 100, xmn=212461, xmx=217461, ymn=9753255, ymx=9758255)
projection(op_trans_map) <- "+proj=utm +zone=48 +south +datum=WGS84"
values(op_trans_map) <- op_trans

# land-use map within EFForTS-ABM for rubber before translation (rb_abm_map)
rb_abm<- lutabm_df$lut__000 == 1
rb_abm_map <- raster(ncol=100, nrow= 100, xmn=212461, xmx=217461, ymn=9753255, ymx=9758255)
projection(rb_abm_map) <- "+proj=utm +zone=48 +south +datum=WGS84"
values(rb_abm_map) <- rb_abm

# land-use map within EFForTS-ABM for rubber after translation (rb_trans_map)
rb_trans<- luttrans_df$lut_invest_integrationtest_1_000 == 1
rb_trans_map <- raster(ncol=100, nrow= 100, xmn=212461, xmx=217461, ymn=9753255, ymx=9758255)
projection(rb_trans_map) <- "+proj=utm +zone=48 +south +datum=WGS84"
values(rb_trans_map) <- rb_trans

# Comparison
validation_translation(op_abm_map, op_trans_map)
validation_translation(rb_abm_map, rb_trans_map)


## Aim 3.2: Correct generation of impact map
# impact-map for InVEST for oilpaln (op_invest)
op_invest <- raster(paste(outpath,"/oilpalm_c.asc",sep=""))

# impact-map for InVEST for rubber (rb_invest)
rb_invest <- raster(paste(outpath,"/rubber_c.asc",sep=""))

# Comparison
validation_translation(op_trans, op_invest)
validation_translation(rb_trans, rb_invest)


## Aim 4: Storing values of map of InVEST within EFForTS-ABM
# habitat quality map
quality_invest <- raster(paste(outpath,"/quality_c_" ,experiment, ".asc",sep=""))

# map with stored values from InVEST within EFForTS-ABM
quality_abm <- raster(paste("{NetVest}/output/lut_quality_",experiment,"_1_000.asc",sep=""))

# Comparison
validation_translation(quality_invest, quality_abm)
################################################################################
### 3) PLOTTING
## Aim 2: Correct translation and genereration of land-use type values

mycol_lut <- c("#238443", "#ec7014", "#fed976")
mycol_lutinvest <- c("#ec7014", "#fed976", "#238443" )

lutabm <- ggplot(data=lutabm_df) + 
           geom_raster(aes(x=x,y=y,fill=factor(lut__000))) + 
           scale_fill_manual(labels = c("forest", "oilpalm", "rubber"), values = mycol_lut, name="Land-use types") +
           xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Before translation") +
           #title("Land-use map before translation") +
           theme (plot.title = element_text(hjust = 0.1, size = 10),
                  axis.title.x = element_text(hjust=1, vjust= -1, size = 10),
                  axis.title.y = element_text(hjust=1, vjust= 2, size = 10),
                  axis.text = element_text(size = 5),
                  axis.ticks = element_line(size = 0),
                  legend.title = element_text(size = 10, face = "bold"),
                  legend.text = element_text(size = 10),
                  legend.key.width = unit(0.3, "cm"),
                  legend.key.height = unit(0.3, "cm"),
                  legend.position = "none",
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major = element_line(color = "gray90", size = 0.2),
                  panel.grid.minor = element_line(color = "gray90", size = 0.2),
                  aspect.ratio = 1,
                  plot.margin = margin(c(5,0,5,0)),
                  legend.spacing.x = unit(0.2, "cm"),
                 )

luttrans <- ggplot(data=luttrans_df) + 
             geom_raster(aes(x=x,y=y,fill=factor(lut_invest_integrationtest_1_000))) + 
             scale_fill_manual(labels = c("oilpalm", "rubber", "forest"), values = mycol_lutinvest, name="Land-use types") +
             xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("After translation") +
             theme (plot.title = element_text(hjust = 0.1, size = 10),
                    axis.title.x = element_text(hjust=1, vjust= -1, size = 10),
                    axis.title.y = element_text(hjust=1, vjust= 2, size = 10),
                    axis.text = element_text(size = 5),
                    axis.ticks = element_line(size = 0),
                    legend.title = element_text(size = 10, face = "bold"),
                    legend.text = element_text(size = 10),
                    legend.key.width = unit(0.3, "cm"),
                    legend.key.height = unit(0.3, "cm"),
                    legend.position = "none", 
                    panel.background = element_rect(fill = "white"),
                    panel.grid.major = element_line(color = "gray90", size = 0.2),
                    panel.grid.minor = element_line(color = "gray90", size = 0.2),
                    aspect.ratio = 1,
                    plot.margin = margin(c(5,0,5,0)),
                    legend.spacing.x = unit(0.2, "cm"),
                   )

lulc_df <- as.data.frame(lulc, xy=TRUE)

lulcplot <- ggplot(data=lulc_df) + 
  geom_raster(aes(x=x,y=y,fill=factor(lulc))) + 
  scale_fill_manual(labels = c("oilpalm", "rubber", "forest"), values = mycol_lutinvest, name="Land-use types") +
  xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Generated") +
  theme (plot.title = element_text(hjust = 0.1, size = 10),
         axis.title.x = element_text(hjust=1, vjust= -1, size = 10),
         axis.title.y = element_text(hjust=1, vjust= 2, size = 10),
         axis.text = element_text(size = 5),
         axis.ticks = element_line(size = 0),
         legend.title = element_text(size = 10, face = "bold"),
         legend.text = element_text(size = 10),
         legend.key.width = unit(0.3, "cm"),
         legend.key.height = unit(0.3, "cm"),
         legend.position = "right",
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(color = "gray90", size = 0.2),
         panel.grid.minor = element_line(color = "gray90", size = 0.2),
         aspect.ratio = 1,
         plot.margin = margin(c(5,0,5,0)),
         legend.spacing.x = unit(0.2, "cm")
  )

lutabm + luttrans + lulcplot
ggsave("lutabm_luttrans_lulc.png", path = "{NetVest}/tests_integration/02_integrationtest/Plots/" )
  
###############################################################################
##Aim 3 Correct translation and generation of impact values 

mycol_op <- c(NA, "#ec7014")
mycol_rb <- c(NA, "#fed976")

opabm <- ggplot(data=lutabm_df) + 
         geom_raster(aes(x=x,y=y,fill=factor(op_abm)))+  
         scale_fill_manual(labels = c("matrix", "oilpalm"), values = mycol_op, name="Impact location (oilpalm)") +
         xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Before translation") +
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
                plot.margin = margin(c(5,0,5,0)),
                legend.spacing.x = unit(0.2, "cm")
               )

optrans <- ggplot(data=luttrans_df) + 
           geom_raster(aes(x=x,y=y,fill=factor(op_trans)))+  
           scale_fill_manual(labels = c("matrix", "oilpalm"), values = mycol_op, name="Impact location (oilpalm)") +
           xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("After translation") +
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
                  plot.margin = margin(c(5,0,5,0)),
                  legend.spacing.x = unit(0.2, "cm")
  )


opinvest_df <- as.data.frame(op_invest, xy=TRUE)

impactop <- ggplot(data=opinvest_df) + 
  geom_raster(aes(x=x,y=y,fill=factor(oilpalm_c))) + 
  scale_fill_manual(labels = c("matrix", "oilpalm"), values = mycol_op, name="Impact location (oilpalm)") +
  xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Generated") +
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
         plot.margin = margin(c(5,0,5,0)),
         legend.spacing.x = unit(0.2, "cm")
  )

opabm + optrans + impactop + plot_layout(guides = 'collect')& theme(legend.position = 'bottom')
ggsave("opabm_optrans.png", path = "{NetVest}/tests_integration/02_integrationtest/Plots/" )


rbabm <- ggplot(data=lutabm_df) + 
         geom_raster(aes(x=x,y=y,fill=factor(rb_abm)))+  
         scale_fill_manual(labels = c("matrix", "rubber"), values = mycol_rb, name="Impact location (rubber)") +
         xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Before translation") +
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
                legend.spacing.x = unit(0.2, "cm")
               )

rbtrans <- ggplot(data=luttrans_df) + 
           geom_raster(aes(x=x,y=y,fill=factor(rb_trans)))+  
           scale_fill_manual(labels = c("matrix", "rubber"), values = mycol_rb, name="Impact location (rubber)") +
           xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("After translation") +
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
                  legend.spacing.x = unit(0.2, "cm")
                 )

rbinvest_df <- as.data.frame(rb_invest, xy=TRUE)
impactrb <- ggplot(data=rbinvest_df) + 
  geom_raster(aes(x=x,y=y,fill=factor(rubber_c)))+  
  scale_fill_manual(labels = c("matrix", "rubber"), values = mycol_rb, name="Impact location (rubber)") +
  xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Generated") +
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
         legend.spacing.x = unit(0.2, "cm")
  )

rbabm + rbtrans + impactrb + plot_layout(guides = 'collect')& theme(legend.position = 'bottom')
ggsave("rbabm_rbtrans.png", path = "{NetVest}/tests_integration/02_integrationtest/Plots/" )
###############################################################################
##Aim 4 Correct storing of habitat quality values
qualityinvest_df <- as.data.frame(quality_invest, xy=TRUE)
quality <- ggplot(data=qualityinvest_df) + 
           geom_raster(aes(x=x,y=y,fill=quality_c_integrationtest)) +  
           scale_fill_gradient(low = "#ffffe5", high = "#00441b", name = "Habitat-Quality Scores") +
           xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Generated habitat-quality map") +
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
                  legend.spacing.x = unit(0.2, "cm")
                 )

qualityabm_df <- as.data.frame(quality_abm, xy=TRUE)
quality2 <- ggplot(data=qualityabm_df) + 
            geom_raster(aes(x=x,y=y,fill=lut_quality_integrationtest_1_000)) +  
            scale_fill_gradient(low = "#ffffe5", high = "#00441b", name = "Habitat-Quality Scores") +
            xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Integrated habitat-quality map") +
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
                  legend.spacing.x = unit(0.2, "cm")
                 )
quality + quality2 + plot_layout(guides = 'collect')& theme(legend.position = 'bottom')
ggsave("quality_quality2.png", path = "{NetVest}/tests_integration/02_integrationtest/Plots/")
