#########################################################
##### Integrationtest for Natcap Invest: 01 #
#########################################################

# specify experiment 
# specify hsc (half-saturation-constant), default: 0.05. Has to be adapted after first simulation to ca. 1/2 of Dmax
# Input folder has to include sensitivitytable.txt, impacttable.txt  
# adapt netlogopath, modelpath, outpath and netlogoversion

### 1) Integrationtest execution
library(nlrx)
library(Refforts)
experiment <- "integrationtest01"
invtest <- paste("\"",experiment,"\"",sep="")
natcapinvestexperiment <- invtest
hsc <- 0.05
netlogopath <- file.path("/home/dockerj/nl")
modelpath <- file.path("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/EFForTS-ABM.nlogo")
outpath <- file.path("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/natcap_invest/output")
netlogoversion <- "6.1.1"

nl <- nl(nlversion = netlogoversion,
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

nl@experiment <- experiment(expname=experiment,
                            outpath=outpath,
                            repetition=1,
                            tickmetrics="false",
                            idsetup="test-integration",
                            idgo="do-nothing",
                            runtime=1,
                            #metrics=c("edu-calc-index"),
                            variables = list(),
                            constants = get.abm.defaults()
                                             )
                                             
nl <- set.nl.constant(nl, "inv-test", invtest)
nl <- set.nl.constant(nl, "k", hsc)
nl <- set.nl.constant(nl, "natcap_invest_experiment", natcapinvestexperiment)
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

## Aim 2: LULC-map 
## Aim 2.1: Correct translation of land-use type values
library(raster)
library(ggplot2)

# land-use map within EFForTS-ABM before translation (lut_abm)
lut_abm <- raster("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/output/lut__001.asc")

# land-use map within EFFOrTS-ABM after translation (lut_trans)
lut_trans <- raster("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/output/lut_invest__001.asc")

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
lulc <- raster("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/natcap_invest/output/lulc.asc")

# Comparison
validation_translation(lut_trans, lulc)


## Aim 3: Impact-map
## Aim 3.1: Correct translation of impact values
# land-use map within EFForTS-ABM for oilpalm before translation (op_abm_map)
lutabm_df <- as.data.frame(lut_abm, xy=TRUE) 
op_abm<- lutabm_df$lut__001 == 0
op_abm_map <- raster(ncol=100, nrow= 100, xmn=212461, xmx=217461, ymn=9753255, ymx=9758255)
projection(op_abm_map) <- "+proj=utm +zone=48 +south +datum=WGS84"
values(op_abm_map) <- op_abm

# land-use map within EFForTS-ABM for oilpalm after translation (op_trans_map)
luttrans_df <- as.data.frame(lut_trans, xy=TRUE)
op_trans<- luttrans_df$lut_invest__001 == 0
op_trans_map <- raster(ncol=100, nrow= 100, xmn=212461, xmx=217461, ymn=9753255, ymx=9758255)
projection(op_trans_map) <- "+proj=utm +zone=48 +south +datum=WGS84"
values(op_trans_map) <- op_trans

# land-use map within EFForTS-ABM for rubber before translation (rb_abm_map)
rb_abm<- lutabm_df$lut__001 == 1
rb_abm_map <- raster(ncol=100, nrow= 100, xmn=212461, xmx=217461, ymn=9753255, ymx=9758255)
projection(rb_abm_map) <- "+proj=utm +zone=48 +south +datum=WGS84"
values(rb_abm_map) <- rb_abm

# land-use map within EFForTS-ABM for rubber after translation (rb_trans_map)
rb_trans<- luttrans_df$lut_invest__001 == 1
rb_trans_map <- raster(ncol=100, nrow= 100, xmn=212461, xmx=217461, ymn=9753255, ymx=9758255)
projection(rb_trans_map) <- "+proj=utm +zone=48 +south +datum=WGS84"
values(rb_trans_map) <- rb_trans

# Comparison
validation_translation(op_abm_map, op_trans_map)
validation_translation(rb_abm_map, rb_trans_map)


## Aim 3.2: Correct generation of impact map
# impact-map for InVEST for oilpaln (op_invest)
op_invest <- raster("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/natcap_invest/output/oilpalm_c.asc")

# impact-map for InVEST for rubber (rb_invest)
rb_invest <- raster("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/natcap_invest/output/rubber_c.asc")

# Comparison
validation_translation(op_trans, op_invest)
validation_translation(rb_trans, rb_invest)


## Aim 4: Storing values of map of InVEST within EFForTS-ABM
# habitat quality map
quality_invest <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/natcap_invest/output/quality_c_" ,experiment, ".asc",sep=""))

# map with stored values from InVEST within EFForTS-ABM
quality_abm <- raster("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/output/lut_quality__001.asc")

# Comparison
validation_translation(quality_invest, quality_abm)
################################################################################
### PLOTTING###
## Aim 2.1: Correct translation of land-use type values

mycol_lut <- c("#238443", "#ec7014", "#fed976")
mycol_lutinvest <- c("#ec7014", "#fed976", "#238443" )

lutabm <- ggplot(data=lutabm_df) + 
           geom_raster(aes(x=x,y=y,fill=factor(lut__001))) + 
           scale_fill_manual(labels = c("forest", "oilpalm", "rubber"), values = mycol_lut, name="Land-use types") +
           xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Land-use map before translation") +
           #title("Land-use map before translation") +
           theme (plot.title = element_text(hjust = 0.1, size = 5),
                  axis.title.x = element_text(hjust=1, vjust= -1, size = 3),
                  axis.title.y = element_text(hjust=1, vjust= 2, size = 3),
                  axis.text = element_text(size = 2.5),
                  axis.ticks = element_line(size = 0),
                  legend.title = element_text(size = 4, face = "bold"),
                  legend.text = element_text(size = 4),
                  legend.key.width = unit(0.3, "cm"),
                  legend.key.height = unit(0.3, "cm"),
                  legend.position = "none",
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major = element_line(color = "gray90", size = 0.2),
                  panel.grid.minor = element_line(color = "gray90", size = 0.2),
                  aspect.ratio = 1,
                  plot.margin = margin(c(5,0,5,0)),
                  legend.spacing.x = unit(0.2, "cm"),
                  #legend.box.margin = unit(c(0,0,20,0), "mm"),
                 )

luttrans <- ggplot(data=luttrans_df) + 
             geom_raster(aes(x=x,y=y,fill=factor(lut_invest__001))) + 
             scale_fill_manual(labels = c("oilpalm", "rubber", "forest"), values = mycol_lutinvest, name="Land-use types") +
             xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Land-use map after translation") +
             theme (plot.title = element_text(hjust = 0.1, size = 5),
                    axis.title.x = element_text(hjust=1, vjust= -1, size = 3),
                    axis.title.y = element_text(hjust=1, vjust= 2, size = 3),
                    axis.text = element_text(size = 2.5),
                    axis.ticks = element_line(size = 0),
                    legend.title = element_text(size = 4, face = "bold"),
                    legend.text = element_text(size = 4),
                    legend.key.width = unit(0.3, "cm"),
                    legend.key.height = unit(0.3, "cm"),
                    legend.position = "none", 
                    panel.background = element_rect(fill = "white"),
                    panel.grid.major = element_line(color = "gray90", size = 0.2),
                    panel.grid.minor = element_line(color = "gray90", size = 0.2),
                    aspect.ratio = 1,
                    plot.margin = margin(c(5,0,5,0)),
                    legend.spacing.x = unit(0.2, "cm"),
                    #legend.box.margin = unit(c(0,0,20,0), "mm"),
                   )

lulc_df <- as.data.frame(lulc, xy=TRUE)

lulcplot <- ggplot(data=lulc_df) + 
  geom_raster(aes(x=x,y=y,fill=factor(lulc))) + 
  scale_fill_manual(labels = c("oilpalm", "rubber", "forest"), values = mycol_lutinvest, name="Land-use types") +
  xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Generated land-use map") +
  theme (plot.title = element_text(hjust = 0.1, size = 5),
         axis.title.x = element_text(hjust=1, vjust= -1, size = 3),
         axis.title.y = element_text(hjust=1, vjust= 2, size = 3),
         axis.text = element_text(size = 2.5),
         axis.ticks = element_line(size = 0),
         legend.title = element_text(size = 4, face = "bold"),
         legend.text = element_text(size = 4),
         legend.key.width = unit(0.3, "cm"),
         legend.key.height = unit(0.3, "cm"),
         legend.position = "right",
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(color = "gray90", size = 0.2),
         panel.grid.minor = element_line(color = "gray90", size = 0.2),
         aspect.ratio = 1,
         plot.margin = margin(c(5,0,5,0)),
         legend.spacing.x = unit(0.2, "cm"),
         #legend.box.margin = unit(c(0,0,20,0), "mm"),
  )

#leg <- ggplot(data=lulc_df) + 
 # geom_raster(aes(x=x,y=y,fill=factor(lulc))) + 
#  scale_fill_manual(labels = c("oilpalm", "rubber", "forest"), values = mycol_lutinvest, name="Land-use types") +
 # xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Generated land-use map") +
#  theme(legend.position = c(0.5,0.5),
 #       legend.key.size = unit(1, "cm"),
  #      legend.text = element_text(size =  12),
   #     legend.title = element_text(size = 15, face = "bold"))+
  #guides(colour = guide_legend(override.aes = list(size=8)))

#legend <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species))+
 # geom_point()+
  #lims(x = c(0,0), y = c(0,0))+
  #theme_void()+
  #theme(legend.position = c(0.5,0.5),
   #     legend.key.size = unit(1, "cm"),
    #    legend.text = element_text(size =  12),
     #   legend.title = element_text(size = 15, face = "bold"))+
  #guides(colour = guide_legend(override.aes = list(size=8)))

library(ggpubr)
ggarrange(lutabm, luttrans, lulcplot, ncol = 3, nrow = 1, common.legend = TRUE, legend = "right" )#%>%
 # gridExtra::grid.arrange (heights = unit(c(80, 10), "mm"))
ggsave("lutabm_luttrans_lulc.png", path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/02_integrationtest/Plots/" )#, plot="plot1",device = png) #, path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/Plots/")

################################################################################
## Aim 2.2 Correct generation of LULC map
luttrans <- ggplot(data=luttrans_df) + 
  geom_raster(aes(x=x,y=y,fill=factor(lut_invest__001))) + 
  scale_fill_manual(labels = c("oilpalm", "rubber", "forest"), values = mycol_lutinvest, name="Land-use types") +
  xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Land-use map after translation") +
  #title("Land-use map after translation") +
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

lulc_df <- as.data.frame(lulc, xy=TRUE)

lulcplot <- ggplot(data=lulc_df) + 
            geom_raster(aes(x=x,y=y,fill=factor(lulc))) + 
            scale_fill_manual(labels = c("oilpalm", "rubber", "forest"), values = mycol_lutinvest, name="Land-use types") +
            xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Generated land-use map") +
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
  
ggarrange(luttrans, lulcplot, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave("luttrans_lulc.png", path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/02_integrationtest/Plots/" )
  
###############################################################################
##Aim 3.1 Correct translation of impact values 

mycol_op <- c(NA, "#ec7014")
mycol_rb <- c(NA, "#fed976")

opabm <- ggplot(data=lutabm_df) + 
         geom_raster(aes(x=x,y=y,fill=factor(op_abm)))+  
         scale_fill_manual(labels = c("matrix", "oilpalm"), values = mycol_op, name="Impact location (oilpalm)") +
         xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Impact-map before translation") +
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

optrans <- ggplot(data=luttrans_df) + 
           geom_raster(aes(x=x,y=y,fill=factor(op_trans)))+  
           scale_fill_manual(labels = c("matrix", "oilpalm"), values = mycol_op, name="Impact location (oilpalm)") +
           xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Impact-map after translation") +
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


ggarrange(opabm, optrans, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave("opabm_optrans.png", path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/02_integrationtest/Plots/" )


rbabm <- ggplot(data=lutabm_df) + 
         geom_raster(aes(x=x,y=y,fill=factor(rb_abm)))+  
         scale_fill_manual(labels = c("matrix", "rubber"), values = mycol_rb, name="Impact location (rubber)") +
         xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Impact-map before translation") +
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

rbtrans <- ggplot(data=luttrans_df) + 
           geom_raster(aes(x=x,y=y,fill=factor(rb_trans)))+  
           scale_fill_manual(labels = c("matrix", "rubber"), values = mycol_rb, name="Impact location (rubber)") +
           xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Impact-map after translation") +
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

ggarrange(rbabm, rbtrans, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave("rbabm_rbtrans.png", path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/02_integrationtest/Plots/" )

###############################################################################
#Aim 3.2 Correct generation of impact maps
optrans <- ggplot(data=luttrans_df) + 
  geom_raster(aes(x=x,y=y,fill=factor(op_trans)))+  
  scale_fill_manual(labels = c("matrix", "oilpalm"), values = mycol_op, name="Impact location (oilpalm)") +
  xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Impact-map after translation") +
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

opinvest_df <- as.data.frame(op_invest, xy=TRUE)
impactop <- ggplot(data=opinvest_df) + 
            geom_raster(aes(x=x,y=y,fill=factor(oilpalm_c))) + 
            scale_fill_manual(labels = c("matrix", "oilpalm"), values = mycol_op, name="Impact location (oilpalm)") +
            xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Generated Impact-map for oilpalm") +
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

rbtrans <- ggplot(data=luttrans_df) + 
  geom_raster(aes(x=x,y=y,fill=factor(rb_trans)))+  
  scale_fill_manual(labels = c("matrix", "rubber"), values = mycol_rb, name="Impact location (rubber)") +
  xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Impact-map after translation") +
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

rbinvest_df <- as.data.frame(rb_invest, xy=TRUE)
impactrb <- ggplot(data=rbinvest_df) + 
            geom_raster(aes(x=x,y=y,fill=factor(rubber_c)))+  
            scale_fill_manual(labels = c("matrix", "rubber"), values = mycol_rb, name="Impact location (rubber)") +
            xlab("Longitude (X)") + ylab("Latitude (Y)") + ggtitle("Generated Impact-map for rubber") +
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
 
ggarrange(optrans, impactop, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave("optrans_impactop.png", path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/02_integrationtest/Plots/" )

ggarrange(rbtrans, impactrb, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave("rbtrans_impactrb.png", path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/02_integrationtest/Plots/" )

###############################################################################
##Aim 4 Correct storing of habitat quality values
qualityinvest_df <- as.data.frame(quality_invest, xy=TRUE)
quality <- ggplot(data=qualityinvest_df) + 
           geom_raster(aes(x=x,y=y,fill=quality_c_integrationtest01)) +  
           scale_fill_gradient(low = "#ffffe5", high = "#00441b", name = "Habitat-Quality Scores ") +
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
            geom_raster(aes(x=x,y=y,fill=lut_quality__001)) +  
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

ggarrange(quality, quality2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
ggsave("quality_quality2.png", path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/02_integrationtest/Plots/" )#, plot="plot1",device = png) #, path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/Plots/")
