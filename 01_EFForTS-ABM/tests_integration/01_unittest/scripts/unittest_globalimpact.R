#########################################################
##### Unittest for Natcap Invest Integration: local #####
#########################################################

# specify experiment and create folder named after experiment with one input folder and one output folder within
# specify hsc (half-saturation-constant), default: 0.5
# Input folder has to include sensitivitytable.txt, impacttable.txt, lulc.asc, oilpalm_c.asc and rubber_c.asc 
# adapt netlogopath, modelpath, outpath and netlogoversion


### 1) Unittest execution
library(nlrx)
experiment <- "globalimpact"
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


### 2) Validation of results

## Aim 1: Correct execution of unittest: [Success] [Success]
##        Correct file in output folder
fileexist <- function(qualitymap){
  qualitymap <- qualitymap
  print ("Checking existence of habitat-quality map in output folder")
  if (qualitymap == TRUE) {print ("Habitat-quality map exists")}  else {print ("Habitat-quality map is missing")}
}

fileexist(qualitymap=file.exists((paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/quality_c_", experiment, ".asc" ,sep=""))))


## Aim 2: Validating linear decrease of degradation scores over space

# Impact location
library(raster)
asc <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/input/oilpalm_c.asc" ,sep=""))
xyimpact <- xyFromCell(asc, which(asc[] == 1)) 

# Linear Regression: Checking linear relationship between distance from impact and degradation score
# Expectation: circular and linear decrease of degradation score with increasing distance from impact location
library(tidyverse)
deg.map <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/deg_sum_c_" , experiment, ".tif" ,sep=""))
deg.map_df <- as.data.frame(deg.map, xy = TRUE)
deg.map_tib <- as_tibble(deg.map_df)

south <- deg.map_tib%>%
  filter(x >= 214936 & x <=214986)%>%
  filter(y <= 9755730)

north <- deg.map_tib%>%
  filter(x >= 214936 & x <=214986)%>%
  filter(y >= 9755730)

east <- deg.map_tib%>%
  filter(x >= 214986) %>%
  filter(y >= 9755730 & y <=9755780)

west <- deg.map_tib%>%
  filter(x <= 214986) %>%
  filter(y >= 9755730 & y <=9755780)


### 3) Plots
library(ggpmisc)
cols <- c("Scores (points) with linear regression (transparent line)"="tomato3")
npc_txt_south = geom_text_npc(aes(npcx = 0.02, npcy = 0.95, label = "South"), size = 8)
npc_txt_north = geom_text_npc(aes(npcx = 0.98, npcy = 0.95, label = "North"), size = 8)
npc_txt_east = geom_text_npc(aes(npcx = 0.98, npcy = 0.95, label = "East"), size = 8)
npc_txt_west = geom_text_npc(aes(npcx = 0.02, npcy = 0.95, label = "West"), size = 8)
npc_txt_impact = geom_text_npc(aes(npcx = 0.02, npcy = 0.95, label = "214986,9755730"), size = 8)

plot.north <- ggplot(data=north, mapping = aes(x = y, y = deg_sum_c_globalimpact)) +
              geom_point(mapping = aes(color = "Scores (points) with linear regression (transparent line)")) +
              geom_line(mapping = aes(color = "Scores (points) with linear regression (transparent line)"), stat="smooth", method="lm", alpha = 0.4) +
              npc_txt_north +
              scale_colour_manual(values=cols) +
              scale_x_continuous(breaks=seq(9755780,9758280,500)) +
              stat_poly_eq(aes(label = paste(stat(eq.label), stat(rr.label), 
                               sep = "~~italic(\"with\")~~")),
                               eq.with.lhs = "D[x][y]~`=`~",
                               eq.x.rhs = "~Y",
                               formula = y ~ x, 
                               parse = TRUE, size = 3, label.y = 0.8, label.x = 0.98) +
              labs(
                   x = "Y-Coordinate (Y)", 
                   y = expression(paste("Habitat-degradation Score (D"["x"]["y"],")"))) +
                   theme_bw(base_size = 10) + theme(legend.position = "none",
                                                    axis.title.y = element_blank(),
                                                    plot.margin = margin(c(5,5,5,20)))
                                                    
plot.east <- ggplot(data=east, mapping = aes(x = x, y = deg_sum_c_globalimpact)) +
             geom_point(mapping = aes(color = "Scores (points) with linear regression (transparent line)")) +
             geom_line(mapping = aes(color = "Scores (points) with linear regression (transparent line)"), stat="smooth", method="lm", alpha = 0.4) +
             stat_poly_eq(aes(label = paste(stat(eq.label), stat(rr.label), 
                              sep = "~~italic(\"with\")~~")),
                              eq.with.lhs = "D[x][y]~`=`~",
                              eq.x.rhs = "~X",
                              formula = y ~ x, 
                              parse = TRUE, size = 3, label.y = 0.8, label.x = 0.98) +
             npc_txt_east +
             scale_colour_manual(values=cols) +
             scale_x_continuous(breaks = seq(214986, 217486, 500)) +
             labs(
                  x = "X-Coordinate (X)", 
                  y = expression(paste("Habitat-degradation Score (D"["x"]["y"],")"))) +
                  theme_bw(base_size = 10) + theme(legend.position = "none",
                                                   axis.title.y = element_blank(),
                                                   plot.margin = margin(c(5,5,5,20)))

plot.south <- ggplot(data=south, mapping = aes(x = y, y = deg_sum_c_globalimpact)) +
              geom_point(mapping = aes(color = "Scores (points) with linear regression (transparent line)")) +
              geom_line(mapping = aes(color = "Scores (points) with linear regression (transparent line)"), stat="smooth", method="lm", alpha = 0.4) +
              stat_poly_eq(aes(label = paste(stat(eq.label), stat(rr.label), 
                               sep = "~~italic(\"with\")~~")),
                               eq.with.lhs = "D[x][y]~`=`~",
                               eq.x.rhs = "~Y",
                               formula = y ~ x, 
                               parse = TRUE, size = 3, label.y = 0.8, label.x = 0.02) +
              npc_txt_south +
              scale_colour_manual(values=cols, name="Habitat degradation") +
              scale_x_continuous(breaks = seq(9753280, 9755780, 500)) + 
              labs(
                   x = "Y-Coordinate (Y)", 
                   y = expression(paste("Habitat-degradation Score (D"["x"]["y"],")"))) +
              theme_bw(base_size = 10) + 
              theme(legend.position = "none",
                    plot.margin = margin(c(5,5,5,20)))

plot.west <- ggplot(data=west, mapping = aes(x = x, y = deg_sum_c_globalimpact)) +
             geom_point(mapping = aes(color = "Scores (points) with linear regression (transparent line)")) +
             geom_line(mapping = aes(color = "Scores (points) with linear regression (transparent line)"), stat="smooth", method="lm", alpha = 0.4) +
             stat_poly_eq(aes(label = paste(stat(eq.label), stat(rr.label), 
                              sep = "~~italic(\"with\")~~")),
                              eq.with.lhs = "D[x][y]~`=`~",
                              eq.x.rhs = "~X",
                              formula = y ~ x, 
                              parse = TRUE, size = 3, label.y = 0.8, label.x = 0.02) +
             npc_txt_west +
             scale_colour_manual(values=cols) +
             scale_x_continuous(breaks = seq(212486, 214986, 500)) +
             labs(
                  x = "X-Coordinate (X)", 
                  y = expression(paste("Habitat-degradation Score (D"["x"]["y"],")"))) +
                  theme_bw(base_size = 10) + 
             theme(legend.position = "none",
                   plot.margin = margin(c(5,5,5,20)))

deg <- ggplot(data=deg.map_df) + 
  geom_raster(aes(x=x,y=y,fill=deg_sum_c_globalimpact)) +  
  scale_fill_gradient(low = "#ffffe5", high = "#800026", name = "Degradation Scores ") +
  xlab("Longitude (X)") + ylab("Latitude (Y)") + #ggtitle("Generated degradation map") +
  theme (plot.title = element_text(hjust = 0.1, size = 6),
         axis.title.x = element_text(hjust=1, vjust= -1, size = 10),
         axis.title.y = element_text(hjust=1, vjust= 2, size = 10),
         axis.text = element_text(size = 5),
         axis.ticks = element_line(size = 0),
         legend.title = element_text(size = 10, face = "bold"),
         legend.text = element_text(size = 8),
         legend.key.width = unit(0.3, "cm"),
         legend.key.height = unit(0.3, "cm"),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(color = "gray90", size = 0.2),
         panel.grid.minor = element_line(color = "gray90", size = 0.2),
         aspect.ratio = 1,
         plot.margin = margin(c(5,-117,5,1)),
         legend.spacing.x = unit(0.2, "cm")
  )

### PLOTTING
library(ggpubr)
ggarrange(ggarrange(plot.south, plot.north, ncol =2, labels = c("A", "B")),  
          ggarrange(plot.west, plot.east, ncol =2, labels = c("C", "D")),
          ggarrange(deg, ncol = 1, labels = "E"),
          nrow = 3, 
          heights = c(1, 1, 1, 1, 100),
          widths = c(1, 1, 1, 1, 100),
          align = "v",
          labels = "A"                                        
)
# !xyimpact is location of impact: 214986 9755730
ggsave("degradation_landscape.png", path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/Plots/", scale = 3 )#, plot="plot1",device = png) #, path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/Plots/")


 
