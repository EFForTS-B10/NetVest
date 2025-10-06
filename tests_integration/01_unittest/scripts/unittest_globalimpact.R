##########################################
##### Unittest for NetVest: global #####
##########################################

# specify experiment and create folder named after experiment with one input folder and one output folder within
# specify hsc (half-saturation-constant), default: 0.5
# Input folder has to include sensitivitytable.txt, impacttable.txt, lulc.asc, oilpalm_c.asc and rubber_c.asc 
# adapt netlogopath, modelpath, outpath and netlogoversion

# needed libraries
library(nlrx)
library(raster)
library(tidyverse)
library(patchwork)
library(ggpmisc)

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

### 1) Unittest execution
experiment <- "globalimpact"
invtest <- paste("\"",experiment,"\"",sep="")
hsc <- 0.5
netlogopath <- file.path("{NetLogo}/6.2.1")
modelpath <- file.path("{NetVest}/EFForTS-ABM.nlogo")
outpath <- file.path(paste("{NetVest}/tests_integration/01_unittest/",experiment,"/output",sep=""))
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

################################################################################
### 2) Validation of results

## Aim 1: Correct execution of unittest: [Success] [Success]
##        Correct file in output folder
fileexist <- function(qualitymap){
  qualitymap <- qualitymap
  print ("Checking existence of habitat-quality map in output folder")
  if (qualitymap == TRUE) {print ("Habitat-quality map exists")}  else {print ("Habitat-quality map is missing")}
}

fileexist(qualitymap=file.exists((paste(outpath,"/quality_c_", experiment, ".asc" ,sep=""))))


## Aim 2: Validating linear decrease of degradation scores over space

# Impact location
asc <- raster(paste("{NetVest}/tests_integration/01_unittest/", experiment, "/input/oilpalm_c.asc" ,sep=""))
xyimpact <- xyFromCell(asc, which(asc[] == 1)) 

# Linear Regression: Checking linear relationship between distance from impact and degradation score
# Expectation: circular and linear decrease of degradation score with increasing distance from impact location
deg.map <- raster(paste(outpath,"/deg_sum_c_" , experiment, ".tif" ,sep=""))
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

lm_north = lm(north$deg_sum_c_globalimpact ~ north$y)
rsquared_north <- (summary(lm_north)$r.squared)
rsquared_north <- round(rsquared_north, 2)
c1_north<- lm_north$coefficients[1]
c1_north <- round(c1_north, 2)
c2_north<- lm_north$coefficients[2]
c2_north <- round(c2_north, 8)

lm_east = lm(east$deg_sum_c_globalimpact ~ east$x)
rsquared_east <- (summary(lm_east)$r.squared)
rsquared_east <- round(rsquared_east, 2)
c1_east<- lm_east$coefficients[1]
c1_east <- round(c1_east, 2)
c2_east<- lm_east$coefficients[2]
c2_east <- round(c2_east, 8)

lm_south = lm(south$deg_sum_c_globalimpact ~ south$y)
rsquared_south <- (summary(lm_south)$r.squared)
rsquared_south <- round(rsquared_south, 2)
c1_south<- lm_south$coefficients[1]
c1_south <- round(c1_south, 2)
c2_south<- lm_south$coefficients[2]
c2_south <- round(c2_south, 8)

lm_west = lm(west$deg_sum_c_globalimpact ~ west$x)
rsquared_west <- (summary(lm_west)$r.squared)
rsquared_west <- round(rsquared_west, 2)
c1_west<- lm_west$coefficients[1]
c1_west <- round(c1_west, 2)
c2_west<- lm_west$coefficients[2]
c2_west <- round(c2_west, 8)

###############################################################################
### 3) Plots
cols <- c("Scores (points) with linear regression (transparent line)"="tomato3")
npc_txt_south = geom_text_npc(aes(npcx = 0.02, npcy = 0.95, label = "South"), size = 5)
npc_txt_north = geom_text_npc(aes(npcx = 0.98, npcy = 0.95, label = "North"), size = 5)
npc_txt_east = geom_text_npc(aes(npcx = 0.98, npcy = 0.95, label = "East"), size = 5)
npc_txt_west = geom_text_npc(aes(npcx = 0.02, npcy = 0.95, label = "West"), size = 5)
npc_txt_impact = geom_text_npc(aes(npcx = 0.02, npcy = 0.95, label = "214986,9755730"), size = 8)

npc_txt_north_reg = geom_text_npc(aes(npcx = 0.97, npcy = 0.8, label = paste("Model =", c1_north, "+", c2_north, "y")), size = 3)
npc_txt_east_reg = geom_text_npc(aes(npcx = 0.97, npcy = 0.8, label = paste("Model =", c1_east, "+", c2_east, "x")), size = 3)
npc_txt_south_reg = geom_text_npc(aes(npcx = 0.02, npcy = 0.8, label = paste("Model =", c1_south, "+", c2_south, "y")), size = 3)
npc_txt_west_reg = geom_text_npc(aes(npcx = 0.02, npcy = 0.8, label = paste("Model =", c1_west, "+", c2_west, "x")), size = 3)


plot.north <- ggplot(data=north, mapping = aes(x = y, y = deg_sum_c_globalimpact)) +
              geom_point(mapping = aes(color = "Scores (points) with linear regression (transparent line)")) +
              geom_line(mapping = aes(color = "Scores (points) with linear regression (transparent line)"), stat="smooth", method="lm", alpha = 0.4) +
              npc_txt_north +
              npc_txt_east_reg +
              scale_colour_manual(values=cols) +
              scale_x_continuous(breaks=seq(9755780,9758280,500)) +
              labs(
                   x = "Y-Coordinate (Y)", 
                   y = expression(paste("Habitat-degradation Score (D"["x"]["y"],")"))) +
                   theme_bw(base_size = 10) + theme(legend.position = "none",
                                                    axis.title.y = element_blank(),
                                                    plot.margin = margin(c(5,5,5,20)))
                                                    
plot.east <- ggplot(data=east, mapping = aes(x = x, y = deg_sum_c_globalimpact)) +
             geom_point(mapping = aes(color = "Scores (points) with linear regression (transparent line)")) +
             geom_line(mapping = aes(color = "Scores (points) with linear regression (transparent line)"), stat="smooth", method="lm", alpha = 0.4) +
             npc_txt_east +
             npc_txt_east_reg +
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
              npc_txt_south +
              npc_txt_south_reg +
              scale_colour_manual(values=cols, name="Habitat degradation") +
              scale_x_continuous(breaks = seq(9753280, 9755780, 500)) + 
              labs(
                   x = "Y-Coordinate (Y)", 
                   y = expression(paste("D"["x"]["y"]))) +
              theme_bw(base_size = 10) + 
              theme(legend.position = "none",
                    plot.margin = margin(c(5,5,5,20)))

plot.west <- ggplot(data=west, mapping = aes(x = x, y = deg_sum_c_globalimpact)) +
             geom_point(mapping = aes(color = "Scores (points) with linear regression (transparent line)")) +
             geom_line(mapping = aes(color = "Scores (points) with linear regression (transparent line)"), stat="smooth", method="lm", alpha = 0.4) +
             npc_txt_west +
             npc_txt_west_reg +
             scale_colour_manual(values=cols) +
             scale_x_continuous(breaks = seq(212486, 214986, 500)) +
             labs(
                  x = "X-Coordinate (X)", 
                  y = expression(paste("D"["x"]["y"],))) +
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

(plot.south + plot.north) / (plot.west + plot.east) / (plot_spacer() + deg + plot_spacer())

# !xyimpact is location of impact: 214986 9755730
ggsave("degradation_landscape.png", path = "{NetVest}/tests_integration/01_unittest/Plots/", scale = 3 )


 
