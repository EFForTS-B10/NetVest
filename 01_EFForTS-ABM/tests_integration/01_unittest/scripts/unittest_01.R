##### Unittest for Natcap Invest Integration ####
#### specify experiment and create folder named after experiment with one input folder and one output folder within
#### specify hsc (half-saturation-constant), default: 0.5
#### Input folder has to include sensitivitytable.txt, impacttable.txt, lulc.asc, oilpalm_c.asc and rubber_c.asc 
#### adapt netlogopath, modelpath, outpath and netlogoversion

#### location of impact from tif or asc
#### influence of 2 impacts




library(nlrx)
library(raster)
library(tidyverse)
library(ggpmisc)

### 1) Unittest execution
experiment <- "forest"
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

## a) Correct execution of unittest: [Success] [Success]

## b) Correct file in output folder
fileexist <- function(qualitymap){
  qualitymap <- qualitymap
  if (qualitymap == TRUE) {print ("Habitat-quality map exists")}  else {print ("Habitat-quality map is missing")}
}

fileexist(qualitymap=file.exists((paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/quality_c_", experiment, ".asc" ,sep=""))))

## c) Checking if transformation from asc to tif and from tif to asc is correct: Location of impact on the correct quadrant of landscape
validation_transformation <- function(inputmap,outputmap){
  transformation <- all.equal(inputmap, outputmap)
  if (transformation == TRUE) {print ("Correct transformation")}  else {print ("No correct transformation")}
}

# asc to tif
validation_transformation(inputmap=raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/input/oilpalm_c.asc" ,sep="")),
                          outputmap=raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/oilpalm_c.tif" ,sep="")))
# tif to asc
validation_transformation(inputmap=raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/quality_c_", experiment, ".tif" ,sep="")),
                          outputmap=raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/quality_c_", experiment, ".asc" ,sep="")))

#inputmap <-raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/input/oilpalm_c.asc" ,sep=""))
#outputmap <-raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/oilpalm_c.tif" ,sep=""))

## Comparison of expected result with result of InVEST
# Generation of expected result
hqcalculation <- function(ry, wr, sumwr, dxy, drmax, bx=bx, Sjr, k, z){
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
              wr=read_csv(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/impact_table.csv" ,sep=""))%>%
                select(WEIGHT) %>%
                .[[1,1]],
              sumwr= read_csv(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/impact_table.csv" ,sep=""))%>%
                select (WEIGHT) %>%
                summarise_all(sum) %>%
                .[[1,1]],
              dxy= 0 ,
              drmax= read_csv(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/impact_table.csv" ,sep=""))%>%
                select(MAX_DIST) %>%
                .[[1,1]],
              #irxy=irxy,
              bx=bx,
              Sjr=read_csv(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/sensitivity_table.csv" ,sep="")) %>%
                select(oilpalm)%>%
                .[[5,1]],
              k=0.5,
              z=2.5)

expectedmap <- raster(ncol=100, nrow= 100, xmn=212461, xmx=217461, ymn=9753255, ymx=9758255)
projection(expectedmap) <- "+proj=utm +zone=48 +south +datum=WGS84"
values(expectedmap) <- 1
plot(expectedmap)
expectedmap[98,80] <- Qxj

# Comparison
validation_maps <- function(investmap,expectedmap){
  equalmaps <- all.equal(investmap, expectedmap)
  if (equalmaps == TRUE) {print ("Expected result")}  else {print ("Unexpected result")}
}

validation_maps(investmap=raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/quality_c_", experiment, ".asc" ,sep="")),
                expectedmap=expectedmap)

## Linear Regression: Checking linear relationship between distance from impact and degradation score
# Expectation: circular and linear decrease of degradation score with increasing distance from impact location

degradationmap_df <- as.data.frame(degradationmap, xy = TRUE)
degradationmap_tib <- as_tibble(degradationmap_df)

south <- degradationmap_tib%>%
  filter(x >= 214936 & x <=214986)%>%
  filter(y <= 9755730)

north <- degradationmap_tib%>%
  filter(x >= 214936 & x <=214986)%>%
  filter(y >= 9755730)

east <- degradationmap_tib%>%
  filter(x >= 214986) %>%
  filter(y >= 9755730 & y <=9755780)

west <- degradationmap_tib%>%
  filter(x <= 214986) %>%
  filter(y >= 9755730 & y <=9755780)

cols <- c("Habitat-degradation scores (points) with linear regression (transparent line)"="tomato3")
npc_txt_south = geom_text_npc(aes(npcx = 0.02, npcy = 0.95, label = "South"), size = 8)
npc_txt_north = geom_text_npc(aes(npcx = 0.98, npcy = 0.95, label = "North"), size = 8)
npc_txt_east = geom_text_npc(aes(npcx = 0.98, npcy = 0.95, label = "East"), size = 8)
npc_txt_west = geom_text_npc(aes(npcx = 0.02, npcy = 0.95, label = "West"), size = 8)


plot.north <- ggplot(data=north, mapping = aes(x = y, y = deg_sum_c_oilpalm_landscape)) +
  geom_point(mapping = aes(color = "Habitat-degradation scores (points) with linear regression (transparent line)")) +
  geom_line(mapping = aes(color = "Habitat-degradation scores (points) with linear regression (transparent line)"), stat="smooth", method="lm", alpha = 0.4) +
  npc_txt_north +
  scale_colour_manual(values=cols) +
  scale_y_continuous(limits = c(0.00002, 0.00009)) +
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
                                   axis.title.y = element_blank())

plot.east <- ggplot(data=east, mapping = aes(x = x, y = deg_sum_c_oilpalm_landscape)) +
  geom_point(mapping = aes(color = "Habitat-degradation scores (points) with linear regression (transparent line)")) +
  geom_line(mapping = aes(color = "Habitat-degradation scores (points) with linear regression (transparent line)"), stat="smooth", method="lm", alpha = 0.4) +
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(rr.label), 
                                 sep = "~~italic(\"with\")~~")),
               eq.with.lhs = "D[x][y]~`=`~",
               eq.x.rhs = "~X",
               formula = y ~ x, 
               parse = TRUE, size = 3, label.y = 0.8, label.x = 0.98) +
  npc_txt_east +
  scale_colour_manual(values=cols) +
  scale_y_continuous(limits = c(0.00002, 0.00009)) +
  scale_x_continuous(breaks = seq(214986, 217486, 500)) +
  labs(
    x = "X-Coordinate (X)", 
    y = expression(paste("Habitat-degradation Score (D"["x"]["y"],")"))) +
  theme_bw(base_size = 10) + theme(legend.position = "none",
                                   axis.title.y = element_blank())

plot.south <- ggplot(data=south, mapping = aes(x = y, y = deg_sum_c_oilpalm_landscape)) +
  geom_point(mapping = aes(color = "Habitat-degradation scores (points) with linear regression (transparent line)")) +
  geom_line(mapping = aes(color = "Habitat-degradation scores (points) with linear regression (transparent line)"), stat="smooth", method="lm", alpha = 0.4) +
  #npc_formula_south +
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(rr.label), 
                                 sep = "~~italic(\"with\")~~")),
               eq.with.lhs = "D[x][y]~`=`~",
               eq.x.rhs = "~Y",
               formula = y ~ x, 
               parse = TRUE, size = 3, label.y = 0.8, label.x = 0.02) +
  npc_txt_south +
  scale_colour_manual(values=cols) +
  scale_y_continuous(limits = c(0.00002, 0.00009)) +
  scale_x_continuous(breaks = seq(9753280, 9755780, 500)) + 
                  #limits = c(9753280, 9755730)) +
  #coord_cartesian(xlim = c(9753280, 9755730)) +
  labs(
    x = "Y-Coordinate (Y)", 
    y = expression(paste("Habitat-degradation Score (D"["x"]["y"],")"))) +
  theme_bw(base_size = 10) + 
  theme(legend.position = "none")

plot.west <- ggplot(data=west, mapping = aes(x = x, y = deg_sum_c_oilpalm_landscape)) +
  geom_point(mapping = aes(color = "Habitat-degradation scores (points) with linear regression (transparent line)")) +
  geom_line(mapping = aes(color = "Habitat-degradation scores (points) with linear regression (transparent line)"), stat="smooth", method="lm", alpha = 0.4) +
  stat_poly_eq(aes(label = paste(stat(eq.label), stat(rr.label), 
                                 sep = "~~italic(\"with\")~~")),
               eq.with.lhs = "D[x][y]~`=`~",
               eq.x.rhs = "~X",
               formula = y ~ x, 
               parse = TRUE, size = 3, label.y = 0.8, label.x = 0.02) +
  npc_txt_west +
  scale_colour_manual(values=cols) +
  scale_y_continuous(limits = c(0.00002, 0.00009)) +
  scale_x_continuous(breaks = seq(212486, 214986, 500)) +
  #coord_cartesian(xlim = c(212486, 214886)) +
  labs(
    x = "X-Coordinate (X)", 
    y = expression(paste("Habitat-degradation Score (D"["x"]["y"],")"))) +
  theme_bw(base_size = 10) + 
  theme(legend.position = "none")

#Arrage plots
# coordinate of impact: X=214986, Y=9755780
grid.arrange(
  plot.north,
  plot.east,
  plot.south,
  plot.west,
  nrow=2,
  #heights= c(1,1,1),
  #widths= c(0.5,1,1),
  layout_matrix = rbind(c(3, 1),
                        c(4, 2)),
  top= "Linear relationship between habitat-degradation score and distance from source of impact"
)

degradationmap <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/deg_sum_c_", experiment, ".tif" ,sep=""))
plot(degradationmap)
degradationmap
investmap <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/quality_c_", experiment, ".tif" ,sep=""))
plot(investmap)
investmap

#weg## Input
#weg#
 #lulc <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/input/lulc.asc" ,sep=""))
#weg#
 #plot(lulc)
#weg#
 #op <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/input/oilpalm_c.asc" ,sep=""))
#weg#
 #plot(op)
 #optif <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/oilpalm_c.tif" ,sep=""))
#plot(optif)
 #weg#
 #rb <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/input/rubber_c.asc" ,sep="")) 
#weg#
  #plot(rb)
  

#cell <- cellFromRowCol(op, 1, 1:5) 
#extract(op, cell)
  
#cell <- cellFromRowCol(investmap, 98, 80)
#v <- getValues(investmap, 98)
#v[80]
#extract(investmap, cell)


## InVEST map
#investmap <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/quality_c_", experiment, ".asc" ,sep=""))
# Creating dataframe of rasterdata
#investmap_df <- as.data.frame(investmap, xy = TRUE)
# Plot raster
#str(investmap_df)
#res(investmap)
#plot(investmap)

 
## Expected map 
# Expected hq based on calculation
# was sich aendert, ist: wk, dxy (aus raster holen), drmax, Sjr


#wr <-read_csv(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/impact_table.csv" ,sep=""))%>%
#  select(WEIGHT) %>%
#  .[[1,1]] 

#Sjr <- read_csv(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/sensitivity_table.csv" ,sep="")) %>%
#  select(oilpalm)%>%
#  .[[5,1]]

#Idee: create point layer from matrix (with same dimension and proj as expctedmap), calculate distance
# Create a point layer
#xy <- matrix(c(60,48),nrow=1, byrow=T) 
#p1 <- SpatialPoints(xy)
#crs(p1) <- "+proj=utm +zone=48 +south +datum=WGS84" 
#expectedmap.d <- distanceFromPoints(expectedmap, p1)

#dxy=if (drmax== 0.05) {0} else {0.1} 

## Expected map
#expectedmap <- raster(ncol=100, nrow= 100, xmn=212461, xmx=217461, ymn=9753255, ymx=9758255)
#projection(expectedmap) <- "+proj=utm +zone=48 +south +datum=WGS84"
#values(expectedmap) <- 1
#expectedmap[60,80] <- hqcalculation

## Comparison



#compareRaster(investmap, expectedmap, values = TRUE)
#all.equal(investmap, expectedmap)


#validation_mean <- function(investmap=investmap,expectedmap=expectedmap){
 #x <- cellStats(investmap, mean)
 #y <- cellStats(expectedmap, mean)
 #if (identical(x, y)) {print ("Unittest validated")}  else {print ("Unittest not validated")}
#}

#validation_mean(investmap,expectedmap)  


#validation_cell <- function(investmap=investmap, hqcalculation=hqcalculation){
 #       cell <- cellFromRowCol(investmap, 60, 48)
  #      x <- extract(investmap, cell)
   #     y <- hqcalculation
#if (x == y) {print ("Unittest validated")}  else {print ("Unittest not validated")}
        
#}

#validation_plantation(investmap)
# additional possible steps to analyze results via NLRX - not required for unit test
# Attach results to nl object:
# setsim(nl, "simoutput") <- results
# Write output to outpath of experiment within nl
# write_simoutput(nl)
# Do further analysis:
# analyze_nl(nl)

