###Calculation of alpha diversity of understorey plant species per land-use type for examination of InVEST parameter impact weighting###

#alpha diversity
library(tidyverse)
sm<- data.frame(read.csv("C:/Users/JuliaHenzler/Documents/01_GitHub/EFForTS-ABM/00_Parameterization/01_ImpactWeighting/understorey_plants/B06_Plants_plot.csv", header=TRUE, sep=",", dec = ".", fill=TRUE, na.strings="NA"))%>%
gather(key=plotID, value=abundance, 2:ncol(.)) %>% 
  mutate(plotID = substr(plotID, 2,2)) %>% 
  group_by(Species, plotID) %>% 
  summarise(abundance_sum = sum(abundance))


sm.f <- subset(sm, sm$plotID=="F")
alpha.f <- nrow(sm.f[sm.f$abundance_sum >= 1, ])

sm.o <- subset(sm, sm$plotID=="O")
alpha.o<-nrow(sm.o[sm.o$abundance_sum >= 1, ])

sm.r <- subset(sm, sm$plotID=="R")
alpha.r <- nrow(sm.r[sm.r$abundance_sum >= 1, ])

#impact weighting (w)
alpha.sum <- alpha.o + alpha.r

w.oilpalm <- 1/alpha.o

w.rubber <- 1/alpha.r
