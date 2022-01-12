###Calculation of alpha diversity of understorey plant species per land-use type###

library(tidyverse)
sm<- data.frame(read.csv("C:/Users/JuliaHenzler/Documents/01_GitHub/EFForTS-ABM/00_Parameterization/00_Plants_alphadiversity/B06_Plants_plot.csv", header=TRUE, sep=",", dec = ".", fill=TRUE, na.strings="NA"))%>%
gather(key=plotID, value=abundance, 2:ncol(.)) %>% 
  mutate(plotID = substr(plotID, 2,2)) %>% 
  group_by(Species, plotID) %>% 
  summarise(abundance_sum = sum(abundance))


sm.f <- subset(sm, sm$plotID=="F")
alpha.f <- nrow(f[f$abundance_sum >= 1, ])

sm.o <- subset(sm, sm$plotID=="O")
alpha.o<-nrow(o[o$abundance_sum >= 1, ])

sm.r <- subset(sm, sm$plotID=="R")
alpha.r <- nrow(r[r$abundance_sum >= 1, ])

