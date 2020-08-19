####Rarefaction: standardize disparate sampling techniques and effort
##rarefy: Expected species richness in random subsamples 
##rrrafey: Randomly rarified community data frame of given sample size (sm): one possible and random occupancy so that sum equals subsample 
##drarefy: Probabilities that species occur in a rarified community: Occupancies of species 1/all possible occupancies  
setwd("C:\\Users\\JuliaHen\\Documents\\3_EFForTS\\Data\\plants_and_env")
library(vegan)
library(tidyverse)

###Variant 1###
##1. Generate species matrix (sm) for each landuse
##2. Calculate sub-sample (smallest sample size) over all landuses
##2. Apply rarefaction for each landuse

##1.Generate species matrix of type row=site=landuse, column=species 
sm<- data.frame(read.csv("B06_Plants_plot.csv", header=TRUE, sep=";", dec = ",", fill=TRUE, na.strings="NA"))%>%
  #column names into a key column, column values into single value column, selecting columns 2 to end of columns
  gather(key=plotID, value=abundance, 2:ncol(.)) %>% 
  #replacement of plotID (e.g. HF0) by only second (2,2) letter (e.g. F)
  mutate(plotID = substr(plotID, 2,2)) %>% 
  #grouping without changing how data looks like, only changes how it acts with other dplyr verbs
  group_by(Species, plotID) %>% 
  #all values grouped of species are summed up for each plotID
  summarise(abundance_sum = sum(abundance)) %>% 
  #increasing number of columns by names_from = species 
  pivot_wider(names_from = Species, values_from = abundance_sum) %>% 
  #column plotID gets to rownames
  column_to_rownames("plotID")

specnumber<-c(963,0,652,0,219,0,230,0)
#specnumber(sm) #species richness per site

#range(rowSums(sm)) #range of values (min - max)

#summing up all individuals for each landuse 
#rowSums(sm)

##2.calculate sub-sample 
individuals.min<-min(rowSums(sm))

##3.rarefaction
#3.1 rarefy: calculate expected species in a rarefied sample
rarefy1<-rarefy(sm, individuals.min, se=TRUE) %>%
  as.data.frame(.) %>%
  rownames_to_column ("rarefy") %>%
  gather(key=PlotID, value=expected_richness1,2:ncol(.)) %>%
  select(PlotID,rarefy,expected_richness1)

rarefy1.1<-data.frame(rarefy1,specnumber)

#3.2 rrarefy: calculate one possible rarified community
rrarefy1<-rrarefy(sm, individuals.min) %>%
  as.data.frame(.) %>%
  rownames_to_column("PlotID") %>%
  pivot_longer(cols= -PlotID, names_to = "species", values_to = "value")%>%
  select(PlotID,species,value) %>%
  group_by(PlotID, species)%>%
  summarise(abundance1=value)

#3.3 drarefy: calculate probabilities that species occurs in rarefied community
drarefy1<-drarefy(sm,individuals.min) %>%
  as.data.frame(.) %>%
  rownames_to_column("PlotID") %>%
  pivot_longer(cols = -PlotID, values_to = "prob_v1")


###Variant 2### --> favorite!
##1. Generate species matrix for each plot
##2. Calculate sub-sample within each landuse (smallest number of individuals per landuse)
##3. Apply rarefaction to each landuse seperately
##4. Calculate expected species/probabilites for each landuse

#1. Generate species matrix for each plot
sm<-data.frame(read.csv2("B06_Plants_plot.csv")) %>%
  gather(key = "PlotID", value = "abundance", 2:ncol(.)) %>%
  pivot_wider(names_from = Species, values_from = abundance) %>%
  mutate(PlotID = substr(PlotID, 2,2))
  
#Generate species matrix for each landuse seperatly
#forest
sm.f<-subset(sm,sm$PlotID=="F")%>%
  as.data.frame(.)%>%
  select(-PlotID)
#sum.f<-rowSums(sm.f)%>%
  #as.data.frame%>%
  #gather(key="a", value="abundance")%>%
  #select(-a)%>%
  #rownames_to_column("Plotnumber")
  

#ggplot(sum.f)+
  #geom_point(aes(x=Plotnumber,y=abundance))

#junglerubber
sm.j<-subset(sm,sm$PlotID=="J")%>%
  as.data.frame(.)%>%
  select(-PlotID)
#oilpalm
sm.o<-subset(sm,sm$PlotID=="O")%>%
  as.data.frame(.)%>%
  select(-PlotID)
max(rowSums(sm.o))
#rubber
sm.r<-subset(sm,sm$PlotID=="R")%>%
  as.data.frame(.)%>%
  select(-PlotID)

#plot number of individuals for each landuse
graph<-data.frame(read.csv2("B06_Plants_plot.csv")) %>%
  gather(key = "PlotID", value = "abundance", 2:ncol(.)) %>%
  pivot_wider(names_from = Species, values_from = abundance) %>%
  column_to_rownames("PlotID")
graph<-data.frame(graph,rowSums(graph))%>%
  rownames_to_column("PlotID")%>%
  mutate(landuse=PlotID)%>%
  mutate(landuse=substr(landuse,2,2))%>%
  mutate(sum_abundance=rowSums.graph.)%>%
  mutate(rowSums.graph.=NULL)%>%
  select(PlotID,landuse,sum_abundance, everything())

ggplot(graph)+
  geom_point(aes(x=PlotID, y=sum_abundance,color=landuse))
               
#mean and se for abundances per landuse for confirming methodology
#graph.mean<-graph%>%
  #group_by(landuse)%>%
  #mutate(mean_abundance=mean(sum_abundance))%>%
  #mutate(se_abundance=sd(sum_abundance))
  
#ggplot(graph.mean)+
  #geom_point(aes(x=landuse,y=mean_abundance))+
  #geom_point(aes(x=landuse,y=se_abundance, color="red"))
pdf("plant_abundance_lu_boxplot.pdf")
ggplot(graph)+
  geom_boxplot(aes(x=landuse,y=sum_abundance))+
  ggtitle("Abundance differences between landuses for plants")+
  xlab("Landuse")+
  ylab("Abundances")+
  theme_bw()+
  theme(axis.title=element_text(size=rel(1.5)))+
  theme(plot.title=element_text(size=rel(1.5)))+
  theme(axis.text =element_text(size=rel(1.3)))
dev.off()


#2. Calculate sub-sample 
individuals.minf<-min(rowSums(sm.f)) #forest
individuals.minj<-min(rowSums(sm.j)) #junglerubber
individuals.mino<-min(rowSums(sm.o)) #oilpalm
individuals.minr<-min(rowSums(sm.r)) #rubber


#3. rarefaction
#3.1 rarefy: calculate expected species in a rarefied sample for each landuse
#forest
rarefy.f<-rarefy(sm.f,individuals.minf, se=TRUE) %>%
  as.data.frame(.) %>%
  rownames_to_column("rarefy") %>%
  gather(key=plotnumber, value=richness, 2:ncol(.)) %>%
  select(plotnumber,rarefy,richness)%>%
  mutate(plotnumber=substr(plotnumber,2,2))%>%
  pivot_wider(names_from = plotnumber, values_from = richness)
#jungerubber
rarefy.j<-rarefy(sm.j,individuals.minj, se=TRUE) %>%
  as.data.frame(.) %>%
  rownames_to_column("rarefy") %>%
  gather(key=plotnumber, value=richness, 2:ncol(.)) %>%
  select(plotnumber,rarefy,richness)%>%
  mutate(plotnumber=substr(plotnumber,2,2))%>%
  pivot_wider(names_from = plotnumber, values_from = richness)
#oilpalm
rarefy.o<-rarefy(sm.o,individuals.mino, se=TRUE) %>%
  as.data.frame(.) %>%
  rownames_to_column("rarefy") %>%
  gather(key=plotnumber, value=richness, 2:ncol(.)) %>%
  select(plotnumber,rarefy,richness)%>%
  mutate(plotnumber=substr(plotnumber,2,2))%>%
  pivot_wider(names_from = plotnumber, values_from = richness)
#rubber
rarefy.r<-rarefy(sm.r,individuals.minr, se=TRUE) %>%
  as.data.frame(.) %>%
  rownames_to_column("rarefy") %>%
  gather(key=plotnumber, value=richness, 2:ncol(.)) %>%
  select(plotnumber,rarefy,richness)%>%
  mutate(plotnumber=substr(plotnumber,2,2))%>%
  pivot_wider(names_from = plotnumber, values_from = richness)

#plot rariefied species richness for each landuse
rarecurve(sm.f, step = 1, individuals.minf, lable=FALSE, col="green4")
rarecurve(sm.j, step=1, individuals.minj, label = FALSE, col = "green3")
rarecurve(sm.o, step=1, individuals.mino, label = FALSE, col = "greenyellow")
rarecurve(sm.r, step=1, individuals.minr, label = FALSE, col = "orange")



#3.2 rrarefy: calculate rarified community
#forest
rrarefy2.f<-rrarefy(sm.f, individuals.minf) %>%
  as.data.frame(.) %>%
  rownames_to_column("plot")%>%
  #mutate(PlotID=substr(PlotID,2,2))%>%
  pivot_longer(cols = -plot)%>%
  group_by(name,plot)%>%
  summarise(abundance2=sum(value))#%>%
  #pivot_wider(names_from = name, values_from = value)

#3.3 drarefy: probabilities that species occurs in rarified community
#forest
drarefy.f<-drarefy(sm.f,individuals.minf) %>%
  as.data.frame(.) %>%
  rownames_to_column("number_plots")%>%
  pivot_longer(cols = -number_plots) %>%
  group_by(name) %>%
  summarise(prob_forest=mean(value))

#junglerubber
drarefy.j<-drarefy(sm.j,individuals.minj) %>%
as.data.frame(.) %>%
  rownames_to_column("number_plots")%>%
  pivot_longer(cols = -number_plots) %>%
  group_by(name) %>%
  summarise(prob_jungle=mean(value))

#oilpalm
drarefy.o<-drarefy(sm.o,individuals.mino) %>%
  as.data.frame(.) %>%
  rownames_to_column("number_plots")%>%
  pivot_longer(cols = -number_plots) %>%
  group_by(name) %>%
  summarise(prob_oilpalm=mean(value))

#rubber
drarefy.r<-drarefy(sm.r,individuals.minr) %>%
  as.data.frame(.) %>%
  rownames_to_column("number_plots")%>%
  pivot_longer(cols = -number_plots) %>%
  group_by(name) %>%
  summarise(prob_rubber=mean(value))

#summarize all dataframes
#rarefy<-rarefy1 %>% left_join(rarefy2)

#rrarefy<-rrarefy1 %>% left_join(rrarefy2)
  
drarefy<-drarefy.f %>% left_join(drarefy.j) %>% left_join(drarefy.o) %>% left_join(drarefy.r)

#save rarefied probabilities
write.table(drarefy, file="plants_rarefied_probs.txt")


###Variant3
##1.Generate species matrix(sm) for each plot
##2. Calculate sub-sample over alls plots (smallest number of individuals)
##3. Apply rarefaction to each plot
##4. Calculate expected species/probabilites for each landuse

#1.
sm<-data.frame(read.csv2("B06_Plants_plot.csv")) %>%
  gather(key = "PlotID", value = "abundance", 2:ncol(.)) %>%
  pivot_wider(names_from = Species, values_from = abundance) %>%
  column_to_rownames("PlotID")
  
#2. sub-sample
individuals.min<-min(rowSums(sm))

#3.
#rarefy
rarefy<-rarefy(sm,individuals.min, se=TRUE) %>%
  as.data.frame(.) %>%
  rownames_to_column("rarefy") #%>%
  gather(key=PlotID, value=richness, 2:ncol(.)) %>%
  select(PlotID,rarefy,richness)#%>%
  #mutate(plotnumber=substr(plotnumber,2,2))%>%
  pivot_wider(names_from = PlotID, values_from = richness)

s<-subset(rarefy,rarefy=="S")%>%
  gather(key=names, value = richness, 2:ncol(.))

ggplot(s)+
  geom_point(aes(x=names, y=richness))

rarecurve(sm, step = 1, sample = individuals.min, label = TRUE)





#number of species per site (sum up number of non-zero entries per row (1))
n<-apply(sp>0,1,sum)
#number of individuals per site (sum up number of entries per row (1))
N<-apply(sp,1,sum)
#Menhinick index (sample size)
menhinick<-n/sqrt(N)
#Margalef's index (sample size)
margalefs<-(n-1)/log(N)

