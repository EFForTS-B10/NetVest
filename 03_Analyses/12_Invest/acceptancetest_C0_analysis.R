### Acceptance Test with C0 Scenario of Dislich 2018

library(tidyverse)
library(gridExtra)
library(ggpubr)
library(raster)

#Read in csv and create tibble
biodiv_C0<-read_csv(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/natcap_invest/output/",experiment,"_simple.csv",sep = "")) %>%
  rename(step="[step]")

#LUT-fractions: plot
biodiv_C0_lutfractions <- biodiv_C0 %>% 
  dplyr::select(step, lut0.fraction, lut1.fraction) %>%
  group_by(step) %>%
  summarise_all(list(mean, sd)) %>%
  ggplot(aes(x = step)) +
  geom_ribbon(aes(ymin = lut0.fraction_fn1 - lut0.fraction_fn2, ymax = lut0.fraction_fn1 + lut0.fraction_fn2), fill = "indianred3", alpha = 0.5) +
  geom_line(aes(y = lut0.fraction_fn1, colour = "indianred3")) +
  geom_ribbon(aes(ymin = lut1.fraction_fn1 - lut1.fraction_fn2, ymax = lut1.fraction_fn1 + lut1.fraction_fn2), fill = "darkgoldenrod1", alpha = 0.5) +
  geom_line(aes(y = lut1.fraction_fn1, colour = "darkgoldenrod1")) +
  scale_color_identity(guide = "legend", labels = c("rubber", "oilpalm"), name = "Fraction of") +
  scale_x_continuous(limits = c(1, 50)) +
  labs(
    x = "Simulation time", # x axis title
    y = "Land-use type fractions") +   # y axis title
    #title = "Temporal dynamics under constant price scenarios") +     # main title of figure
  theme_light(base_size = 7) + theme(axis.title.x = element_text(size = 5),
                                     axis.title.y = element_text(size = 5),
                                     axis.text = element_text(size = 2.5),
                                     legend.title = element_blank (), #element_text(size = 4, face = "bold"),
                                     legend.text = element_text(size = 3),
                                     legend.key.width = unit(0.3, "cm"),
                                     legend.key.height = unit(0.3, "cm"),
                                     legend.position = "right",
                                     aspect.ratio = 1,
                                     #plot.margin = margin(c(5,0,5,5)))
)

#Habitat quality: plot
biodiv_C0_hq <- biodiv_C0 %>% 
  dplyr::select(step, p.hq) %>%
  group_by(step) %>%
  summarise_all(list(mean, sd)) #%>%
  ggplot(aes(x = step)) +
  geom_ribbon(aes(ymin = fn1 - fn2, ymax = fn1 + fn2), fill = "#238443", alpha = 0.5) +
  geom_line(aes(y = fn1, color = "#238443")) +
  geom_segment(
    x = 5, y = 0.805,
    xend = 5, yend = 0.815,
    size = 0.3, 
    arrow = arrow(length = unit(0.1, "inches")),
    colour = "#238443" ) +
  geom_segment(
    x = 20, y = 0.850,
    xend = 20, yend = 0.860,
    size = 0.3, 
    arrow = arrow(length = unit(0.1, "inches")),
    colour = "#238443" ) +
  scale_color_identity(guide = "legend", labels = "Biodiversity", name = "Biodiversity") +
  scale_x_continuous(limits = c(1, 50)) +
  labs(x = "Simulation time",  
       y = "Habitat Quality Score") +   
  #title = "Temporal dynamics under constant price scenarios") +    
  theme_light(base_size = 7) +
  theme(axis.title.x = element_text(size = 5),
        axis.title.y = element_text(size = 5),
        axis.text = element_text(size = 2.5),
        legend.title = element_blank(),#,#(size = 4, face = "bold"),
        legend.text = element_text(size = 3),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.position = "right",
                                     aspect.ratio = 1,
                                     #plot.margin = margin(c(5,-12,5,8)))
)


## Habitat quality maps and histogram of hq values in year 5 and 20
files_path <- "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/output/" #path where my files are    

#year 5: histogram
luthq005_files = intersect(list.files(files_path, full.names = TRUE, pattern = "lut_quality"), list.files(files_path, full.names = TRUE, pattern = "005.asc$")) 
luthq005_stack <- stack(luthq005_files) #stack them together  
luthq005_stack_df <- as.data.frame(luthq005_stack, xy=TRUE) #create dataframe of all repitions in year 5
luthq005_hist <- 
  luthq005_stack_df %>% 
  dplyr::select(-c(x,y)) %>%
  pivot_longer(cols = c(1:ncol(.)), names_to = "repititions", values_to = "hq") %>%
  mutate(category = cut(hq, breaks=c(-Inf, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, Inf))) %>%
  group_by(repititions) %>%
  count(category) %>%
  ungroup() %>%
  add_row(repititions = "lut_quality_C0_8_005", category = "(0.65,0.7]") %>%
  add_row(repititions = "lut_quality_C0_9_005", category = "(0.65,0.7]") %>%
  add_row(repititions = "lut_quality_C0_10_005", category = "(0.65,0.7]") %>%
  add_row(repititions = "lut_quality_C0_12_005", category = "(0.65,0.7]") %>%
  add_row(repititions = "lut_quality_C0_13_005", category = "(0.65,0.7]") %>%
  add_row(repititions = "lut_quality_C0_14_005", category = "(0.65,0.7]") %>%
  add_row(repititions = "lut_quality_C0_15_005", category = "(0.65,0.7]") %>%
  add_row(repititions = "lut_quality_C0_17_005", category = "(0.65,0.7]") %>%
  add_row(repititions = "lut_quality_C0_19_005", category = "(0.65,0.7]") %>%
  group_by(category) %>%
  summarise(mean=mean(n), sd=sd(n)) #%>%
  ggplot(aes(category,mean)) +
  geom_bar (stat = "identity", fill="#7EA6E0") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                  position=position_dodge(0.9)) +
    #scale_y_continuous(limits=c(0,2000)) +
    #coord_cartesian(ylim=c(0,2000), oob = rescale_none) +
  scale_x_discrete(breaks=c("(-Inf,0.05]", "(0.05,0.1]", "(0.1,0.15]", "(0.15,0.2]", "(0.2,0.25]", "(0.25,0.3]", "(0.3,0.35]", "(0.35,0.4]", "(0.4,0.45]", "(0.45,0.5]", "(0.5,0.55]", "(0.55,0.6]", "(0.6,0.65]", "(0.65,0.7]", "(0.7,0.75]", "(0.75,0.8]", "(0.8,0.85]", "(0.85,0.9]", "(0.9,0.95]", "(0.95, Inf]"),
                     labels=c("0.05", "0.1", "0.15", "0.2", "0.25", "0.3", "0.35", "0.4", "0.45", "0.5", "0.55", "0.6", "0.65", "0.7", "0.75", "0.8", "0.85", "0.9", "0.95", "1")) +
  labs(x = "Habitat Quality Score",  
       y = "Counts") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 5),
        axis.title.y = element_text(size = 5),
        axis.text = element_text(size = 2.5),
  #plot.margin = margin(c(5, 1,5, 20))
)

#year 5: hq up to 0.25  
luthq005_lowhq <- luthq005_stack_df %>% 
  dplyr::select(-c(x,y)) %>%
  pivot_longer(cols = c(1:ncol(.)), names_to = "repititions", values_to = "hq") %>%
  mutate(category = cut(hq, breaks=c(-Inf,  0.25, Inf))) %>%
  group_by(repititions) %>%
  count(category) %>%
  filter(category == "(-Inf,0.25]") %>%
  summarise(mean=mean(n), sd=sd(n))

#year 5: raster map
hq005 <- raster("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/output//lut_quality_C0_1_005.asc")
hq005_df <- as.data.frame(hq005, xy=TRUE)
hqmap005 <- 
  ggplot(data=hq005_df) + 
  geom_raster(aes(x=x,y=y,fill=lut_quality_C0_1_005)) +  
  scale_fill_gradient(low = "#ffffe5", high = "#00441b", name = "Habitat-Quality Scores ") +
  xlab("Longitude (X)") + ylab("Latitude (Y)") + #ggtitle("Generated habitat-quality map") +
  theme (plot.title = element_text(hjust = 0.1, size = 6),
         axis.title.x = element_text(hjust=1, vjust= -1, size = 5),
         axis.title.y = element_text(hjust=1, vjust= 2, size = 5),
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
         plot.margin = margin(c(5,1,5,20)),
         legend.spacing.x = unit(0.2, "cm")
  )
  
#year 20:histogram  
luthq020_files = intersect(list.files(files_path, full.names = TRUE, pattern = "lut_quality"), list.files(files_path, full.names = TRUE, pattern = "020.asc$")) #, list.files(files_path, full.names = TRUE, pattern = "005"))
luthq020_stack <- stack(luthq020_files) #stack them together  
luthq020_stack_df <- as.data.frame(luthq020_stack, xy=TRUE) #create dataframe of all repititions in year 5
luthq020_hist <- luthq020_stack_df %>% 
  dplyr::select(-c(x,y)) %>%
  pivot_longer(cols = c(1:ncol(.)), names_to = "repititions", values_to = "hq") %>%
  mutate(category = cut(hq, breaks=c(-Inf, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, Inf))) %>%
  group_by(repititions) %>%
  count(category) %>%
  group_by(category) %>%
  summarise(mean=mean(n), sd=sd(n)) %>%
  ggplot(aes(category,mean)) +
  geom_bar (stat = "identity", fill="#7EA6E0") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  scale_x_discrete(breaks=c("(-Inf,0.05]", "(0.05,0.1]", "(0.1,0.15]", "(0.15,0.2]", "(0.2,0.25]", "(0.25,0.3]", "(0.3,0.35]", "(0.35,0.4]", "(0.4,0.45]", "(0.45,0.5]", "(0.5,0.55]", "(0.55,0.6]", "(0.6,0.65]", "(0.65,0.7]", "(0.7,0.75]", "(0.75,0.8]", "(0.8,0.85]", "(0.85,0.9]", "(0.9,0.95]", "(0.95, Inf]"),
                   labels=c("0.05", "0.1", "0.15", "0.2", "0.25", "0.3", "0.35", "0.4", "0.45", "0.5", "0.55", "0.6", "0.65", "0.7", "0.75", "0.8", "0.85", "0.9", "0.95", "1")) +
  labs(x = "Habitat Quality Score",  
       y = "Counts") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 5),
        axis.title.y = element_text(size = 5),
        axis.text = element_text(size = 2.5),
  #plot.margin = margin(c(5,-10,5,1))
  )

#year 20: hq up to 0.25
luthq020_lowhq <- luthq020_stack_df %>% 
  dplyr::select(-c(x,y)) %>%
  pivot_longer(cols = c(1:ncol(.)), names_to = "repititions", values_to = "hq") %>%
  mutate(category = cut(hq, breaks=c(-Inf,  0.25, Inf))) %>%
  group_by(repititions) %>%
  count(category) %>%
  filter(category == "(-Inf,0.25]")

#year 20: raster map
hq020 <- raster("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/output//lut_quality_C0_1_020.asc" )
hq020_df <- as.data.frame(hq020, xy=TRUE)
hqmap020 <- 
  ggplot(data=hq020_df) + 
  geom_raster(aes(x=x,y=y,fill=lut_quality_C0_1_020)) +  
  scale_fill_gradient(low = "#ffffe5", high = "#00441b", name = "Habitat-Quality Scores ") +
  xlab("Longitude (X)") + ylab("Latitude (Y)") + #ggtitle("Generated habitat-quality map") +
  theme (plot.title = element_text(hjust = 0.1, size = 6),
         panel.background = element_rect(fill = "white"),
         panel.grid.major = element_line(color = "gray90", size = 0.2),
         panel.grid.minor = element_line(color = "gray90", size = 0.2),
         axis.title.x = element_text(hjust=1, vjust= -1, size = 5),
         axis.title.y = element_text(hjust=1, vjust= 2, size = 5),
         axis.text = element_text(size = 2.5),
         axis.ticks = element_line(size = 0),
         legend.title = element_text(size = 4, face = "bold"),
         legend.text = element_text(size = 4),
         legend.key.width = unit(0.3, "cm"),
         legend.key.height = unit(0.3, "cm"),
         legend.position = "bottom",
         aspect.ratio = 1,
         plot.margin = margin(c(5,-20,5,1)),
         legend.spacing.x = unit(0.2, "cm")
  )

##Statistical methods
#t-test paired, one-sided for significant difference of hq values up to 0.25 between year 5 and year 20
t.test(luthq005_lowhq$n, luthq020_lowhq$n, paired = TRUE, alternative = "greater")

##Arrange plots
library(ggpubr)
ggarrange(biodiv_C0_lutfractions, biodiv_C0_hq,  ncol = 1, nrow = 2, align = "v", labels = "AUTO") 
ggsave("acceptancetest_plot.png", path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/03_acceptancetest/Plots/" )#, plot="plot1",device = png) #, path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/Plots/")

ggarrange(hqmap005, hqmap020, luthq005_hist, luthq020_hist, ncol = 2, nrow = 2, align = "hv", labels = "AUTO", common.legend = TRUE, legend="top") 
ggsave("acceptancetest_maps.png", path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/03_acceptancetest/Plots/" )#, plot="plot1",device = png) #, path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/Plots/")


#######################################################################################
# Consumption
biodiv_C0_consumption <- biodiv_C0 %>% 
  select(step, hh.consumption.mean) %>%
  group_by(step) %>%
  summarise_all(list(mean, sd)) %>%
  #rename(mean_consumption = "fn1", sd_consumption = "fn2") %>%
  ggplot(aes(x = step)) +
  geom_ribbon(aes(ymin = fn1 - fn2, ymax = fn1 + fn2), fill = "blue4", alpha = 0.5) +
  geom_line(aes(y = fn1, color = "blue4")) +
  scale_color_identity(guide = "legend", labels = "consumption", name = "Economic function") +
  scale_x_continuous(limits = c(1, 50)) +
  labs(
    x = "Simulation time", 
    y = "Mean Consumption (USD)") +
  #title = "Temporal dynamics under constant price scenarios") +     
  theme_light(base_size = 7) + theme(legend.title = element_text(size = 4, face = "bold"),
                                     legend.text = element_text(size = 4),
                                     legend.key.width = unit(0.3, "cm"),
                                     legend.key.height = unit(0.3, "cm"),
                                     legend.position = "right",
                                     #aspect.ratio = 1,
                                     plot.margin = margin(c(5,0,5,5)))

# Carbon
biodiv_C0_carbon <-biodiv_C0 %>% 
  select(step, lut0.carbon, lut1.carbon, area_under_agriculture) %>%
  mutate(lut0.carbon_agr = lut0.carbon/area_under_agriculture, lut1.carbon_agr = lut1.carbon/area_under_agriculture) %>%
  group_by(step) %>%
  summarise_all(list(mean, sd)) %>%
  ggplot(aes(x = step)) +
  geom_ribbon(aes(ymin = lut0.carbon_agr_fn1 - lut0.carbon_agr_fn2, ymax = lut0.carbon_agr_fn1 + lut0.carbon_agr_fn2), fill = "indianred3", alpha = 0.5) +
  geom_line(aes(y = lut0.carbon_agr_fn1, colour = "indianred3")) +
  geom_ribbon(aes(ymin = lut1.carbon_agr_fn1 - lut1.carbon_agr_fn2, ymax = lut1.carbon_agr_fn1 + lut1.carbon_agr_fn2), fill = "darkgoldenrod1", alpha = 0.5) +
  geom_line(aes(y = lut1.carbon_agr_fn1, colour = "darkgoldenrod1")) +
  scale_color_identity(guide = "legend", labels = c("rubber", "oilpalm"), name = "Carbon in biomass") +
  scale_x_continuous(limits = c(1, 50)) +
  labs(
    x = "Simulation time",  # x axis title
    y = "Carbon (Tons/ha)") +   # y axis title
  #title = "Temporal dynamics under constant price scenarios") +     # main title of figure
  theme_light(base_size = 7) + theme(legend.title = element_text(size = 4, face = "bold"),
                                     legend.text = element_text(size = 4),
                                     legend.key.width = unit(0.3, "cm"),
                                     legend.key.height = unit(0.3, "cm"),
                                     legend.position = "right",
                                     #aspect.ratio = 1,
                                     plot.margin = margin(c(5,-12,5,8)))


#arrow example
ggplot(iris) +
  geom_segment(
    x = 1, y = 1,
    xend = 4, yend = 7,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = 2, 
    arrow = arrow(length = unit(0.3, "inches")),
    colour = "#EC7014" # Also accepts "red", "blue' etc
  ) 

missingvalues_frac <- biodiv_C0 %>% 
  dplyr::select(step, lut0.fraction, lut1.fraction) %>%
  group_by(step) %>%
  summarise_all(list(mean, sd))

missingvalues_bio <- biodiv_C0 %>% 
  dplyr::select(step, p.hq) %>%
  group_by(step) %>%
  summarise_all(list(mean, sd))






luthq005 = list.files(luthq_files, pattern = "_005")
all_files <- list.files(files_path,
                        full.names = TRUE,
                        pattern = ".asc$") #take all the ascii files in    
files_stack <- stack(all_files) #stack them together    
luthq_files = gsub("lut_quality.*.asc","",all_files)    #gsub(".*mass-","",all_files))
luthq_files

write.csv(luthq005_hist,"/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/03_acceptancetest/Plots/hist005.csv", row.names = FALSE)
