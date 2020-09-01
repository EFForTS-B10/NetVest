


## For each run We have results for 50 years and 3 random-seeds
#
# 
# The landscape statts with a lot of rubber
# 
# 
# Main questions
#
# Classify final landscapes:
# if rubber_area is >= 75 % -> rubber dominated
# if oilpalm_area is >= 75 % -> oilpalm dominated
# otherwise -> mix
#
# We take the mean of the last 20 steps to calculate the area:


########################################################################
## Load packages:
library(ggthemes)
library(nlrx)
library(tidyverse)
library(grid)
library(ggridges)
library(Refforts)
########################################################################
## Load simulation data
nl <- readRDS(file = file.path(getwd(), "03_Analyses/07_landmarket_v3/constantprices_ff_sensitivity.rds"))
results <- nl@simdesign@simoutput

# variables to consider:
var.names <- c("rubber_area", 
               "oilpalm_area", 
               "household_nr", 
               "immigrant_nr", 
               "household_size", 
               "abandoned", 
               "lm_seller_wealth_k",
               "lm_buyer_wealth_k",
               "lm_ratio",
               "capitalstock", 
               "consumption_k", 
               "carbon_k", 
               "biodiversity", 
               "area_mn", 
               "ed")

# process data
res_agg <- results %>% 
  dplyr::filter(`[step]` >= 30) %>% 
  group_by(siminputrow, `LUT-0-price`, `LUT-1-price`, price_shock_scenario) %>% 
  dplyr::summarise(rubber_area = mean(lut1.fraction),
                   oilpalm_area = mean(lut0.fraction),
                   household_nr = mean(hh.count),
                   immigrant_nr = mean(hh.count.immigrant),
                   household_size = mean(hh.area.mean),
                   abandoned = mean(abandoned.land),
                   capitalstock = mean(p.capitalstock.mean),
                   consumption_k = mean(hh.consumption.mean) / 1000,
                   carbon_k = mean(lut0.carbon + lut1.carbon) / 1000,
                   biodiversity = mean(p.sar),
                   area_mn = mean(area_mn, na.rm = TRUE),
                   ed = mean(ed, na.rm = TRUE),
                   lpi = mean(lpi, na.rm = TRUE),
                   lsi = mean(lsi, na.rm = TRUE),
                   shdi = mean(shdi, na.rm = TRUE),
                   lm_seller_wealth_k = mean(lm.seller.wealth, na.rm = TRUE) / 1000,
                   lm_buyer_wealth_k = mean(lm.buyer.wealth, na.rm = TRUE) / 1000,
                   lm_ratio = (lm_seller_wealth_k / lm_buyer_wealth_k) * 100) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(`LUT-0-price`, `LUT-1-price`, price_shock_scenario, var.names) %>% 
  dplyr::mutate(scenario_crop = case_when(price_shock_scenario == "lut0_boom" ~ "oilpalm",
                                          price_shock_scenario == "lut0_shock" ~ "oilpalm",
                                          price_shock_scenario == "lut1_boom" ~ "rubber",
                                          price_shock_scenario == "lut1_shock" ~ "rubber",
                                          price_shock_scenario == "default" ~ "default")) %>% 
  dplyr::mutate(scenario_price = case_when(price_shock_scenario == "lut0_boom" ~ "boom",
                                           price_shock_scenario == "lut0_shock" ~ "shock",
                                           price_shock_scenario == "lut1_boom" ~ "boom",
                                           price_shock_scenario == "lut1_shock" ~ "shock",
                                           price_shock_scenario == "default" ~ "default")) %>% 
  dplyr::mutate(landscape_type = case_when(rubber_area >= 0.75 ~ "rubber_dominated",
                                           oilpalm_area >= 0.75 ~ "oilpalm_dominated")) %>% 
  dplyr::mutate(landscape_type = case_when(landscape_type == "rubber_dominated" ~ "rubber_dominated",
                                           landscape_type == "oilpalm_dominated" ~ "oilpalm_dominated",
                                           is.na(landscape_type) ~ "mix"))




#####################
## QUESTION 1:
#
# Only default price scenario
# 
# How do prices lead to dominance of specific crops:

res_agg_long <- res_agg %>% 
  dplyr::filter(price_shock_scenario %in% c("default")) %>% 
  dplyr::select(`LUT-0-price`, `LUT-1-price`, landscape_type, oilpalm_area, price_shock_scenario, consumption_k, carbon_k, biodiversity) %>% 
  tidyr::pivot_longer(c("consumption_k", "carbon_k", "biodiversity"))

ggplot(res_agg_long, aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=oilpalm_area)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(x="Palm oil price [$ per ton FFB]", y="Rubber price [$ per ton]", title="Oil palm area (fill) increases (brighter) with higher prices") +
  guides(fill="none") +
  theme_tufte(base_size = 12)

util.ggsave(filename="p1_prices_crops", path="03_Analyses/07_landmarket_v3", units = "cm", width=12, height=12, dpi=300)

#####################
## QUESTION 1:
#
# Only default price scenario
# Grouped by dominance of crops
##
## When we vary the price over a large spectrum of crop prices, 
## we see that landscapes converge to a dominance of one crop or to a mixture of both crops.
## How does this dominance relate to our outputs (EF)

purrr::map(unique(res_agg_long$name), function(x){
  res_agg_long %>% 
    dplyr::filter(name==x) %>% 
    ggplot(., aes(x=oilpalm_area, y=value)) +
    geom_point() +
    geom_smooth() +
    #guides(fill="none") +
    ggtitle(paste0(x)) +
    #scale_color_viridis_c() +
    #scale_fill_viridis_c() +
    #scale_linetype_manual(values=ltys) +
    #scale_fill_manual(values=cols) +
    #theme_ridges()
  theme_tufte(base_size=12)
}) %>% 
  cowplot::plot_grid(plotlist=., ncol=3)

util.ggsave(filename="p1_oilpalm_area_functions", path="03_Analyses/07_landmarket_v3", units = "cm", width=18, height=12, dpi=300)


## version two, stnadardize 0...1:

res_agg %>% 
  dplyr::filter(price_shock_scenario %in% c("default")) %>% 
  dplyr::select(`LUT-0-price`, `LUT-1-price`, landscape_type, oilpalm_area, price_shock_scenario, consumption_k, carbon_k, biodiversity) %>% 
  tidyr::pivot_longer(c("consumption_k", "carbon_k", "biodiversity")) %>% 
  group_by(name) %>% 
  dplyr::mutate(value = (((value - min(value)) * (1 - 0)) / (max(value) - min(value))) + 0) %>% 
  ggplot(., aes(x=oilpalm_area, y=value, color=name)) +
  geom_point(size=1, alpha=0.2) +
  geom_smooth(se=FALSE) +
  ggsci::scale_color_d3() +
  #theme_ridges()
  theme_tufte(base_size=12)


util.ggsave(filename="p1_oilpalm_area_functions_standardized", path="03_Analyses/07_landmarket_v3", units = "cm", width=12, height=12, dpi=300)



###################
## Household consolidation:

start.sizes <- results %>% 
  dplyr::filter(`[step]` == min(`[step]`)) %>% 
  dplyr::select(siminputrow,`LUT-0-price`, `LUT-1-price`, price_shock_scenario, hh.area.mean) %>% 
  dplyr::rename(hh.area.t0 = hh.area.mean)

final.sizes <- results %>% 
  dplyr::filter(`[step]` == max(`[step]`)) %>% 
  dplyr::select(siminputrow,`LUT-0-price`, `LUT-1-price`, price_shock_scenario, hh.area.mean) %>% 
  dplyr::rename(hh.area.t1 = hh.area.mean)



consolidation <- start.sizes %>% 
  dplyr::left_join(final.sizes) %>% 
  dplyr::mutate(hh.area.change = ((hh.area.t1 - hh.area.t0)/hh.area.t0) * 100)

cols <- c("lut0_boom"="#CF002C", 
          "lut0_shock"="#F99397",
          "lut1_boom"="#317AB2", 
          "lut1_shock"="#B1C8E7", 
          "default"="#424451")

ggplot(consolidation, aes(x=price_shock_scenario, y=hh.area.change, fill=price_shock_scenario, color=price_shock_scenario)) +
  geom_jitter(alpha=0.1) +
  geom_violin(size=1, scale="count") +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme_tufte(base_size = 12)


raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

ggplot(consolidation, aes(x = price_shock_scenario, y = hh.area.change, fill = price_shock_scenario)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = hh.area.change, color = price_shock_scenario), position = position_jitter(width = .15), size = .5, alpha = 0.2) +
  geom_boxplot(width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.9) +
  expand_limits(x = 5.25) +
  guides(fill="none", color="none") +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_y_log10() +
  #scale_color_brewer(palette = "Spectral") +
  #scale_fill_brewer(palette = "Spectral") +
  #coord_flip() +
  theme_bw() +
  raincloud_theme



#### Investigate the min prices and max prices scenarios:

min.prices <- consolidation %>% 
  dplyr::filter(`LUT-0-price` == min(`LUT-0-price`) & `LUT-1-price` == min(`LUT-1-price`)) %>%
  dplyr::mutate(scenario = "min.prices")

max.prices <- consolidation %>% 
  dplyr::filter(`LUT-0-price` == max(`LUT-0-price`) & `LUT-1-price` == max(`LUT-1-price`)) %>%
  dplyr::mutate(scenario = "max.prices")
  

min.prices %>% 
  dplyr::bind_rows(max.prices) %>% 
  ggplot(., aes(x=price_shock_scenario, y=hh.area.change, color=scenario)) +
  geom_point() +
  theme_tufte()
  



ggplot(consolidation, aes(x=`LUT-0-price`, y=hh.area.change, color=`LUT-1-price`)) +
  geom_point()




ggplot(results, aes(x=`[step]`, y=hh.area.mean, color=siminputrow)) +
  facet_wrap(~price_shock_scenario) +
  geom_point()





###############################################################

results %>% 
  dplyr::filter(`[step]` == 49) %>% 
  dplyr::filter(price_shock_scenario == "default") %>% 
  ggplot(., aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=hh.area.mean)) +
  geom_tile() +
  scale_fill_viridis_c(option="D")





results %>% 
 # dplyr::filter(`[step]` == 49) %>% 
#  dplyr::filter(price_shock_scenario == "default") %>% 
  ggplot(., aes(x=`[step]`, y=paste0(`LUT-0-price`,`LUT-1-price`), fill=hh.area.mean)) +
  facet_wrap(~price_shock_scenario) +
  geom_tile() +
  scale_fill_viridis_c(option="A") +
  theme_tufte()














res_agg %>% 
  dplyr::filter(price_shock_scenario %in% c("lut0_boom", "lut0_shock","lut1_boom", "lut1_shock", "default")) %>% 
  dplyr::select(`LUT-0-price`, `LUT-1-price`, landscape_type, oilpalm_area, price_shock_scenario, household_size) %>% 
  ggplot(., aes(x=`LUT-0-price`/`LUT-1-price`, y=household_size, color=price_shock_scenario)) +
  geom_point() +
  scale_fill_viridis_c() +
  #theme_ridges()
  theme_tufte(base_size=12)

res_agg %>% 
  dplyr::filter(price_shock_scenario %in% c("lut0_boom", "lut0_shock", "lut1_boom", "lut1_shock", "default")) %>% 
  dplyr::select(`LUT-0-price`, `LUT-1-price`, landscape_type, oilpalm_area, price_shock_scenario, household_size) %>% 
  ggplot(., aes(x=household_size, y=price_shock_scenario)) +
  geom_density_ridges(scale=1) +
  scale_x_log10() +
  theme_ridges()
  theme_tufte(base_size=12)




purrr::map(unique(res_agg_long$name), function(x){
  res_agg_long %>% 
    dplyr::filter(name==x) %>% 
    ggplot(., aes(x=value, y=landscape_type)) +
    #facet_wrap(~, ncol=1) +
    #geom_density(alpha=0.5, size=1, color=NA) +
    geom_density_ridges(scale=1
                        ) +
    guides(fill="none") +
    ggtitle(paste0(x)) +
    #scale_linetype_manual(values=ltys) +
    scale_fill_manual(values=cols) +
    theme_ridges()
  #theme_tufte(base_size=12)
}) %>% 
  cowplot::plot_grid(plotlist=., ncol=3)




purrr::map(unique(res_agg_long$name), function(x){
  res_agg_long %>% 
    dplyr::filter(name==x) %>% 
    ggplot(., aes(x=value, y=price_shock_scenario)) +
    #facet_wrap(~, ncol=1) +
    #geom_density(alpha=0.5, size=1, color=NA) +
    geom_density_ridges(scale=1) +
    guides(fill="none") +
    ggtitle(paste0(x)) +
    #scale_linetype_manual(values=ltys) +
    scale_fill_manual(values=cols) +
    theme_ridges()
  #theme_tufte(base_size=12)
}) %>% 
  cowplot::plot_grid(plotlist=., ncol=3)







cols <- c("oilpalm" = "#B24746",
          "rubber" = "#0072B5",
          "default" = "black")

ltys <- c("boom" = 1,
          "shock" = 3,
          "default" = 2)



























##### ONLY DISPLAY RESULTS FOR OILPALM BOOM
##

res_agg_long <- res_agg %>% 
  dplyr::filter(price_shock_scenario %in% c("lut0_boom", "default")) %>% 
  dplyr::select(landscape_type, price_shock_scenario, consumption_k, carbon_k, biodiversity) %>% 
  tidyr::pivot_longer(c("consumption_k", "carbon_k", "biodiversity"))

cols <- c("oilpalm_dominated" = "#B24746",
          "rubber_dominated" = "#0072B5",
          "mix" = "black")

purrr::map(unique(res_agg_long$name), function(x){
  res_agg_long %>% dplyr::filter(name==x) %>% 
    ggplot(., aes(x=value, fill=landscape_type)) +
    facet_wrap(~price_shock_scenario, ncol=1) +
    geom_density(alpha=0.5, size=1, color=NA) +
    #geom_density_ridges(aes(y=landscape_type)) +
    guides(fill="none") +
    ggtitle(paste0(x)) +
    #scale_linetype_manual(values=ltys) +
    scale_fill_manual(values=cols) +
    theme_ridges()
  #theme_tufte(base_size=12)
}) %>% 
  cowplot::plot_grid(plotlist=., ncol=3)





####################################################################
####################################################################
## How can we do this without classifying the landscapes first?
##
## rubber_area as
res_agg_long <- res_agg %>% 
  dplyr::filter(price_shock_scenario %in% c("lut0_boom", "lut0_shock", "default")) %>% 
  dplyr::select(oilpalm_area, price_shock_scenario, consumption_k, carbon_k, biodiversity) %>% 
  tidyr::pivot_longer(c("consumption_k", "carbon_k", "biodiversity"))

cols <- c("default" = "#B24746",
          "lut0_boom" = "#0072B5",
          "lut0_shock" = "black")


ggplot(res_agg_long, aes(x=oilpalm_area, y=value, fill=price_shock_scenario, color=price_shock_scenario)) +
  facet_wrap(~name, scales="free") +
  geom_point(alpha=0.2, size=0.7) +
  geom_smooth(alpha=0.3) +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  guides(fill="none", color="none") +
  theme_tufte(base_size = 12)

ggsave(paste0("03_Analyses/07_landmarket_v3/smoothplot.png"), units = "cm", width=16, height=8, dpi=300)






###################

purrr::map(unique(res_agg_long$name), function(x){
  res_agg_long %>% 
    dplyr::filter(name==x) %>% 
    ggplot(., aes(x=value, y=price_shock_scenario)) +
    #facet_wrap(~, ncol=1) +
    #geom_density(alpha=0.5, size=1, color=NA) +
    geom_density_ridges() +
    guides(fill="none") +
    ggtitle(paste0(x)) +
    #scale_linetype_manual(values=ltys) +
    scale_fill_manual(values=cols) +
    theme_ridges()
  #theme_tufte(base_size=12)
}) %>% 
  cowplot::plot_grid(plotlist=., ncol=3)
