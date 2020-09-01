


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
########################################################################
## Load simulation data
nl <- readRDS(file = file.path(getwd(), "03_Analyses/07_landmarket_v3/constantprices_ff_sensitivity.rds"))
results <- nl@simdesign@simoutput

# variables to consider:
classes <- 4
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
                                           is.na(landscape_type) ~ "mix")) #%>% 
  #dplyr::mutate_at(var.names, ~cut(., classes))
 # dplyr::mutate_at(var.names, ~ntile(., classes))


cols <- c("oilpalm" = "#B24746",
          "rubber" = "#0072B5",
          "default" = "black")

ltys <- c("boom" = 1,
         "shock" = 3,
         "default" = 2)

ggplot(res_agg, aes(x=consumption_k, color=scenario_crop, lty=scenario_price)) +
  facet_wrap(~landscape_type) +
  geom_density(alpha=0.5, size=1) +
  scale_linetype_manual(values=ltys) +
  scale_color_manual(values=cols) +
  theme_tufte(base_size=12)


ggplot(res_agg, aes(x=biodiversity, color=scenario_crop, lty=scenario_price)) +
  facet_wrap(~landscape_type) +
  geom_density(alpha=0.5, size=1) +
  scale_linetype_manual(values=ltys) +
  scale_color_manual(values=cols) +
  theme_tufte(base_size=12)

ggplot(res_agg, aes(x=carbon_k, color=scenario_crop, lty=scenario_price)) +
  facet_wrap(~landscape_type) +
  geom_density(alpha=0.5, size=1) +
  scale_linetype_manual(values=ltys) +
  scale_color_manual(values=cols) +
  theme_tufte(base_size=12)



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
  


library(ggridges)


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
