

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

colors <- c("white", #green
            "black", #pink
            "grey")

max.arrow.size <- 6

# number of classes for binning tileplots:
classes <- 4


########################################################################
## PLOT 1: Output variables
########################################################################

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
  dplyr::filter(`[step]` >= 25) %>% 
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
                                          price_shock_scenario == "lut1_shock" ~ "rubber")) %>% 
  dplyr::mutate(scenario_price = case_when(price_shock_scenario == "lut0_boom" ~ "boom",
                                           price_shock_scenario == "lut0_shock" ~ "shock",
                                           price_shock_scenario == "lut1_boom" ~ "boom",
                                           price_shock_scenario == "lut1_shock" ~ "shock"))





tt <- res_agg %>% 
  dplyr::filter(price_shock_scenario != "default")
  #dplyr::filter(price_shock_scenario %in% c("default", "lut0_boom"))


ggplot(tt, aes(x=`LUT-1-price`, y=rubber_area, group = `LUT-0-price`, color=`LUT-0-price`)) +
  facet_grid(scenario_crop~scenario_price) +
  geom_line() +
  geom_hline(yintercept = 0.5, lty=3) +
  scale_color_viridis_c() +
  theme_tufte(base_size = 12)

ggplot(tt, aes(x=`LUT-1-price`, y=biodiversity, group = `LUT-0-price`, color=`LUT-0-price`)) +
  facet_grid(scenario_crop~scenario_price) +
  geom_line() +
  scale_color_viridis_c() +
  theme_tufte(base_size = 12)
