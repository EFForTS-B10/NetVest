
########################################################################
## Load packages:
library(ggthemes)
library(nlrx)
library(tidyverse)
########################################################################
## Load simulation data
nl <- readRDS(file = file.path("03_Analyses/06_landmarket_v2/constantprices_ff_sensitivity.rds"))
results <- nl@simdesign@simoutput



########################################################################
## ANALYSIS PART 1.1: Default scenario, raw variables
########################################################################

# number of classes for raw value tileplots:
classes <- 8
# variables to consider in tileplot:
var.names <- c("rubber_area", 
               "oilpalm_area", 
               "household_nr", 
               "immigrant_nr", 
               "household_size", 
               "abandoned", 
               "capitalstock", 
               "consumption_k", 
               "carbon_k", 
               "biodiversity", 
               "area_mn", 
               "ed",
               "lm_new",
               "lm_seller_wealth_k",
               "lm_buyer_wealth_k")

# processing the data
sim_tile_default <- results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
  dplyr::filter(price_shock_scenario == "default") %>% 
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
                   lm_new = mean(lm_new, na.rm = TRUE),
                   lm_seller_wealth_k = mean(lm.seller.wealth, na.rm = TRUE) / 1000,
                   lm_buyer_wealth_k = mean(lm.buyer.wealth, na.rm = TRUE) / 1000) %>% 
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, price_shock_scenario, var.names) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(var.names, ~cut(., classes)) %>% 
  tidyr::pivot_longer(cols=var.names)

# Creating the plots
plots_tile_default <- purrr::map(var.names, function(x) {
  results_grouped.x <- sim_tile_default %>% dplyr::filter(name == x)
  
  p.x <- ggplot(results_grouped.x, aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=factor(value))) +
    geom_tile() +
    xlab("palm oil price [$/ton]") +
    ylab("rubber price [$/ton]") +
    scale_fill_viridis_d() +
    guides(fill=guide_legend(title=x)) +
    theme_tufte(base_size = 11)
  
  return(p.x)
})

# Painting and saving plots:
cowplot::plot_grid(plotlist=plots_tile_default, ncol = 3)
ggsave("03_Analyses/06_landmarket_v2/tileplot_rawvars_default.png", units = "cm", width=30, height=30, dpi=300)


########################################################################
## ANALYSIS PART 1.2: Default scenario, trade-offs and synergies
########################################################################

# number of classes for raw value tileplots:
classes <- 4
# variables to consider in tradeoff-plot
var.names <- c("consumption_k", "carbon_k", "biodiversity")
# process data:
sim_synergy_default <- results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
  dplyr::filter(price_shock_scenario == "default") %>% 
  group_by(siminputrow, `LUT-0-price`, `LUT-1-price`, price_shock_scenario) %>% 
  dplyr::summarise(consumption_k = mean(hh.consumption.mean) / 1000,
                   carbon_k = mean(lut0.carbon + lut1.carbon) / 1000,
                   biodiversity = mean(p.sar)) %>% 
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, price_shock_scenario, var.names) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(var.names, ~cut(., classes, labels=FALSE)) %>% 
  dplyr::mutate(consumption_carbon = round(((consumption_k + carbon_k) / 2), digits=2)) %>%  
  dplyr::mutate(consumption_biodiversity = round(((consumption_k + biodiversity) / 2), digits=2)) %>%
  dplyr::mutate(consumption_carbon_biodiversity = round(((consumption_k + biodiversity + carbon_k) / 3), digits=2)) %>%
  dplyr::select(`LUT-0-price`, `LUT-1-price`, consumption_carbon, consumption_biodiversity, consumption_carbon_biodiversity) %>% 
  tidyr::pivot_longer(cols=c("consumption_carbon", "consumption_biodiversity", "consumption_carbon_biodiversity"))

# paint and save plot
ggplot(sim_synergy_default, aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=factor(value))) +
  facet_wrap(~name) +
  geom_tile() +
  xlab("palm oil price [$/ton]") +
  ylab("rubber price [$/ton]") +
  scale_fill_viridis_d() +
  guides(fill=guide_legend(title="mean score")) +
  theme_tufte(base_size = 11)
ggsave("03_Analyses/06_landmarket_v2/tileplot_synergy_default.png", units = "cm", width=28, height=10, dpi=300)



########################################################################
## ANALYSIS PART 2: Sensitivity of price change scenarios
########################################################################

# number of classes for raw value tileplots:
classes <- 8
# variables to consider:
var.names <- c("rubber_area", 
               "oilpalm_area", 
               "household_nr", 
               "immigrant_nr", 
               "household_size", 
               "abandoned", 
               "capitalstock", 
               "consumption_k", 
               "carbon_k", 
               "biodiversity", 
               "area_mn", 
               "ed",
               "lm_new",
               "lm_seller_wealth_k",
               "lm_buyer_wealth_k")

# process data
sim_tile_sens <- results %>% 
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
                   lm_new = mean(lm_new, na.rm = TRUE),
                   lm_seller_wealth_k = mean(lm.seller.wealth, na.rm = TRUE) / 1000,
                   lm_buyer_wealth_k = mean(lm.buyer.wealth, na.rm = TRUE) / 1000) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(`LUT-0-price`, `LUT-1-price`, price_shock_scenario, var.names)

## split up into default and non-default dataset:
sim_tile_sens_def <- sim_tile_sens %>% dplyr::filter(price_shock_scenario == "default")
sim_tile_sens_sens <- sim_tile_sens %>% dplyr::filter(price_shock_scenario != "default")
## put back together:
sim_tile_sens <- sim_tile_sens_sens %>% dplyr::full_join(sim_tile_sens_def, by=c("LUT-0-price", "LUT-1-price"))

## Calculate sensitivity:
metrics_sens <- purrr::map_dfc(var.names, function(x){
  
  var.name.x <- paste0(x, ".x")
  var.name.x.sym <- rlang::sym(paste0(x, ".x"))
  var.name.y.sym <- rlang::sym(paste0(x, ".y"))
  
  sim_tile_sens_xy <- sim_tile_sens %>% 
    dplyr::mutate(!!x := (((!!var.name.x.sym) - (!!var.name.y.sym)) / (!!var.name.y.sym)) * 100) %>% 
    dplyr::select(x)
  
  return(sim_tile_sens_xy)
})

sim_tile_sens_plot <- sim_tile_sens %>% 
  dplyr::select(`LUT-0-price`, `LUT-1-price`, price_shock_scenario.x) %>% 
  dplyr::bind_cols(metrics_sens) %>% 
  tidyr::pivot_longer(cols=var.names) %>% 
  dplyr::mutate(scenario_crop = case_when(price_shock_scenario.x == "lut0_boom" ~ "oilpalm",
                                          price_shock_scenario.x == "lut0_shock" ~ "oilpalm",
                                          price_shock_scenario.x == "lut1_boom" ~ "rubber",
                                          price_shock_scenario.x == "lut1_shock" ~ "rubber")) %>% 
  dplyr::mutate(scenario_price = case_when(price_shock_scenario.x == "lut0_boom" ~ "boom",
                                           price_shock_scenario.x == "lut0_shock" ~ "shock",
                                           price_shock_scenario.x == "lut1_boom" ~ "boom",
                                           price_shock_scenario.x == "lut1_shock" ~ "shock"))


## Plot 
plots_tile_sens <- purrr::map(var.names, function(x) {
  results_grouped.x <- sim_tile_sens_plot %>% dplyr::filter(name == x)
  
  p.x <- ggplot(results_grouped.x, aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=value)) +
    facet_grid(scenario_crop~scenario_price) +
    geom_tile() +
    xlab("palm oil price [$/ton]") +
    ylab("rubber price [$/ton]") +
    #scale_fill_viridis_c() +
    scale_fill_gradient2(midpoint=0, low="blue", mid="white", high="red") +
    guides(fill=guide_colorbar(title=x)) +
    theme_tufte(base_size = 11)
  
  ggsave(paste0("03_Analyses/06_landmarket_v2/tileplot_sens_", x, ".png"), plot=p.x, units = "cm", width=10, height=8, dpi=300)
  
  return(p.x)
})

### Find best synergy:
classes <- 4
var.names <- c("consumption_k", "carbon_k", "biodiversity")
sim_synergy_scenario <- results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
  dplyr::filter(price_shock_scenario != "default") %>% 
  group_by(siminputrow, `LUT-0-price`, `LUT-1-price`, price_shock_scenario) %>% 
  dplyr::summarise(consumption_k = mean(hh.consumption.mean) / 1000,
                   carbon_k = mean(lut0.carbon + lut1.carbon) / 1000,
                   biodiversity = mean(p.sar)) %>% 
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, price_shock_scenario, var.names) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(price_shock_scenario) %>% 
  dplyr::mutate_at(var.names, ~cut(., classes, labels=FALSE)) %>% 
  dplyr::mutate(consumption_carbon = round(((consumption_k + carbon_k) / 2), digits=2)) %>%  
  dplyr::mutate(consumption_biodiversity = round(((consumption_k + biodiversity) / 2), digits=2)) %>%
  dplyr::mutate(consumption_carbon_biodiversity = round(((consumption_k + biodiversity + carbon_k) / 3), digits=2)) %>%
  dplyr::select(price_shock_scenario, `LUT-0-price`, `LUT-1-price`, consumption_carbon, consumption_biodiversity, consumption_carbon_biodiversity) %>% 
  tidyr::pivot_longer(cols=c("consumption_carbon", "consumption_biodiversity", "consumption_carbon_biodiversity"))



sim_synergy_sens <- sim_synergy_scenario %>% dplyr::full_join(sim_synergy_default, by=c("LUT-0-price", "LUT-1-price", "name")) %>% 
  dplyr::mutate(index_sensitivity = ((value.x - value.y) / value.y) * 100) %>% 
  dplyr::mutate(scenario_crop = case_when(price_shock_scenario == "lut0_boom" ~ "oilpalm",
                                          price_shock_scenario == "lut0_shock" ~ "oilpalm",
                                          price_shock_scenario == "lut1_boom" ~ "rubber",
                                          price_shock_scenario == "lut1_shock" ~ "rubber")) %>% 
  dplyr::mutate(scenario_price = case_when(price_shock_scenario == "lut0_boom" ~ "boom",
                                           price_shock_scenario == "lut0_shock" ~ "shock",
                                           price_shock_scenario == "lut1_boom" ~ "boom",
                                           price_shock_scenario == "lut1_shock" ~ "shock"))




plots_tile_sens <- purrr::map(unique(sim_synergy_sens$name), function(x) {
  results_grouped.x <- sim_synergy_sens %>% dplyr::filter(name == x)
  
  p.x <- ggplot(results_grouped.x, aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=index_sensitivity)) +
    facet_grid(scenario_crop~scenario_price) +
    geom_tile() +
    xlab("palm oil price [$/ton]") +
    ylab("rubber price [$/ton]") +
    scale_fill_gradient2(midpoint=0, low="blue", mid="white", high="red") +
    guides(fill=guide_colorbar(title="mean score")) +
    theme_tufte(base_size = 11)
  
  ggsave(paste0("03_Analyses/06_landmarket_v2/tileplot_snyergy_sens_", x, ".png"), plot=p.x, units = "cm", width=10, height=8, dpi=300)
  
  return(p.x)
})

###







































### Correlations:
var.names <- c("rubber_area", "oilpalm_area", "household_nr", "household_size", "abandoned", "capitalstock", "consumption_k", "carbon_k", "biodiversity", "area_mn", "ed")

calccorr <- function(res) {
  res <- Hmisc::rcorr(as.matrix(res), type="pearson")
  pmat <- res$P
  cormat <- res$r
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
  group_by(siminputrow, `LUT-0-price`, `LUT-1-price`) %>% 
  dplyr::summarise(rubber_area = mean(lut1.fraction),
                   oilpalm_area = mean(lut0.fraction),
                   household_nr = mean(hh.count),
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
                   shdi = mean(shdi, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(var.names) %>% 
  calccorr(.) %>% 
  mutate(row=factor(row, levels=unique(row))) %>% 
  mutate(column=factor(column, levels=unique(column))) %>% 
  mutate(signif = dplyr::case_when(p <= 0.0005 ~ "***",
                                   p <= 0.005 ~ "**",
                                   p <= 0.05 ~ "*",
                                   p > 0.05 ~ "n.s.",
                                   is.na(p) ~ "n.s.")) %>% 
  ggplot(., aes(x=row, y=column, fill=cor)) +
  geom_tile() +
  geom_text(aes(label = signif), size=4, color="grey40") +
  #ggthemes::scale_fill_gradient2_tableau(palette="Classic Orange-White-Blue") +
  paletteer::scale_fill_paletteer_c("pals::ocean.curl", direction = -1) +
  xlab("") +
  ylab("") +
  theme_tufte(base_size = 11) +
  theme(axis.text.x = element_text(angle=45,hjust=0.95,vjust=0.9))

ggsave("03_Analyses/05_Refforts_constantPrices/prices_ff_correlations.png", units = "cm", width=14, height=14, dpi=300)
