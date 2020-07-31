
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
  dplyr::select(`LUT-0-price`, `LUT-1-price`, price_shock_scenario, var.names)

## split up into default and non-default dataset:
res_agg_null <- res_agg %>% dplyr::filter(price_shock_scenario == "default")
res_agg_scenario <- res_agg %>% dplyr::filter(price_shock_scenario != "default")
## put back together:
res_agg_combined <- res_agg_scenario %>% dplyr::full_join(res_agg_null, by=c("LUT-0-price", "LUT-1-price"))

## Calculate sensitivity:
res_sensitivity <- purrr::map_dfc(var.names, function(x){
  
  var.name.x <- paste0(x, ".x")
  var.name.x.sym <- rlang::sym(paste0(x, ".x"))
  var.name.y.sym <- rlang::sym(paste0(x, ".y"))
  
  res_agg_combined_xy <- res_agg_combined %>% 
    dplyr::mutate(!!x := (((!!var.name.x.sym) - (!!var.name.y.sym)) / (!!var.name.y.sym)) * 100) %>% 
    dplyr::select(x)
  
  return(res_agg_combined_xy)
})

## Attach to scenario data:
res_sensitivity_long <- res_agg_combined %>% 
  dplyr::select(`LUT-0-price`, `LUT-1-price`, price_shock_scenario.x) %>% 
  dplyr::bind_cols(res_sensitivity) %>% 
  tidyr::pivot_longer(cols=var.names) %>% 
  dplyr::mutate(scenario_crop = case_when(price_shock_scenario.x == "lut0_boom" ~ "oilpalm",
                                          price_shock_scenario.x == "lut0_shock" ~ "oilpalm",
                                          price_shock_scenario.x == "lut1_boom" ~ "rubber",
                                          price_shock_scenario.x == "lut1_shock" ~ "rubber")) %>% 
  dplyr::mutate(scenario_price = case_when(price_shock_scenario.x == "lut0_boom" ~ "boom",
                                           price_shock_scenario.x == "lut0_shock" ~ "shock",
                                           price_shock_scenario.x == "lut1_boom" ~ "boom",
                                           price_shock_scenario.x == "lut1_shock" ~ "shock"))

## Convert null data to long format for tile plotting
res_agg_null_long <- purrr::map_dfr(unique(paste0(res_sensitivity_long$scenario_crop, "_", res_sensitivity_long$scenario_price)), function(x) {
  
  res_agg_null.x <- res_agg_null %>% 
    dplyr::mutate_at(var.names, ~cut(., classes)) %>% 
    tidyr::pivot_longer(cols=var.names) %>% 
    dplyr::mutate(value = as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", value))) %>% 
    dplyr::mutate(scenario_crop = strsplit(x, split="_")[[1]][[1]]) %>% 
    dplyr::mutate(scenario_price = strsplit(x, split="_")[[1]][[2]])
  
})


##### CUSTOM LEGEND:
library(grid)
draw_key_text <- function(data, params, size) {
  if(is.null(data$label)) data$label <- "⭦"
  
  textGrob(data$label, 0.5, 0.5,
           rot = data$angle %||% 0,
           gp = gpar(
             col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
             fontfamily = data$family %||% "",
             fontface = data$fontface %||% 1,
             fontsize = (data$size %||% 3.88) * .pt
           )
  )
}


## Plot 
plots_tile_sens <- purrr::map(var.names, function(x) {
  
  res_sensitivity_long.x <- res_sensitivity_long %>% dplyr::filter(name == x) %>% 
    dplyr::mutate(shape_sensitivity = dplyr::case_when(value > 0 ~ "⭦",
                                                       value < 0 ~ "⭨",
                                                       value == 0 ~ "•"))
  
  res_agg_null_long.x <- res_agg_null_long %>% 
    dplyr::filter(name == x)

  
  p.x <- ggplot(res_sensitivity_long.x, aes(x=`LUT-0-price`, y=`LUT-1-price`)) +
    facet_grid(scenario_crop~scenario_price) +
    coord_equal(ratio=1/10) +
    geom_tile(data=res_agg_null_long.x, aes(fill=factor(value)), alpha=1) +
    geom_text(data=res_sensitivity_long.x, aes(size=abs(value), label=shape_sensitivity, color=shape_sensitivity), key_glyph=draw_key_text) +
    xlab("palm oil price [$/ton]") +
    ylab("rubber price [$/ton]") +
    scale_fill_viridis_d(option = "plasma", begin=0.3, end=0.9) +
    scale_color_manual(values=colors) +
    scale_size_continuous(range=c(0,max.arrow.size)) +
    scale_x_continuous(breaks=c(50, 100, 150)) +
    scale_y_continuous(breaks=c(500,1000,1500)) +
    guides(fill=guide_legend(title=x, order=1),
           color="none", size=guide_legend(title="sensitivity [%]", order=2)) +
    theme_tufte(base_size = 11) +
    theme(panel.spacing = unit(-0.5, "lines"))
  
  ggsave(paste0("03_Analyses/06_landmarket_v2/tileplot_sens_", x, ".png"), plot=p.x, units = "cm", width=16, height=12, dpi=300)
  
  return(p.x)
})

########################################################################
## PLOT 1: Multiple outputs
########################################################################

### Find best synergy:
var.names <- c("consumption_k", "carbon_k", "biodiversity")
res_syn <- results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
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
  dplyr::select(price_shock_scenario, `LUT-0-price`, `LUT-1-price`, consumption_carbon, consumption_biodiversity, consumption_carbon_biodiversity) 


## split up into default and non-default dataset:
res_syn_null <- res_syn %>% dplyr::filter(price_shock_scenario == "default")
res_syn_scenario <- res_syn %>% dplyr::filter(price_shock_scenario != "default")
## put back together:
res_syn_combined <- res_syn_scenario %>% dplyr::full_join(res_syn_null, by=c("LUT-0-price", "LUT-1-price"))

## Calculate sensitivity:
var.names <- c("consumption_carbon", "consumption_biodiversity", "consumption_carbon_biodiversity")
res_sensitivity <- purrr::map_dfc(var.names, function(x){
  
  var.name.x <- paste0(x, ".x")
  var.name.x.sym <- rlang::sym(paste0(x, ".x"))
  var.name.y.sym <- rlang::sym(paste0(x, ".y"))
  
  res_syn_combined_xy <- res_syn_combined %>% 
    dplyr::mutate(!!x := (((!!var.name.x.sym) - (!!var.name.y.sym)) / (!!var.name.y.sym)) * 100) %>% 
    dplyr::select(x)
  
  return(res_syn_combined_xy)
})

## Attach to scenario data:
res_sensitivity_long <- res_syn_combined %>% 
  dplyr::select(`LUT-0-price`, `LUT-1-price`, price_shock_scenario.x) %>% 
  dplyr::bind_cols(res_sensitivity) %>% 
  tidyr::pivot_longer(cols=var.names) %>% 
  dplyr::mutate(scenario_crop = case_when(price_shock_scenario.x == "lut0_boom" ~ "oilpalm",
                                          price_shock_scenario.x == "lut0_shock" ~ "oilpalm",
                                          price_shock_scenario.x == "lut1_boom" ~ "rubber",
                                          price_shock_scenario.x == "lut1_shock" ~ "rubber")) %>% 
  dplyr::mutate(scenario_price = case_when(price_shock_scenario.x == "lut0_boom" ~ "boom",
                                           price_shock_scenario.x == "lut0_shock" ~ "shock",
                                           price_shock_scenario.x == "lut1_boom" ~ "boom",
                                           price_shock_scenario.x == "lut1_shock" ~ "shock"))

## Convert scenario data to long format for tile plotting
res_syn_null_long <- purrr::map_dfr(unique(paste0(res_sensitivity_long$scenario_crop, "_", res_sensitivity_long$scenario_price)), function(x) {
  
  res_syn_null.x <- res_syn_null %>% 
    tidyr::pivot_longer(cols=var.names) %>% 
    dplyr::mutate(scenario_crop = strsplit(x, split="_")[[1]][[1]]) %>% 
    dplyr::mutate(scenario_price = strsplit(x, split="_")[[1]][[2]])
  
})


res_syn_scenario_long <- res_syn_scenario %>% 
  tidyr::pivot_longer(cols=var.names)


plots_tile_sens <- purrr::map(unique(res_sensitivity_long$name), function(x) {
  
  res_sensitivity_long.x <- res_sensitivity_long %>% dplyr::filter(name == x) %>% 
    dplyr::mutate(shape_sensitivity = dplyr::case_when(value > 0 ~ "⭦",
                                                       value < 0 ~ "⭨",
                                                       value == 0 ~ "•"))
  
  res_syn_scenario_long.x <- res_syn_scenario_long %>% dplyr::filter(name == x)

  legend_title <- paste(strsplit(x, split="_")[[1]], collapse = "\n")
  
  p.x <- ggplot(res_sensitivity_long.x, aes(x=`LUT-0-price`, y=`LUT-1-price`)) +
    facet_grid(scenario_crop~scenario_price) +
    coord_equal(ratio=1/10) +
    geom_tile(data=res_syn_scenario_long.x, aes(fill=value), alpha=1) +
    geom_text(data=res_sensitivity_long.x, aes(size=abs(value), label=shape_sensitivity, color=shape_sensitivity), key_glyph=draw_key_text) +
    xlab("palm oil price [$/ton]") +
    ylab("rubber price [$/ton]") +
    scale_fill_viridis_c(option = "plasma") +
    scale_color_manual(values=colors) +
    scale_size_continuous(range=c(0,max.arrow.size)) +
    scale_x_continuous(breaks=c(50, 100, 150)) +
    scale_y_continuous(breaks=c(500,1000,1500)) +
    guides(fill=guide_colorbar(title=legend_title, order=1),
           color="none", size=guide_legend(title="sensitivity [%]", order=2)) +
    theme_tufte(base_size = 11) +
    theme(panel.spacing = unit(-0.5, "lines"))
  
  ggsave(paste0("03_Analyses/06_landmarket_v2/tileplot_synergy_sens_", x, ".png"), plot=p.x, units = "cm", width=16, height=12, dpi=300)
  
  return(p.x)
})




############################################################################################################
############################################################################################################
############################################################################################################




