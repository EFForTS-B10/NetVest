
##
##
##  In this analysis we expeirment with different wages levels (see script)
##
##  However, this was performed with the old landmarket map
##  If this analysis yields interesting plots/results, we havve to repeat it with the current map and settings (see landmarket _v3/_v4)
##
##

library(tidyverse)
library(nlrx)
library(Refforts)

## Load preliminary results:
nl <- readRDS("03_Analyses/09_landmarket_v5_wages/sims_wages_distinct.rds")
#timeplot <- doplot.abm.timeseries(nl, metrics=nl@experiment@metrics)
#timeplot

## Manual plotting:
results <- nl@simdesign@simoutput

## Hypothesis 1: Yields imrpove with higher labor
#### Calculate mean of last 10 ticks for assessment:
# variables to consider:
var.names <- c("rubber_area", 
               "oilpalm_area", 
               "rubber_yield",
               "oilpalm_yield",
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
               "lpi",
               "lsi",
               "shdi",
               "lm_seller_wealth_k",
               "lm_buyer_wealth_k",
               "lm_oilpalm_ineff_diff",
               "lm_rubber_ineff_diff",
               "lm_area_diff",
               "lm_ratio")


# process data
res_agg <- results %>% 
  dplyr::filter(`[step]` >= 30) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(wages = as.numeric(stringr::str_split(`LUT-0-folder`, pattern="_")[[1]][[3]]))

res_agg <- res_agg %>% 
  dplyr::rename(rubber_price = `LUT-1-price`,
                oilpalm_price = `LUT-0-price`) %>% 
  group_by(siminputrow, rubber_price, oilpalm_price, wages) %>% 
  dplyr::summarise(rubber_area = mean(lut1.fraction),
                   oilpalm_area = mean(lut0.fraction),
                   rubber_yield = mean(lut1.yield.mean),
                   oilpalm_yield = mean(lut0.yield.mean),
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
                   lm_oilpalm_ineff_diff = mean(lm.buyer.lut0.ineff - lm.seller.lut0.ineff),
                   lm_rubber_ineff_diff = mean(lm.buyer.lut1.ineff - lm.seller.lut1.ineff),
                   lm_area_diff = mean(lm.buyer.area - lm.seller.area),
                   lm_ratio = (lm_seller_wealth_k / lm_buyer_wealth_k) * 100) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(rubber_price, oilpalm_price, wages, var.names) %>% 
  dplyr::mutate(landscape_type = case_when(rubber_area >= 0.75 ~ "rubber_dominated",
                                           oilpalm_area >= 0.75 ~ "oilpalm_dominated",
                                           rubber_area < 0.75 & oilpalm_area < 0.75 ~ "mix")) %>% 
  dplyr::mutate(rubber_price_rank = dplyr::percent_rank(rubber_price),
                oilpalm_price_rank = dplyr::percent_rank(oilpalm_price)) %>% 
  dplyr::mutate(price_rank_mu = (rubber_price_rank + oilpalm_price_rank) / 2)



# only use min and max rubber prices:
res_agg_yield <- res_agg %>% 
  dplyr::filter(rubber_price == max(rubber_price) | rubber_price == min(rubber_price)) %>% 
  dplyr::filter(oilpalm_price == max(oilpalm_price) | oilpalm_price == min(oilpalm_price)) %>% 
  dplyr::mutate(prices = case_when(rubber_price == min(rubber_price) & oilpalm_price == min(oilpalm_price) ~ "lowest",
                                   rubber_price == max(rubber_price) & oilpalm_price == max(oilpalm_price) ~ "highest",
                                   rubber_price == min(rubber_price) & oilpalm_price == max(oilpalm_price) ~ "oilpalm-boom",
                                   rubber_price == max(rubber_price) & oilpalm_price == min(oilpalm_price) ~ "rubber-boom"))

####################################################
## PLOTTING:
####################################################

####################################################
### PLOTTING FUNCTIONS:
####################################################

pf_landscape <- function(lut, homebase, roads)
{
  cols <- c("1" = "#AD4047", "2" = "#DA8B4A")
  p_landscape <- ggplot() +
    geom_raster(data=lut, aes(x=x, y=y, fill=factor(land.use.type.raster))) +
    geom_sf(data=roads, color="white", size=1) +
    geom_sf(data=homebase, fill="black", color="black", pch=17, size=1, alpha=0.7) +
    scale_fill_manual(values = cols, na.value="#7EB098") +
    guides(fill="none") +
    theme_void(base_size = 12)
  
  return(p_landscape)
}



pf_lines <- function(data, yvar) 
{
  price_cols <- c("highest" = "#32BD20", 
                  "lowest" = "#F8781D",
                  "oilpalm-boom" = "#EA32EE",
                  "rubber-boom" = "#2E9CCA")
  
  data <- data %>% 
    dplyr::rename(yvar = yvar)
  
  p <- ggplot(data, aes(x=wages, y=yvar, color=prices, group=prices)) +
    geom_point(size=2) +
    geom_line(size=1) +
    labs(y=yvar) +
    scale_color_manual(values = price_cols) +
    ggthemes::theme_tufte()
  
  return(p)
}

pf_hull <- function(data, xvar, yvar)
{
  price_cols <- c("highest" = "#32BD20", 
                  "lowest" = "#F8781D",
                  "oilpalm-boom" = "#EA32EE",
                  "rubber-boom" = "#2E9CCA")
  
  data <- data %>% 
    dplyr::rename(xvar = xvar, yvar = yvar)
  
  hull <- data %>%
    group_by(prices) %>% 
    slice(chull(xvar, yvar))
  
  p <- ggplot(data, aes(x=xvar, y=yvar, color=prices, fill=prices)) +
    geom_point(aes(pch=factor(wages))) +
    geom_polygon(data=hull, alpha=0.1) +
    labs(x=xvar, y=yvar) +
    scale_color_manual(values = price_cols) +
    scale_fill_manual(values = price_cols) +
    ggthemes::theme_tufte(base_size=12)
  
  return(p)
}


####################################################
#### plot0: initial landscape
library(raster)
lut <- raster::as.data.frame(raster("01_EFForTS-ABM/input/maps/landmarkets2/land-use-type-raster.asc"), xy=TRUE)
homebase <- sf::st_centroid(sf::st_as_sf(stars::read_stars("01_EFForTS-ABM/input/maps/landmarkets2/homebase-raster.asc", NA_value = -1)))
roads <- sf::st_simplify(sf::read_sf("01_EFForTS-ABM/input/maps/landmarkets2/roads_polyline_layer.shp"))

p_landscape <- pf_landscape(lut, homebase, roads)
util.ggsave(plot=p_landscape, filename="initial_landscape", path="03_Analyses/09_landmarket_v5_wages", units = "cm", width=10, height=10, dpi=300)


####################################################
### Plot 1: Oilpalm area:
p_op_area <- pf_lines(res_agg_yield, "oilpalm_area")
util.ggsave(plot=p_op_area, filename="lines_oilpalm_area", path="03_Analyses/09_landmarket_v5_wages", units = "cm", width=10, height=5, dpi=300)

## > Interpretation
## 
## We observe high oilpalm area for boom prices.
## With higher labor costs, we see a tendency towards more oilpalm cultivation, even when rubber prices are low.
## Only with rubber boom prices, we do not see any oilpalm cultivation
##
##

####################################################
### Plot 2: Yields (efficiency)
p_op_yield <- pf_lines(res_agg_yield, "oilpalm_yield")
p_rb_yield <- pf_lines(res_agg_yield, "rubber_yield")
pall <- cowplot::plot_grid(plotlist=list(p_op_yield,p_rb_yield), ncol=1)
util.ggsave(plot=pall, filename="lines_yields", path="03_Analyses/09_landmarket_v5_wages", units = "cm", width=10, height=10, dpi=300)

## > Interpretation
##
## For oilpalm, we observe higher yields with higher labor costs
## However, this is only true when either all prices are very low, or only oilpalm is high
## When both prices are high, or only rubber is high, there is no yield increase in palm oil
##

####################################################
### Plot 3: HOusehold size / consolidation
p_hh_nr <- pf_lines(res_agg_yield, "household_nr")
p_hh_size <- pf_lines(res_agg_yield, "household_size")
p_immi_nr <- pf_lines(res_agg_yield, "immigrant_nr")
p_abandoned <- pf_lines(res_agg_yield, "abandoned")
pall <- cowplot::plot_grid(plotlist=list(p_hh_nr,p_hh_size,p_immi_nr,p_abandoned), ncol=1)
util.ggsave(plot=pall, filename="lines_household_size", path="03_Analyses/09_landmarket_v5_wages", units = "cm", width=10, height=20, dpi=300)

##> Interpretation
##
## We observe the strongest household consolidation when prices are lowest, which makes sense.
## economic pressure is the strongest here, so "inefficient" household drop out quickly
## But it is interesting to see that the oilpalm boom also leads to a strong consolidation when labor costs are high
##

####################################################
### Plot 4: Landscape metrics
p_ed <- pf_lines(res_agg_yield, "ed")
util.ggsave(plot=p_ed, filename="lines_landscape", path="03_Analyses/09_landmarket_v5_wages", units = "cm", width=10, height=5, dpi=300)

## > Interpretation
##
## With increasing labor costs, we observe more oilpalm area, thus dedge density increases
## For high labor and oilpalm boom, the landscape already switches into oilpalm dominance, which reduces the edge density again
##
##

####################################################
### Plot 5: Ecosystem functions
p_ef_con <- pf_lines(res_agg_yield, "consumption_k")
p_ef_car <- pf_lines(res_agg_yield, "carbon_k")
p_ef_bio <- pf_lines(res_agg_yield, "biodiversity")
pall <- cowplot::plot_grid(plotlist=list(p_ef_con,p_ef_car,p_ef_bio), ncol=1)
util.ggsave(plot=pall, filename="lines_ef", path="03_Analyses/09_landmarket_v5_wages", units = "cm", width=10, height=15, dpi=300)


####################################################
### Plot: all lineplots together on one page:

pall <- cowplot::plot_grid(plotlist=list(p_op_area + guides(color="none"), 
                                         p_ed + guides(color="none"), 
                                         p_hh_nr + guides(color="none"),
                                         p_hh_size + guides(color="none"),
                                         p_immi_nr + guides(color="none"),
                                         p_abandoned + theme(legend.position = c(0.3,0.6),
                                                             legend.background = element_rect(fill=NA)),
                                         p_op_yield + guides(color="none"), 
                                         p_rb_yield + guides(color="none")), 
                           ncol=2,
                           labels = "AUTO") 

util.ggsave(plot=pall, filename="lines_panel", path="03_Analyses/09_landmarket_v5_wages", units = "cm", width=20, height=25, dpi=300)


####################################################
### Plot 6: Trade offs as hull plots in dependence of op area

############################
## Plotting functions:

p1 <- pf_hull(res_agg_yield, "biodiversity", "carbon_k") + 
  geom_vline(xintercept=mean(res_agg_yield$biodiversity), lty=2) +
  geom_hline(yintercept=mean(res_agg_yield$carbon_k), lty=2)

p2 <- pf_hull(res_agg_yield, "biodiversity", "consumption_k") + 
  geom_vline(xintercept=mean(res_agg_yield$biodiversity), lty=2) +
  geom_hline(yintercept=mean(res_agg_yield$consumption_k), lty=2)

p3 <- pf_hull(res_agg_yield, "carbon_k", "consumption_k") + 
  geom_vline(xintercept=mean(res_agg_yield$carbon_k), lty=2) +
  geom_hline(yintercept=mean(res_agg_yield$consumption_k), lty=2)

cowplot::plot_grid(plotlist=list(p1,p2,p3), ncol=1)
util.ggsave(filename="hull_ef", path="03_Analyses/09_landmarket_v5_wages", units = "cm", width=10, height=15, dpi=300)


### PLOT 7: Tradeoffs as ranks:
rank_cols <- c("biodiversity" = "#28995F",
               "carbon" = "#244764",
               "consumption" = "#FBCB58")

res_agg_yield %>% 
  dplyr::select(wages, prices, consumption_k, carbon_k, biodiversity) %>% 
  dplyr::mutate(consumption_rank = dplyr::percent_rank(consumption_k),
                carbon_rank = dplyr::percent_rank(carbon_k),
                biodiversity_rank = dplyr::percent_rank(biodiversity)) %>% 
  tidyr::pivot_longer(contains("rank")) %>% 
  tidyr::separate(name, "name") %>% 
  ggplot(., aes(x=factor(wages), y=value, fill=name)) +
  facet_wrap(~prices) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = rank_cols) +
  geom_hline(yintercept = 3, lty=2) +
  guides(fill=guide_legend(title="")) +
  xlab("wage cost factor") +
  ylab("percent rank") +
  ggthemes::theme_tufte(base_size=12) +
  theme(legend.position = "top",
        legend.direction = "horizontal")

util.ggsave(filename="bar_ef", path="03_Analyses/09_landmarket_v5_wages", units = "cm", width=12, height=10, dpi=300)





#############################################################################################
#############################################################################################
##### STANDARDIZED REGRESSION COEFFICIENTS:
#############################################################################################

library(ggpattern)

res_lm <- results %>% 
  dplyr::filter(`[step]` >= 30) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(wages = as.numeric(stringr::str_split(`LUT-0-folder`, pattern="_")[[1]][[3]])) %>% 
  dplyr::rename(rubber_price = `LUT-1-price`,
                oilpalm_price = `LUT-0-price`,
                rubber_area = lut1.fraction,
                oilpalm_area = lut0.fraction,
                rubber_yield = lut1.yield.mean,
                oilpalm_yield = lut0.yield.mean,
                household_nr = hh.count,
                immigrant_nr = hh.count.immigrant,
                household_size = hh.area.mean,
                abandoned = abandoned.land,
                capitalstock = p.capitalstock.mean,
                consumption = hh.consumption.mean,
                biodiversity =p.sar,
                lm_seller_wealth = lm.seller.wealth,
                lm_buyer_wealth = lm.buyer.wealth) %>% 
  dplyr::mutate(carbon_k = (lut0.carbon + lut1.carbon) / 1000)
                
lm.output.vars <- c("oilpalm_area", "ed", "household_nr", "household_size", "oilpalm_yield", "rubber_yield", "consumption", "carbon_k", "biodiversity")

res_lm_fit <- purrr::map_dfr(lm.output.vars, function(x){
  
  
  res_lmx <- res_lm %>% 
    dplyr::rename(value = !!x) %>% 
    dplyr::select(value, rubber_price, oilpalm_price, wages)
  
  res_lmx$value <- scale(res_lmx$value)
  res_lmx$rubber_price <- scale(res_lmx$rubber_price)
  res_lmx$oilpalm_price <- scale(res_lmx$oilpalm_price)
  res_lmx$wages <- scale(res_lmx$wages)
    
  res_lmx_fit <- summary(lm(value ~ rubber_price * oilpalm_price * wages, data=res_lmx))
  
  res_lmx_fit_coeff <- as_tibble(res_lmx_fit$coefficients) %>% 
    dplyr::mutate(parameter = names(res_lmx_fit$coefficients[,1]),
                  output = x) %>% 
    dplyr::mutate(sig = ifelse(`Pr(>|t|)` < 0.05, "*", ""))
  
  
  return(res_lmx_fit_coeff)
  
}) %>% 
  dplyr::filter(parameter != "(Intercept)") %>% 
  dplyr::filter(parameter != "rubber_price:oilpalm_price:wages") %>% 
  dplyr::mutate(type = ifelse(grepl(":", parameter), "interaction", "direct")) %>% 
  tidyr::separate(parameter, into=c("paramA", "paramB"), sep=":") %>% 
  dplyr::mutate(paramB = ifelse(is.na(paramB), paramA, paramB)) %>% 
  dplyr::mutate(category = case_when(output %in% lm.output.vars[1:2] ~ "landscape",
                                output %in% lm.output.vars[3:4] ~ "households",
                                output %in% lm.output.vars[5:6] ~ "yields",
                                output %in% lm.output.vars[7:9] ~ "ef"))
  
res_lm_fit$output <- factor(res_lm_fit$output, levels=lm.output.vars)
res_lm_fit$category <- factor(res_lm_fit$category, levels=c("landscape", "households", "yields", "ef"))
                                 

cols <- c("oilpalm_price" = "#C74B4E",
          "rubber_price" = "#EBBD28",
          "wages" = "#2476C0")


ggplot(res_lm_fit) +
  coord_flip() +
  facet_grid(category~type, scales="free", switch = "y") +
  geom_col_pattern(aes(x=output, y=Estimate, fill=paramA, pattern_fill=paramB, pattern_color=paramA), color="black", pattern_density=0.5, pattern_key_scale_factor = 0.5, position="dodge") +
  guides(pattern_fill=guide_legend(title="Parameter"), fill=guide_legend(title="Parameter"), pattern_color="none") +
  geom_hline(yintercept = 0, lty=2) +
  scale_fill_manual(values=cols) +
  scale_pattern_fill_manual(values=cols) +
  scale_pattern_color_manual(values=cols) +
  ggthemes::theme_tufte(base_size=12) +
  theme(strip.placement = "outside",
        panel.border = element_rect(fill=NA))

util.ggsave(filename="src_striped", path="03_Analyses/09_landmarket_v5_wages", units = "cm", width=15, height=15, dpi=300)



res_lm_fit$paramA <- factor(res_lm_fit$paramA, levels=c("rubber_price", "oilpalm_price", "wages"))
res_lm_fit$paramB <- factor(res_lm_fit$paramB, levels=c("rubber_price", "oilpalm_price", "wages"))

ggplot(res_lm_fit, aes(x=output, y=Estimate, fill=category)) +
  facet_grid(paramA~paramB) +
  coord_flip() +
  geom_bar(stat="identity") +
  geom_hline(yintercept = 0, lty=2) +
  ggsci::scale_fill_jco() +
  guides(fill="none") +
  ggthemes::theme_tufte(base_size = 12) +
  theme(panel.border = element_rect(fill=NA))

util.ggsave(filename="src_grid", path="03_Analyses/09_landmarket_v5_wages", units = "cm", width=15, height=15, dpi=300)
