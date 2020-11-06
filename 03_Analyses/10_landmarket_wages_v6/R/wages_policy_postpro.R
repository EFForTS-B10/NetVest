
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

main.dir <- "03_Analyses/10_landmarket_wages_v6/"

## Load preliminary results:
nl <- readRDS(file.path(main.dir, "data/sims_wages_policy.rds"))
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
  dplyr::mutate(wages_op = as.numeric(stringr::str_split(`LUT-0-folder`, pattern="_")[[1]][[3]])) %>% 
  dplyr::mutate(wages_rb = as.numeric(stringr::str_split(`LUT-1-folder`, pattern="_")[[1]][[3]]))

res_agg <- res_agg %>% 
  dplyr::rename(rubber_price = `LUT-1-price`,
                oilpalm_price = `LUT-0-price`) %>% 
  group_by(siminputrow, rubber_price, oilpalm_price, wages_op, wages_rb) %>% 
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
  dplyr::select(rubber_price, oilpalm_price, wages_op, wages_rb, var.names) %>% 
  dplyr::mutate(landscape_type = case_when(rubber_area >= 0.75 ~ "rubber_dominated",
                                           oilpalm_area >= 0.75 ~ "oilpalm_dominated",
                                           rubber_area < 0.75 & oilpalm_area < 0.75 ~ "mix")) %>% 
  dplyr::mutate(rubber_price_rank = dplyr::percent_rank(rubber_price),
                oilpalm_price_rank = dplyr::percent_rank(oilpalm_price)) %>% 
  dplyr::mutate(price_rank_mu = (rubber_price_rank + oilpalm_price_rank) / 2)

####################################################

# only use min and max rubber prices:
res_agg_yield <- res_agg %>% 
  dplyr::filter(rubber_price == max(rubber_price) | rubber_price == min(rubber_price)) %>% 
  dplyr::filter(oilpalm_price == max(oilpalm_price) | oilpalm_price == min(oilpalm_price)) %>% 
  dplyr::mutate(prices = case_when(rubber_price == min(rubber_price) & oilpalm_price == min(oilpalm_price) ~ "low",
                                   rubber_price == max(rubber_price) & oilpalm_price == max(oilpalm_price) ~ "high",
                                   rubber_price == min(rubber_price) & oilpalm_price == max(oilpalm_price) ~ "oilpalm-boom",
                                   rubber_price == max(rubber_price) & oilpalm_price == min(oilpalm_price) ~ "rubber-boom"))

####################################################


## CALCULATING THE DIFFERENCES BETWEEN EACH GROUP WITH RUBBER WAGES 1 and 2
res_plot <- res_agg_yield %>% 
  dplyr::select(prices, wages_rb, var.names) %>% 
  tidyr::pivot_wider(names_from=wages_rb, values_from=var.names) %>% 
  dplyr::mutate(biodiversity = ((biodiversity_1 - biodiversity_2) / biodiversity_2) * 100,
                consumption_k = ((consumption_k_1 - consumption_k_2) / consumption_k_2) * 100,
                carbon_k = ((carbon_k_1 - carbon_k_2) / carbon_k_2) * 100) %>% 
  dplyr::select(prices, biodiversity, carbon_k, consumption_k) %>% 
  tidyr::pivot_longer(c(biodiversity, carbon_k, consumption_k))

rank_cols <- c("biodiversity" = "#28995F",
               "carbon_k" = "#244764",
               "consumption_k" = "#FBCB58")

p_policy <- ggplot(res_plot, aes(x=prices, y=value, fill=name)) +
  geom_bar(stat="identity", position = "dodge") +
  geom_hline(yintercept = 0, lty=1) +
  scale_fill_manual(values=rank_cols) +
  guides(fill=guide_legend(title="function")) +
  xlab("price scenario") +
  ylab("effect of subsidies (-50% on rubber wages)") +
  ggthemes::theme_tufte(base_size=12)

util.ggsave(plot=p_policy, filename="policy_subsidies", path=file.path(main.dir, "figures"), units = "cm", width=14, height=9, dpi=300)  


