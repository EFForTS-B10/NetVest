


library(ggthemes)
library(nlrx)
library(tidyverse)

## Load simulation data
nl <- readRDS(file = file.path("03_Analyses/05_Refforts_constantPrices/constantprices_ff_ineff.rds"))
results <- nl@simdesign@simoutput

### Without carbon:

classes <- 8
var.names <- c("rubber_area", "oilpalm_area", "household_nr", "immigrant_nr", "household_size", "abandoned", "capitalstock", "consumption_k", "carbon_k", "biodiversity", "area_mn", "ed")
results_grouped <- results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
  group_by(siminputrow, `LUT-0-price`, `LUT-1-price`) %>% 
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
                   shdi = mean(shdi, na.rm = TRUE)) %>% 
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, var.names) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(var.names, ~cut(., classes)) %>% 
  tidyr::pivot_longer(cols=var.names)
 # dplyr::mutate_at(var.names, ~dplyr::ntile(., classes))


plots <- purrr::map(var.names, function(x) {
  results_grouped.x <- results_grouped %>% dplyr::filter(name == x)
  
  p.x <- ggplot(results_grouped.x, aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=factor(value))) +
    geom_tile() +
    xlab("palm oil price [$/ton]") +
    ylab("rubber price [$/ton]") +
    scale_fill_viridis_d() +
    guides(fill=guide_legend(title=x)) +
    theme_tufte(base_size = 11)
  
  return(p.x)
})


cowplot::plot_grid(plotlist=plots, ncol = 3)
ggsave("03_Analyses/05_Refforts_constantPrices/prices_ff_tradeoff_p1.png", units = "cm", width=30, height=30, dpi=300)


### Find best synergy:
classes <- 4
var.names <- c("consumption_k", "carbon_k", "biodiversity")
results_grouped <- results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
  group_by(siminputrow, `LUT-0-price`, `LUT-1-price`) %>% 
  dplyr::summarise(consumption_k = mean(hh.consumption.mean) / 1000,
                   carbon_k = mean(lut0.carbon + lut1.carbon) / 1000,
                   biodiversity = mean(p.sar)) %>% 
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, var.names) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(var.names, ~cut(., classes, labels=FALSE)) %>% 
  dplyr::mutate(consumption_carbon = round(((consumption_k + carbon_k) / 2), digits=2)) %>%  
  dplyr::mutate(consumption_biodiversity = round(((consumption_k + biodiversity) / 2), digits=2)) %>%
  dplyr::mutate(consumption_carbon_biodiversity = round(((consumption_k + biodiversity + carbon_k) / 3), digits=2)) %>%
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, consumption_carbon, consumption_biodiversity, consumption_carbon_biodiversity) %>% 
  tidyr::pivot_longer(cols=c("consumption_carbon", "consumption_biodiversity", "consumption_carbon_biodiversity"))


ggplot(results_grouped, aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=factor(value))) +
  facet_wrap(~name) +
  geom_tile() +
  xlab("palm oil price [$/ton]") +
  ylab("rubber price [$/ton]") +
  scale_fill_viridis_d() +
  guides(fill=guide_legend(title="mean score")) +
  theme_tufte(base_size = 11)
ggsave("03_Analyses/05_Refforts_constantPrices/prices_ff_tradeoff_p2.png", units = "cm", width=28, height=10, dpi=300)


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
