


library(ggthemes)
library(nlrx)
library(tidyverse)


#nl <- readRDS(file = file.path("03_Analyses/sims_constantprices_ff.rds"))

nl <- readRDS(file = file.path("03_Analyses/04_testing_Refforts/sims_constantprices_ff_heterogenous.rds"))
results <- nl@simdesign@simoutput


## Calculate landscape metrics:
nl_sp <- nlrx::nl_to_raster(nl_spatial)
#nl_sp <- nlrx::nl_to_raster(nl)
metrics <- c("lsm_l_ed", "lsm_l_shdi", "lsm_l_lsi", "lsm_l_lpi")
results <- cbind(nl_sp,
                 landscapemetrics::calculate_lsm(nl_sp$spatial.raster, what=metrics) %>% 
                   dplyr::select(layer, metric, value) %>% 
                   tidyr::pivot_wider(names_from=metric, values_from = value))
  



######## FACET VERSION:
classes <- 10
results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
  group_by(siminputrow, `LUT-0-price`, `LUT-1-price`) %>% 
  dplyr::summarise(rubber = mean(lut1.fraction),
                   area = mean(hh.area.mean),
                   biodiversity = mean(p.sar),
                   consumption = mean(hh.consumption.mean),
                   carbon = mean(lut0.carbon + lut1.carbon)) %>% 
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, rubber, area, biodiversity, consumption, carbon) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(c("rubber", "area", "biodiversity", "consumption", "carbon"), ~dplyr::ntile(., classes)) %>% 
  dplyr::mutate(to = biodiversity + consumption + carbon) %>% 
  dplyr::mutate(to = dplyr::ntile(to, classes)) %>% 
  tidyr::pivot_longer(cols=c("rubber", "area", "biodiversity", "consumption", "carbon", "to")) %>%
  dplyr::mutate(name = factor(name, levels=c("rubber", "area", "biodiversity", "consumption", "carbon", "to"))) %>% 
  ggplot(., aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=factor(value))) +
  facet_wrap(~name) +
  geom_tile() +
  scale_fill_viridis_d(option = "E")

ggsave("03_Analyses/04_testing_Refforts/prices_ff.png")

### Without carbon:

classes <- 8
vars <- c("rubber_area", "oilpalm_area", "household_size", "capitalstock", "consumption", "biodiversity")
results_quartiles <- results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
  group_by(siminputrow, `LUT-0-price`, `LUT-1-price`) %>% 
  dplyr::summarise(rubber_area = mean(lut1.fraction),
                   oilpalm_area = mean(lut0.fraction),
                   household_size = mean(hh.area.mean),
                   capitalstock = mean(p.capitalstock.mean),
                   consumption = mean(hh.consumption.mean),
                   biodiversity = mean(p.sar)) %>% 
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, rubber_area, oilpalm_area, household_size, capitalstock, consumption, biodiversity) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(vars, ~dplyr::ntile(., classes))

## Plot 1:
p1 <- results_quartiles %>% 
  tidyr::pivot_longer(cols=vars) %>%
  dplyr::mutate(name = factor(name, levels=vars)) %>% 
  ggplot(., aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=factor(value))) +
  facet_wrap(~name) +
  geom_tile() +
  xlab("palm oil price [$/ton]") +
  ylab("rubber price [$/ton]") +
  scale_fill_viridis_d() +
  guides(fill="none") +
  theme_tufte(base_size = 11)

## Calculate trade-off:
p2 <- results_quartiles %>% 
  dplyr::mutate(to = consumption + biodiversity) %>%
  dplyr::mutate(to = dplyr::ntile(to, 3)) %>% 
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, to) %>% 
  ggplot(., aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=to)) +
  geom_tile() +
  scale_fill_viridis_c() +
  xlab("palm oil price [$/ton]") +
  ylab("rubber price [$/ton]") +
  guides(fill="none") +
  theme_tufte(base_size = 11)

cowplot::plot_grid(p1, p2, 
                   rel_widths = c(3,2), 
                   labels = c("", "consumption + biodiversity"), 
                   label_fontfamily = "serif", 
                   label_size = 11, 
                   label_fontface = "plain")
ggsave("03_Analyses/04_testing_Refforts/prices_ff_tradeoff.png", units = "cm", width=24, height=12, dpi=300)


results_quartiles %>% 
  dplyr::select(-siminputrow, -`LUT-0-price`, -`LUT-1-price`) %>% 
  cor(., method="kendall") %>% 
  data.frame(.) %>% 
  dplyr::mutate(row = rownames(.)) %>% 
  tidyr::gather(column, Correlation, -row) %>% 
  dplyr::mutate(Correlation = round(Correlation, digits=2)) %>% 
  ggplot(., aes(x=row, y=column, fill=Correlation)) +
  geom_tile() +
  paletteer::scale_fill_paletteer_c("pals::ocean.curl", direction = -1)

ggsave("03_Analyses/04_testing_Refforts/prices_ff_correlations.png", units = "cm", width=20, height=12, dpi=300)
