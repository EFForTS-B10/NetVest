


library(ggthemes)
library(nlrx)
library(tidyverse)


nl <- readRDS(file = file.path("03_Analyses/sims_constantprices_ff.rds"))
results <- nl@simdesign@simoutput



results_50 <- results %>% dplyr::filter(`[step]` == 49)

ggplot(results_50, aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=lut1.fraction)) +
  geom_tile() +
  scale_fill_viridis_c()
#scale_fill_gradient2(low="red", mid="white", high="blue", midpoint = 0.5)

ggplot(results_50, aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=hh.area.mean)) +
  geom_tile() +
  scale_fill_viridis_c()


## classify (quantiles)

p.lut <- results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
  group_by(siminputrow, `LUT-0-price`, `LUT-1-price`) %>% 
  dplyr::summarise(lut1.fraction = mean(lut1.fraction)) %>% 
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, lut1.fraction) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(lut1.fraction.group = dplyr::ntile(lut1.fraction, 4)) %>% 
  ggplot(., aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=as.factor(lut1.fraction.group))) +
  geom_tile() +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(title="lut1"))

p.area <- results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
  group_by(siminputrow, `LUT-0-price`, `LUT-1-price`) %>% 
  dplyr::summarise(hh.area.mean = mean(hh.area.mean)) %>% 
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, hh.area.mean) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(hh.area.mean.group = dplyr::ntile(hh.area.mean, 4)) %>% 
  ggplot(., aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=as.factor(hh.area.mean.group))) +
  geom_tile() +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(title="area"))


p.sar <- results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
  group_by(siminputrow, `LUT-0-price`, `LUT-1-price`) %>% 
  dplyr::summarise(p.sar = mean(p.sar)) %>% 
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, p.sar) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(p.sar.group = dplyr::ntile(p.sar, 4)) %>% 
  ggplot(., aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=as.factor(p.sar.group))) +
  geom_tile() +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(title="sar"))


p.con <- results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
  group_by(siminputrow, `LUT-0-price`, `LUT-1-price`) %>% 
  dplyr::summarise(hh.consumption.mean = mean(hh.consumption.mean)) %>% 
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, hh.consumption.mean) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(hh.consumption.mean.group = dplyr::ntile(hh.consumption.mean, 4)) %>% 
  ggplot(., aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=as.factor(hh.consumption.mean.group))) +
  geom_tile() +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(title="cons"))

p.car <- results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
  group_by(siminputrow, `LUT-0-price`, `LUT-1-price`) %>% 
  dplyr::summarise(carbon = mean(lut0.carbon + lut1.carbon)) %>% 
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, carbon) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(carbon.group = dplyr::ntile(carbon, 4)) %>% 
  ggplot(., aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=as.factor(carbon.group))) +
  geom_tile() +
  scale_fill_viridis_d() +
  guides(fill = guide_legend(title="carb"))


cowplot::plot_grid(p.lut, p.area, p.sar, p.con, p.car)


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
  scale_fill_viridis_d()

ggsave("03_Analyses/04_testing_Refforts/prices_ff.png")

### Without carbon:

classes <- 10
results_quartiles <- results %>% 
  dplyr::filter(`[step]` >= 25) %>% 
  group_by(siminputrow, `LUT-0-price`, `LUT-1-price`) %>% 
  dplyr::summarise(rubber = mean(lut1.fraction),
                   area = mean(hh.area.mean),
                   biodiversity = mean(p.sar),
                   consumption = mean(hh.consumption.mean)) %>% 
  dplyr::select(siminputrow, `LUT-0-price`, `LUT-1-price`, rubber, area, biodiversity, consumption) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate_at(c("rubber", "area", "biodiversity", "consumption"), ~dplyr::ntile(., classes))

## Plot 1:
results_quartiles %>% 
  tidyr::pivot_longer(cols=c("rubber", "area", "biodiversity", "consumption")) %>%
  dplyr::mutate(name = factor(name, levels=c("rubber", "area", "biodiversity", "consumption"))) %>% 
  ggplot(., aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=factor(value))) +
  facet_wrap(~name) +
  geom_tile() +
  scale_fill_viridis_d() +
  guides(fill=guide_legend(title="ntile"))

## Calculate trade-off:
results_quartiles %>% 
  ggplot(., aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=biodiversity + consumption)) +
  geom_tile() +
  scale_fill_viridis_c()
  
results_quartiles %>% 
  dplyr::select(`LUT-0-price`, `LUT-1-price`, biodiversity, consumption) %>% 
  tidyr::pivot_longer(cols=c("biodiversity", "consumption")) %>%
  ggplot(., aes(x=`LUT-0-price`, y=value, color=name)) +
  geom_line()



#  dplyr::mutate(to = biodiversity + consumption) %>% 
 # dplyr::mutate(to = dplyr::ntile(to, classes)) %>% 
  tidyr::pivot_longer(cols=c("rubber", "area", "biodiversity", "consumption")) %>%
  dplyr::mutate(name = factor(name, levels=c("rubber", "area", "biodiversity", "consumption"))) %>% 
  ggplot(., aes(x=`LUT-0-price`, y=`LUT-1-price`, fill=factor(value))) +
  facet_wrap(~name) +
  geom_tile() +
  scale_fill_viridis_d()
