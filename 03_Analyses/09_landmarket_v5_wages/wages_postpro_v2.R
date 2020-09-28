
### use hh nr as a proxy for economic pressure



res_agg_yield %>% 
  group_by(wages, prices)


ggplot(res_agg_yield, aes(x=, y=carbon_k)) +
  geom_point(size=2) +
  geom_line(size=1) +
 # scale_color_manual(values = price_cols) +
  ggthemes::theme_tufte()


hullplot <- function(data, xvar, yvar)
{
  data <- data %>% 
    dplyr::rename(xvar = xvar, yvar = yvar)
  
  hull <- data %>%
    group_by(prices) %>% 
    slice(chull(xvar, yvar))
  
  ggplot(data, aes(x=xvar, y=yvar, color=factor(household_nr), fill=factor(household_nr))) +
    geom_point() +
    geom_polygon(data=hull, alpha=0.1) +
    labs(x=xvar, y=yvar) +
    #scale_color_manual(values = price_cols) +
    #scale_fill_manual(values = price_cols) +
    ggthemes::theme_tufte(base_size=12)
  
}
hullplot(res_agg_yield, "biodiversity", "carbon_k") + 
  geom_vline(xintercept=mean(res_agg_yield$biodiversity), lty=2) +
  geom_hline(yintercept=mean(res_agg_yield$carbon_k), lty=2)


ggplot(res_agg_yield, aes(x=biodiversity, y=carbon_k, color=oilpalm_area)) +
  geom_point() +
  scale_color_viridis_c() +
  ggthemes::theme_tufte(base_size=12) +
  geom_vline(xintercept=mean(res_agg_yield$biodiversity), lty=2) +
  geom_hline(yintercept=mean(res_agg_yield$carbon_k), lty=2)

ggplot(res_agg_yield, aes(x=biodiversity, y=consumption_k, color=oilpalm_area)) +
  geom_point() +
  scale_color_viridis_c() +
  ggthemes::theme_tufte(base_size=12) +
  geom_vline(xintercept=mean(res_agg_yield$biodiversity), lty=2) +
  geom_hline(yintercept=mean(res_agg_yield$consumption_k), lty=2)

ggplot(res_agg_yield, aes(x=carbon_k, y=consumption_k, color=oilpalm_area)) +
  geom_point() +
  scale_color_viridis_c() +
  ggthemes::theme_tufte(base_size=12) +
  geom_vline(xintercept=mean(res_agg_yield$carbon_k), lty=2) +
  geom_hline(yintercept=mean(res_agg_yield$consumption_k), lty=2)



### ranks:
tt <- res_agg_yield %>% 
  dplyr::select(wages, prices, consumption_k, carbon_k, biodiversity) %>% 
  dplyr::mutate(consumption_rank = dplyr::percent_rank(consumption_k),
                carbon_rank = dplyr::percent_rank(carbon_k),
                biodiversity_rank = dplyr::percent_rank(biodiversity)) %>% 
  dplyr::mutate(rank = (consumption_rank + carbon_rank + biodiversity_rank))


ggplot(tt, aes(x=wages, y=rank, color=prices)) +
  geom_point()

