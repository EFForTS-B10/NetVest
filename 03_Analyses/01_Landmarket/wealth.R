

library(nlrx)

## load rds from simulations:

nl <- readRDS("03_Analyses/01_Landmarket/output/sims.rds")
res <- nl@simdesign@simoutput
res <- unnest_simoutput(nl)

## Only look at heterogenous:
res_hh <- res %>% dplyr::filter(`heterogeneous-hhs?` == "true")


library(ggplot)

ggplot(res_hh, aes(x=`[step]`, y=h_wealth, color=`landmarket?`)) +
  geom_jitter(alpha=0.5)


## Look at 10% richest and 10% poorest:

res_quart <- res %>% dplyr::filter(`heterogeneous-hhs?` == "true") %>% 
  dplyr::group_by(`landmarket?`) %>% 
  dplyr::mutate(wealth_quart = dplyr::ntile(h_wealth, n = 4)) %>% 
  dplyr::ungroup()


ggplot(res_quart, aes(x=`[step]`, y=h_wealth, color=factor(`landmarket?`), fill=factor(`landmarket?`))) +
  facet_wrap(~wealth_quart, scales="free") +
  scale_y_sqrt() +
  geom_smooth()
