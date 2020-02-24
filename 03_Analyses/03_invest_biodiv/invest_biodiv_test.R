

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define variables:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

variables <- list(
  'landmarket?' = list(values=c("true", "false")),
  "allow-fallow?" = list(values=c("true", "false")),
  "heterogeneous-hhs?" = list(values=c("true", "false")),
  "price_scenario" = list(values=c("\"constant_prices\"", "\"historical_trends\""))
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define constants:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
constants <- list(
  
  ### Setup:
  "reproducable?" = "false",
  "rnd-seed" = 1234, 
  "which-map" = "\"landmarkets1\"",
  "land-use-change-decision" = "\"only-one-field-per-year\"",
  "sim-time" = 50,
  
  ### Price:
  "price-fluctuation-percent" = 10, 
  "historical_smoothing" = 0, 
  "LUT-0-folder" = "\"oilpalm_env\"",
  "LUT-0-price" = 90,
  "LUT-0-price-mu" = 1.9,
  "LUT-0-price-sd" = 1.9,
  "LUT-1-folder" = "\"rubber_env\"",
  "LUT-1-price" = 1100,
  "LUT-1-price-mu" = 11,
  "LUT-1-price-sd" = 11,
  
  ### Consumption:  
  "consumption-on?" = "true",
  "consumption_base" = 1000,
  "consumption_frac_cash" = 0.1,
  "consumption_frac_wealth" = 0.05,
  
  ### Heterogeneity and learning:
  "learning-spillover?" = "false",
  "setup-hh-network" = "\"hh-nw-n-nearest-neighbors\"",
  "hh-nw-param1" = 10,
  "hh-nw-param2" = 50,
  "spillover-share" = 1,
  
  ### Landmarket:
  "h_debt_years_max_bankrupt" = 5,
  #"landmarket?" = "true",
  "buyer_pool_n" = 10,
  "immigrant_probability" = 0.5,
  "land_price_increase" = 0.05,
  "immigrant-xp-bonus" = "\"[0 0]\"",
  "immigrant-wealth-factor" = 1,
  
  ### Wealth:
  "initial-wealth-distribution" = "\"log-normal\"",
  "init-wealth-correction-factor" = 10,
  "wealth-log-mean" = 7,
  "wealth-log-sd" = 1,
  "wealth-constant" = 10000, ## not used
  "min-wealth" = 30,
  
  ### Capital:
  "time-horizon" = 10,
  "discount-rate" = 0.1,
  "land_price" = 750,
  "external_income" = 500,
  "rent_rate_capital_lend" = 0.1,
  "rent_rate_capital_borrow" = 0.15,
  "rent_rate_land" = 0.1,
  
  ### Household age:
  "hh_age_alpha" = 14.24,
  "hh_age_lambda" = 0.31,
  "hh_age_min" = 18,
  "hh_age_max" = 80,
  "age_generation" = 40,
  "takeover_prob" = 0.5,
  
  ### Biodiversity:
  "calc_bird_richness?" = "false",
  "invest_plantdiv?" = "true",
  
  ### Management:
  #"allow-fallow?" = "false",
  
  ### Output:
  "go-once-profiler?" = "false",
  "SHOW-OUTPUT?" = "false",
  "write-maps?" = "false",
  "write-hh-data-to-file?" = "false",
  "export-view?" = "false",
  "show-homebases?" = "true",
  "show-roads?" = "true"
  
)


library(nlrx)

# Windows default NetLogo installation path (adjust to your needs!):
netlogopath <- file.path("C:/Program Files/NetLogo 6.1.0")
modelpath <- "01_EFForTS-ABM/EFForTS-ABM.nlogo"
outpath <- "03_Analyses/03_invest_biodiv/output"

nl <- nl(nlversion = "6.1.0",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

## Experiment:
nl@experiment <- experiment(expname="invest",
                            outpath=outpath,
                            repetition=1,
                            tickmetrics="true",
                            idsetup="setup-with-external-maps",
                            idgo="go",
                            runtime=50,
                            metrics=c("item 1 item 0 carbon", "item 1 item 1 carbon",
                                      "item 0 prices", "item 1 prices",
                                      "item 0 LUT-fractions", "item 1 LUT-fractions",
                                      "count hhs", "count hhs with [h_immigrant? = TRUE]",
                                      "precision mean [h_area] of hhs 3",
                                      "mean_hh_consumption",
                                      "sar_ratio"),
                            #metrics.turtles = list("hhs"=c("pxcor", "pycor", "h_age", "h_wealth", "h_netcashflow", "h_debts")),
                            #metrics.patches = c("pxcor", "pycor", "p_age", "p_landuse", "p_capitalstock", "p_carbon", "p_actual_production", "p_owner"),
                            variables = variables,
                            constants = constants)

nl@simdesign <- simdesign_ff(nl = nl, nseeds = 3)

## Run simulations:
library(future)
plan(multisession)
#results <- run_nl_all(nl, split=nrow(nl@simdesign@siminput))
results <- run_nl_all(nl)

## Attach output:
setsim(nl, "simoutput") <- results
saveRDS(nl, file = file.path(outpath, "sims.rds"))
nl <- readRDS(file.path(outpath, "sims.rds"))
results <- nl@simdesign@simoutput



library(tidyverse)

# Rename factors:
results$`landmarket?` <- ifelse(results$`landmarket?` == TRUE, "landmarket", "no landmarket")
results$`allow-fallow?` <- ifelse(results$`allow-fallow?` == TRUE, "fallow", "no fallow")
results$`heterogeneous-hhs?` <- ifelse(results$`heterogeneous-hhs?` == TRUE, "heterogeneous hhs", "homogeneous hhs")
# 
# # Filter tick 0:
# results.agg <- results %>% dplyr::filter(`[step]` > 0) %>% 
#   group_by(`landmarket?`, `allow-fallow?`, `heterogeneous-hhs?`, `price_scenario`, `[step]`) %>% 
#   summarize(cons.mu = mean(`mean_hh_consumption`), cons.sd = sd(`mean_hh_consumption`),
#             carb.op.mu = mean(`item 1 item 0 carbon`), carb.op.sd = sd(`item 1 item 0 carbon`),
#             carb.rm.mu = mean(`item 1 item 1 carbon`), carb.rm.sd = sd(`item 1 item 1 carbon`),
#             sar.mu = mean(sar_ratio), sar.sd = sd(sar_ratio))
# 
# 
# 
# ggplot(results.agg) +
#   facet_grid(~`heterogeneous-hhs?`) +
#   geom_line(aes(x=`[step]`, y=sar.mu, color=`landmarket?`, lty=`allow-fallow?`)) +
#   geom_ribbon(aes(x=`[step]`, ymin=sar.mu - sar.sd, ymax=sar.mu + sar.sd, fill=`landmarket?`))
# 
# 
# 
# ggplot(results.agg) +
#   facet_grid(~`heterogeneous-hhs?`) +
#   geom_line(aes(x=`[step]`, y=cons.mu, color=`landmarket?`, lty=`allow-fallow?`))
# 
# 
# ### Remove the time dimension:
# results.agg <- results %>% dplyr::filter(`[step]` > 0) %>% 
#   group_by(`landmarket?`, `allow-fallow?`, `heterogeneous-hhs?`) %>% 
#   summarize(cons.mu = mean(`mean_hh_consumption`), cons.sd = sd(`mean_hh_consumption`),
#             carb.op.mu = mean(`item 1 item 0 carbon`), carb.op.sd = sd(`item 1 item 0 carbon`),
#             carb.rm.mu = mean(`item 1 item 1 carbon`), carb.rm.sd = sd(`item 1 item 1 carbon`),
#             sar.mu = mean(sar_ratio), sar.sd = sd(sar_ratio))
# 
# 
# # boxplot
# ggplot(results, aes(x=paste0(`landmarket?`, `allow-fallow?`))) +
#   facet_grid(~`heterogeneous-hhs?`) +
#   geom_boxplot(aes(y=`mean_hh_consumption`))
# 
# 
# ggplot(results, aes(x=paste0(`landmarket?`, `allow-fallow?`))) +
#   facet_grid(~`heterogeneous-hhs?`) +
#   geom_boxplot(aes(y=sar_ratio))
# 
# 
# 

##################################
## Try trade-off analysis:


# first we calculate for each combination of outputs (e.g. carbon/consumption, carbon/sar, consumption/sar) the percentage change in comparison to the last time step
# then display the sum? in a tile plot
# on the x axis: time, on the y axis: the combination

results.agg <- results %>% dplyr::filter(`[step]` > 0) %>% 
  group_by(`landmarket?`, `allow-fallow?`, `heterogeneous-hhs?`, `price_scenario`, `[step]`) %>% 
  summarize(cons.mu = mean(`mean_hh_consumption`), cons.sd = sd(`mean_hh_consumption`),
            carb.mu = mean(`item 1 item 0 carbon` + `item 1 item 1 carbon`), carb.sd = sd(`item 1 item 0 carbon` + `item 1 item 1 carbon`),
            sar.mu = mean(sar_ratio), sar.sd = sd(sar_ratio))


results.agg.lag <- results.agg %>% 
  group_by(`landmarket?`, `allow-fallow?`, `heterogeneous-hhs?`, `price_scenario`) %>% 
  mutate(carb.change = ((carb.mu - lag(carb.mu)) / lag(carb.mu)) * 100,
         cons.change = ((cons.mu - lag(cons.mu)) / lag(cons.mu)) * 100,
         sar.change = ((sar.mu - lag(sar.mu)) / lag(sar.mu)) * 100) %>% 
  mutate(tradeoff = case_when(carb.change > 0 & cons.change > 0 & sar.change > 0 ~ "total synergy",
                   carb.change <= 0 & cons.change <= 0 & sar.change <= 0 ~ "total loss",
                   carb.change > 0 & cons.change <= 0 & sar.change <= 0 ~ "carbon gain",
                   carb.change <= 0 & cons.change > 0 & sar.change <= 0 ~ "consumption gain",
                   carb.change <= 0 & cons.change <= 0 & sar.change > 0 ~ "biodiversity gain",
                   carb.change > 0 & cons.change > 0 & sar.change <= 0 ~ "carbon and consumption gain",
                   carb.change > 0 & cons.change <= 0 & sar.change > 0 ~ "carbon and biodiversity gain",
                   carb.change <= 0 & cons.change > 0 & sar.change > 0 ~ "consumption and biodiversity gain")) %>% 
  rowwise() %>% 
  mutate(tradeoff.mu = mean(c(carb.change, cons.change, sar.change)))


library(ggpubr)
library(ggthemes)
library(ggsci)


## Tradeoff plot, consumption carbon:
ggplot(results.agg.lag) +
  facet_grid(`landmarket?`~`allow-fallow?`) +
  geom_point(aes(x=cons.change, y=carb.change, color = paste(`price_scenario`, " + ", `heterogeneous-hhs?`)), size=1.5) +
  #stat_chull(aes(x=cons.change, y=carb.change, color = paste(`price_scenario`, " + ", `heterogeneous-hhs?`), fill = paste(`price_scenario`, " + ", `heterogeneous-hhs?`)), 
   #          alpha = 0.05, geom = "polygon") +
  #geom_density_2d(aes(x=cons.change, y=carb.change, color = paste(`price_scenario`, " + ", `heterogeneous-hhs?`)), alpha=0.4, contour=FALSE) +
  stat_ellipse(aes(x=cons.change, y=carb.change, color = paste(`price_scenario`, " + ", `heterogeneous-hhs?`))) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  guides(fill=guide_legend(title="Scenario"), color=guide_legend(title="Scenario")) +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  theme_light() +
  theme(axis.text = element_text(size=11),
        axis.title = element_text(size=11),
        strip.text = element_text(size=11),
        legend.title = element_text(size=11),
        legend.text = element_text(size=11))
  
ggsave("tradeoffplot_cons_carb.png", path = "03_Analyses/03_invest_biodiv/", width=16)

## Tradeoff plot, consumption biodiv:
g2 <- ggplot(results.agg.lag) +
  facet_grid(`landmarket?`~`allow-fallow?`) +
  geom_point(aes(x=cons.change, y=sar.change, color = paste(`price_scenario`, " + ", `heterogeneous-hhs?`)), size=1.5) +
  stat_chull(aes(x=cons.change, y=sar.change, color = paste(`price_scenario`, " + ", `heterogeneous-hhs?`), fill = paste(`price_scenario`, " + ", `heterogeneous-hhs?`)), 
             alpha = 0.05, geom = "polygon") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  guides(fill=guide_legend(title="Scenario"), color=guide_legend(title="Scenario")) +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  theme_light() +
  theme(axis.text = element_text(size=11),
        axis.title = element_text(size=11),
        strip.text = element_text(size=11),
        legend.title = element_text(size=11),
        legend.text = element_text(size=11))

ggsave("tradeoffplot_cons_biodiv.png", path = "03_Analyses/03_invest_biodiv/", width=16)


### 2nd version:
# 

library(ggpubr)
library(ggthemes)
library(ggsci)
library(patchwork)
library(alphahull)


## Aggregate Data
results.agg <- results %>% dplyr::filter(`[step]` > 0) %>% 
  group_by(`landmarket?`, `allow-fallow?`, `heterogeneous-hhs?`, `price_scenario`, `[step]`) %>% 
  summarize(cons.mu = mean(`mean_hh_consumption`), cons.sd = sd(`mean_hh_consumption`),
            carb.mu = mean(`item 1 item 0 carbon` + `item 1 item 1 carbon`), carb.sd = sd(`item 1 item 0 carbon` + `item 1 item 1 carbon`),
            sar.mu = mean(sar_ratio), sar.sd = sd(sar_ratio))


results.agg.lag <- results.agg %>% 
  group_by(`landmarket?`, `allow-fallow?`, `heterogeneous-hhs?`, `price_scenario`) %>% 
  mutate(carb.change = ((carb.mu - lag(carb.mu)) / lag(carb.mu)) * 100,
         cons.change = ((cons.mu - lag(cons.mu)) / lag(cons.mu)) * 100,
         sar.change = ((sar.mu - lag(sar.mu)) / lag(sar.mu)) * 100) %>% 
  mutate(tradeoff = case_when(carb.change > 0 & cons.change > 0 & sar.change > 0 ~ "total synergy",
                              carb.change <= 0 & cons.change <= 0 & sar.change <= 0 ~ "total loss",
                              carb.change > 0 & cons.change <= 0 & sar.change <= 0 ~ "carbon gain",
                              carb.change <= 0 & cons.change > 0 & sar.change <= 0 ~ "consumption gain",
                              carb.change <= 0 & cons.change <= 0 & sar.change > 0 ~ "biodiversity gain",
                              carb.change > 0 & cons.change > 0 & sar.change <= 0 ~ "carbon and consumption gain",
                              carb.change > 0 & cons.change <= 0 & sar.change > 0 ~ "carbon and biodiversity gain",
                              carb.change <= 0 & cons.change > 0 & sar.change > 0 ~ "consumption and biodiversity gain")) %>% 
  rowwise() %>% 
  mutate(tradeoff.mu = mean(c(carb.change, cons.change, sar.change)))


## Convert alpha shape to plottable olygon data:
fortify.ashape <- function(ashape_res) {
  xdf <- data.frame(ashape_res$edges)
  xdf <- do.call(
    rbind,
    lapply(1:nrow(xdf), function(i) {
      rbind(
        data.frame(x=xdf$x1[i], y=xdf$y1[i]),
        data.frame(x=xdf$x2[i], y=xdf$y2[i])
      )
    })
  )
  xdf <- xdf[order(-1 * atan2(
    xdf$y - mean(range(xdf$y)), 
    xdf$x - mean(range(xdf$x)))), c("x", "y")]
  xdf <- rbind.data.frame(xdf[nrow(xdf),], xdf[1:(nrow(xdf)-1),])
  xdf
}

## Loop trough factor combinations:
f <- c("fallow", "no fallow")
l <- c("landmarket", "no landmarket")
comb <- expand.grid(f, l)

for (i in 1:nrow(comb))
{
  
  ## Extract data:
  results.agg.lag.plot <- results.agg.lag %>%
    dplyr::filter(`allow-fallow?` == comb$Var1[i]) %>%
    dplyr::filter(`landmarket?` == comb$Var2[i]) %>%
    mutate(scenario = paste(`price_scenario`, " + ", `heterogeneous-hhs?`))
  
  # Calculate alpha hull for each group:
  # scenarios <- unique(results.agg.lag.plot$scenario)
  # hulls_carb <- NULL
  # hulls_biodiv <- NULL
  # 
  # for(j in 1:length(scenarios))
  # {
  #   hulldata <- results.agg.lag.plot %>% 
  #     dplyr::filter(scenario == scenarios[j]) %>% 
  #     dplyr::select(cons.change,
  #                   carb.change,
  #                   sar.change) %>% 
  #     tidyr::drop_na()
  #   
  #   w_carb <- fortify(alphahull::ashape(hulldata$cons.change , hulldata$carb.change, alpha = 200))  #alphahull::ashape(hulldata$cons.change , hulldata$carb.change, alpha = 1)
  #   w_biodiv <- fortify(alphahull::ashape(hulldata$cons.change, hulldata$sar.change, alpha = 200)) 
  #   
  #   hulls_carb <- rbind(hulls_carb,
  #                       data.frame(x = w_carb$x, y = w_carb$y, scenario = scenarios[j]))
  #   hulls_biodiv <- rbind(hulls_biodiv, 
  #                         data.frame(x = w_biodiv$x, y = w_biodiv$y, scenario = scenarios[j]))
  #   
  # }
  
  ## Color palette:
  col.pal <- c("constant_prices  +  heterogeneous hhs" =  "#244764",  # "#00AFBB", 
               "historical_trends  +  heterogeneous hhs" = "#E7B800", 
               "constant_prices  +  homogeneous hhs" = "#FC4E07", 
               "historical_trends  +  homogeneous hhs" = "#28995F")
  
  ### CARBON PLOT:
  
  plot1 <- ggplot(results.agg.lag.plot) + 
    geom_point(aes(x = cons.change, y = carb.change, color = scenario), size = 1) + 
    #geom_polygon(data=hulls_carb, aes(x, y, fill=scenario), alpha=0.2) +
    stat_chull(aes(x=cons.change, y=carb.change, color = scenario, fill = scenario), 
               alpha = 0.05, geom = "polygon") +
    geom_hline(yintercept = 0, lty=3) +
    geom_vline(xintercept = 0, lty=3) +
    scale_color_manual(values = col.pal) +
    scale_fill_manual(values = col.pal) +
    guides(color="none", fill="none") +
    theme_light()
  
  dens1 <- ggplot(results.agg.lag.plot, aes(x = cons.change, fill = scenario)) + 
    geom_density(alpha = 0.4) + 
    theme_void() + 
    scale_fill_manual(values = col.pal) +
    theme(legend.position = "none")
  
  dens2 <- ggplot(results.agg.lag.plot, aes(x = carb.change, fill = scenario)) + 
    geom_density(alpha = 0.4) + 
    theme_void() + 
    scale_fill_manual(values = col.pal) +
    theme(legend.position = "none") + 
    coord_flip()
  
  dens1 + plot_spacer() + plot1 + dens2 + 
    plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
  
  ggsave(paste0("tradeoff_carbon_", comb$Var1[i], "_", comb$Var2[i], ".png"), path = "03_Analyses/03_invest_biodiv/", width=6, height=6)
  
  ### BIODIV PLOT:
  
  plot1 <- ggplot(results.agg.lag.plot) + 
    geom_point(aes(x = cons.change, y = sar.change, color = scenario), size = 1) + 
    #geom_polygon(data=hulls_biodiv, aes(x, y, fill=scenario), alpha=0.2) +
    stat_chull(aes(x=cons.change, y=sar.change, color = scenario, fill = scenario), 
               alpha = 0.05, geom = "polygon") +
    geom_hline(yintercept = 0, lty=3) +
    geom_vline(xintercept = 0, lty=3) +
    scale_color_manual(values = col.pal) +
    scale_fill_manual(values = col.pal) +
    guides(color="none", fill="none") +
    theme_light()
  
  dens1 <- ggplot(results.agg.lag.plot, aes(x = cons.change, fill = scenario)) + 
    geom_density(alpha = 0.4) + 
    theme_void() + 
    scale_fill_manual(values = col.pal) +
    theme(legend.position = "none")
  
  dens2 <- ggplot(results.agg.lag.plot, aes(x = sar.change, fill = scenario)) + 
    geom_density(alpha = 0.4) + 
    theme_void() + 
    scale_fill_manual(values = col.pal) +
    theme(legend.position = "none") + 
    coord_flip()
  
  dens1 + plot_spacer() + plot1 + dens2 + 
    plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
  
  ggsave(paste0("tradeoff_biodiv_", comb$Var1[i], "_", comb$Var2[i], ".png"), path = "03_Analyses/03_invest_biodiv/", width=6, height=6)
}






results.agg.lag.plot <- results.agg.lag %>%
  dplyr::filter(`allow-fallow?` == "fallow") %>%
  dplyr::filter(`landmarket?` == "no landmarket") %>%
  mutate(scenario = paste(`price_scenario`, " + ", `heterogeneous-hhs?`))

ggscatterhist(
  results.agg.lag.plot,
  x = "cons.change", y = "carb.change",
  color = "scenario",
  size = 1.5, alpha = 0.6,
  palette = c("#00AFBB", "#E7B800", "#FC4E07", "#28995F"),
  margin.params = list(fill = "scenario", color = "black", size = 0.2),
  ggtheme = theme_light()
)
ggsave("tradeoffplot_hist_cons_carb.png", path = "03_Analyses/03_invest_biodiv/", width=5, height=5)
ggscatterhist(
  results.agg.lag.plot,
  x = "cons.change", y = "sar.change",
  color = "scenario",
  size = 1.5, alpha = 0.6,
  palette = c("#00AFBB", "#E7B800", "#FC4E07", "#28995F"),
  margin.params = list(fill = "scenario", color = "black", size = 0.2),
  ggtheme = theme_light()
)
ggsave("tradeoffplot_hist_cons_biodiv.png", path = "03_Analyses/03_invest_biodiv/", width=5, height=5)





results.agg.lag.plot <- results.agg.lag %>%
  dplyr::filter(`allow-fallow?` == "fallow") %>%
  dplyr::filter(`landmarket?` == "landmarket") %>%
  mutate(scenario = paste(`price_scenario`, " + ", `heterogeneous-hhs?`))

results.agg.lag.plot <- results.agg.lag %>%
  dplyr::filter(`allow-fallow?` == "fallow") %>%
  dplyr::filter(`landmarket?` == "landmarket") %>%
  mutate(scenario = paste(`price_scenario`, " + ", `heterogeneous-hhs?`))

ggscatterhist(
  results.agg.lag.plot,
  x = "cons.change", y = "carb.change",
  color = "scenario",
  size = 3, alpha = 0.6,
  palette = c("#00AFBB", "#E7B800", "#FC4E07", "#28995F"),
  margin.params = list(fill = "scenario", color = "black", size = 0.2),
  ggtheme = theme_light()
)



######################################################
######################################################
######################################################



results.agg.lag.gather <- results.agg.lag %>% 
  select(`landmarket?`, `allow-fallow?`, `heterogeneous-hhs?`, `[step]`, carb.change, cons.change, sar.change, tradeoff.mu) %>% 
  gather(key, value, carb.change, cons.change, sar.change, tradeoff.mu)


## only carbon:
results.agg.lag.gather.carb <- results.agg.lag.gather %>% dplyr::filter(key=="carb.change")

ggplot(results.agg.lag.gather.carb, aes(x=`[step]`, y=`allow-fallow?`, fill=value)) +
  facet_grid(`landmarket?`~`heterogeneous-hhs?`, scales="free") +
  geom_tile() +
  scale_fill_gradient2() +
  theme_minimal()

## only consumption
results.agg.lag.gather.cons <- results.agg.lag.gather %>% dplyr::filter(key=="cons.change")

ggplot(results.agg.lag.gather.cons, aes(x=`[step]`, y=`allow-fallow?`, fill=value)) +
  facet_grid(`landmarket?`~`heterogeneous-hhs?`, scales="free") +
  geom_tile() +
  scale_fill_gradient2() +
  theme_minimal()

## only biodiv:
results.agg.lag.gather.sar <- results.agg.lag.gather %>% dplyr::filter(key=="sar.change")

ggplot(results.agg.lag.gather.sar, aes(x=`[step]`, y=`allow-fallow?`, fill=value)) +
  facet_grid(`landmarket?`~`heterogeneous-hhs?`, scales="free") +
  geom_tile() +
  scale_fill_gradient2() +
  theme_minimal()

## mean:
results.agg.lag.gather.tradeoff <- results.agg.lag.gather %>% dplyr::filter(key=="tradeoff.mu")

ggplot(results.agg.lag.gather.tradeoff, aes(x=`[step]`, y=`allow-fallow?`, fill=value)) +
  facet_grid(`landmarket?`~`heterogeneous-hhs?`, scales="free") +
  geom_tile() +
  scale_fill_gradient2() +
  theme_minimal()




############################

ggplot(results) +
  geom_point(aes(x=`mean_hh_consumption`, y=`item 1 item 0 carbon`), color="red") +
  geom_point(aes(x=`mean_hh_consumption`, y=`item 1 item 1 carbon`), color="gold")

ggplot(results) +
  geom_point(aes(x=sar_ratio, y=`item 1 item 0 carbon`), color="red") +
  geom_point(aes(x=sar_ratio, y=`item 1 item 1 carbon`), color="gold")

ggplot(results) +
  geom_point(aes(x=`mean_hh_consumption`, y=sar_ratio))

ggplot(results) +
  geom_point(aes(x=`item 1 item 0 carbon` + `item 1 item 1 carbon`, y=sar_ratio))



ggplot(results) +
  facet_grid(`landmarket?`~`allow-fallow?`) +
  geom_line(aes(x=`[step]`, y=`mean_hh_consumption`))


ggplot(results) +
  facet_grid(`landmarket?`~`allow-fallow?`) +
  geom_line(aes(x=sar_ratio, y=`mean_hh_consumption`))
