


## Scenarios:

## It does not make much sense to look at landmarket on/off.
## because without landmarket, the model gives inaccurate results anyway.
## So this would be comparing a good model with a bad model essentially.
## What we could do instead:
##
## Look at increasing wages!

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define variables:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

variables = list(
  "LUT-0-folder" = list(values=rep(c("\"oilpalm_labor_min\"", "\"oilpalm_labor_med\"", "\"oilpalm_labor_high\""), 2)),
  "LUT-1-folder" = list(values=rep(c("\"rubber_labor_min\"", "\"rubber_labor_med\"", "\"rubber_labor_high\""), 2)),
  "historical_smoothing" = list(values=c(rep(0, 3), rep(3, 3)))
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
  "price_scenario" = "\"historical_trends\"",
  "price-fluctuation-percent" = 10, 
  #"historical_smoothing" = 0, 
  #"LUT-0-folder" = "\"oilpalm_env\"",
  "LUT-0-price" = 90,
  "LUT-0-price-mu" = 1.9,
  "LUT-0-price-sd" = 1.9,
  #"LUT-1-folder" = "\"rubber_env\"",
  "LUT-1-price" = 1100,
  "LUT-1-price-mu" = 11,
  "LUT-1-price-sd" = 11,
  
  ### Consumption:  
  "consumption-on?" = "true",
  "consumption_base" = 1000,
  "consumption_frac_cash" = 0.1,
  "consumption_frac_wealth" = 0.05,
  
  ### Heterogeneity and learning:
  "heterogeneous-hhs?" = "true",
  "learning-spillover?" = "false",
  "setup-hh-network" = "\"hh-nw-n-nearest-neighbors\"",
  "hh-nw-param1" = 10,
  "hh-nw-param2" = 50,
  "spillover-share" = 1,
  
  ### Landmarket:
  "h_debt_years_max_bankrupt" = 5,
  "landmarket?" = "true",
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
  "allow-fallow?" = "false",
  
  ### Output:
  "go-once-profiler?" = "false",
  "SHOW-OUTPUT?" = "false",
  "write-maps?" = "false",
  "write-hh-data-to-file?" = "false",
  "export-view?" = "false",
  "show-homebases?" = "true",
  "show-roads?" = "true"
  
)

###
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
                            metrics=c("lut0.carbon", "lut1.carbon",
                                      "lut0.price", "lut1.price",
                                      "lut0.fraction", "lut1.fraction",
                                      "lut0.yield.sum", "lut1.yield.sum",
                                      "lut0.yield.mean", "lut1.yield.mean",
                                      "hh.count", "hh.count.immigrant",
                                      "hh.area.sum", "hh.area.mean",
                                      "hh.consumption.sum", "hh.consumption.mean",
                                      "p.sar", 
                                      "p.tinput.sum", "p.tinput.mean",
                                      "p.capitalstock.sum", "p.capitalstock.mean"),
                            variables = variables,
                            constants = constants)

nl@simdesign <- simdesign_distinct(nl = nl, nseeds = 1)

## Run simulations:
library(future)
plan(multisession)
results <- run_nl_all(nl, split=6)

## Attach output:
setsim(nl, "simoutput") <- results
saveRDS(nl, file = file.path(outpath, "sims_wages_smoothing.rds"))



###################################################
#### ANALYSIS:

## Load packages:
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(ggsci)
library(patchwork)
library(alphahull)

## Load data:
nl <- readRDS(file.path(outpath, "sims_wages_smoothing.rds"))
results <- nl@simdesign@simoutput

# Remove step 0 and rename some columns:
results.lag <- results %>% 
  dplyr::filter(`[step]` > 0) %>% 
  dplyr::rename(step = `[step]`,
                labor.op = `LUT-0-folder`,
                labor.rm = `LUT-1-folder`,
                price.scenario = price_scenario,
                random.seed = `random-seed`) %>% 
  dplyr::mutate(carb.total = lut0.carbon + lut1.carbon) %>% 
  dplyr::mutate(labor.op = dplyr::case_when(labor.op == "oilpalm_labor_min" ~ "low",
                                            labor.op == "oilpalm_labor_med" ~ "med",
                                            labor.op == "oilpalm_labor_high" ~ "high")) %>% 
  dplyr::mutate(labor.rm = dplyr::case_when(labor.rm == "rubber_labor_min" ~ "low",
                                            labor.rm == "rubber_labor_med" ~ "med",
                                            labor.rm == "rubber_labor_high" ~ "high")) %>% 
  dplyr::mutate(price.scenario = dplyr::case_when(historical_smoothing == 0 ~ "dynamic",
                                                  historical_smoothing == 3 ~ "dynamic_smooth")) %>% 
  ### CALCULATING PERCENTAGE CHANGES AND TRADEOFFS:
  group_by(labor.op, labor.rm, price.scenario, random.seed) %>% 
  mutate(carb.change = ((carb.total - lag(carb.total)) / lag(carb.total)) * 100,
         cons.change = ((hh.consumption.mean - lag(hh.consumption.mean)) / lag(hh.consumption.mean)) * 100,
         sar.change = ((p.sar - lag(p.sar)) / lag(p.sar)) * 100) %>% 
  mutate(tradeoff = case_when(carb.change > 0 & cons.change > 0 & sar.change > 0 ~ "total synergy",
                              carb.change <= 0 & cons.change <= 0 & sar.change <= 0 ~ "total loss",
                              carb.change > 0 & cons.change <= 0 & sar.change <= 0 ~ "carbon gain",
                              carb.change <= 0 & cons.change > 0 & sar.change <= 0 ~ "consumption gain",
                              carb.change <= 0 & cons.change <= 0 & sar.change > 0 ~ "biodiversity gain",
                              carb.change > 0 & cons.change > 0 & sar.change <= 0 ~ "carbon and consumption gain",
                              carb.change > 0 & cons.change <= 0 & sar.change > 0 ~ "carbon and biodiversity gain",
                              carb.change <= 0 & cons.change > 0 & sar.change > 0 ~ "consumption and biodiversity gain")) %>% 
  rowwise() %>% 
  mutate(tradeoff = mean(c(carb.change, cons.change, sar.change))) %>% 
  mutate(labor.op = factor(labor.op, levels=c("low", "med", "high")))



## Color palette for labor levels:
#col.pal <- c("low" =  "#244764", "med" = "#E7B800", "high" = "#FC4E07")
col.pal <- c("low" = "#9367BB", "med" = "#3EC1CE", "high" = "#BBBB33")

## Extract data:
results.lag.i <- results.lag %>%
  dplyr::filter(price.scenario == "dynamic")

### Limits for CARBON PLOT:
## x:
max(results.lag$cons.change, na.rm = TRUE)
min(results.lag$cons.change, na.rm = TRUE)
## y, carbon:
max(results.lag$carb.change, na.rm = TRUE)
min(results.lag$carb.change, na.rm = TRUE)
## y, sar:
max(results.lag$sar.change, na.rm = TRUE)
min(results.lag$sar.change, na.rm = TRUE)

## set x limits:
xmax <- 55
xmin <- -55
##set y limits:
ymax.carb <-  15
ymin.carb <- -15
ymax.sar <-  0.2
ymin.sar <- -0.2


## Update y limits:
ymax <- ymax.carb
ymin <- ymin.carb

### Main plot carbon, constant:  
p_constant_carbon_main <- ggplot(results.lag.i) + 
  geom_point(aes(x = cons.change, y = carb.change, color = labor.op), size = 1) + 
  stat_chull(aes(x=cons.change, y=carb.change, color = labor.op, fill = labor.op, lty=labor.op), 
             alpha = 0.05, geom = "polygon") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(xmin, xmax), expand = c(0, 0)) +
  scale_y_continuous(limits = c(ymin, ymax), position = "right", expand = c(0, 0)) +
  xlab("mean consumption change [%]") +
  ylab("") +
  guides(color=guide_legend(title="labor costs"), fill=guide_legend(title="labor costs"), lty=guide_legend(title="labor costs")) +
  scale_color_manual(values = col.pal) +
  scale_fill_manual(values = col.pal) +
  theme_light(base_size = 11) +
  theme(legend.position = c(0.2,0.8),
        plot.margin=grid::unit(c(0,0,0,0),"cm"))


p_constant_carbon_dens_x <- ggplot(results.lag.i, aes(x = cons.change, fill = labor.op, lty=labor.op)) + 
  geom_density(alpha=0.2) + 
  scale_x_continuous(limits = c(xmin, xmax), expand = c(0, 0)) +
  geom_vline(xintercept = 0) +
  theme_void() + 
  scale_fill_manual(values = col.pal) +
  ggtitle("Dynamic prices") +
  theme(legend.position = "none")

p_constant_carbon_dens_y <- ggplot(results.lag.i, aes(x = carb.change, fill = labor.op, lty=labor.op)) + 
  geom_density(alpha=0.2, aes(y = -(..density..))) + 
  scale_x_continuous(limits = c(ymin, ymax), expand = c(0, 0)) +
  geom_vline(xintercept = 0) +
  theme_void() + 
  scale_fill_manual(values = col.pal) +
  theme(legend.position = "none") + 
  coord_flip()


#  ggsave(paste0("labor_tradeoff_carbon_", scenario.i, ".png"), path = "03_Analyses/03_invest_biodiv/", width=6, height=6)

## Update y limits:
ymax <- ymax.sar
ymin <- ymin.sar

p_constant_biodiv_main <- ggplot(results.lag.i) + 
  geom_point(aes(x = cons.change, y = sar.change, color = labor.op), size = 1) + 
  stat_chull(aes(x=cons.change, y=sar.change, color = labor.op, fill = labor.op, lty=labor.op), 
             alpha = 0.05, geom = "polygon") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(xmin, xmax), position = "top", expand = c(0, 0)) +
  scale_y_continuous(limits = c(ymin, ymax), position = "right", expand = c(0, 0)) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = col.pal) +
  scale_fill_manual(values = col.pal) +
  theme_light(base_size = 11) +
  theme(legend.position = "none",
        plot.margin=grid::unit(c(0,0,0,0),"cm"))

p_constant_biodiv_dens_x <- ggplot(results.lag.i, aes(x = cons.change, lty=labor.op, fill = labor.op)) + 
  geom_density(alpha = 0.2, aes(y = -(..density..))) + 
  scale_x_continuous(limits = c(xmin, xmax), expand = c(0, 0)) +
  geom_vline(xintercept = 0) +
  theme_void() + 
  scale_fill_manual(values = col.pal) +
  guides(color=guide_legend(title="labor costs"), fill=guide_legend(title="labor costs"), lty=guide_legend(title="labor costs")) +
  theme(legend.position = "none")

p_constant_biodiv_dens_y <- ggplot(results.lag.i, aes(x = sar.change, fill = labor.op, lty=labor.op)) + 
  geom_density(alpha = 0.2, aes(y = -(..density..))) + 
  scale_x_continuous(limits = c(ymin, ymax), expand = c(0, 0)) + 
  geom_vline(xintercept = 0) +
  theme_void() + 
  scale_fill_manual(values = col.pal) +
  theme(legend.position = "none") + 
  coord_flip()

## Extract data:
results.lag.i <- results.lag %>%
  dplyr::filter(price.scenario == "dynamic_smooth")

## Update y limits:
ymax <- ymax.carb
ymin <- ymin.carb

### Main plot carbon, dynamic:  
p_dynamic_carbon_main <- ggplot(results.lag.i) + 
  geom_point(aes(x = cons.change, y = carb.change, color = labor.op), size = 1) + 
  stat_chull(aes(x=cons.change, y=carb.change, color = labor.op, fill = labor.op, lty=labor.op), 
             alpha = 0.05, geom = "polygon") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(xmin, xmax), expand = c(0, 0)) +
  scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0)) +
  xlab("mean consumption change [%]") +
  ylab("carbon stock change [%]") +
  scale_color_manual(values = col.pal) +
  scale_fill_manual(values = col.pal) +
  theme_light(base_size = 11) +
  theme(legend.position = "none",
        plot.margin=grid::unit(c(0,0,0,0),"cm"))


p_dynamic_carbon_dens_x <- ggplot(results.lag.i, aes(x = cons.change, fill = labor.op, lty=labor.op)) + 
  geom_density(alpha = 0.2) + 
  scale_x_continuous(limits = c(xmin, xmax), expand = c(0, 0)) +
  geom_vline(xintercept = 0) +
  theme_void() + 
  scale_fill_manual(values = col.pal) +
  ggtitle("Dynamic prices smoothed") +
  guides(color=guide_legend(title="labor costs"), fill=guide_legend(title="labor costs"), lty=guide_legend(title="labor costs")) +
  theme(legend.position = "none")

p_dynamic_carbon_dens_y <- ggplot(results.lag.i, aes(x = carb.change, fill = labor.op, lty=labor.op)) + 
  geom_density(alpha = 0.2) + 
  scale_x_continuous(limits = c(ymin, ymax), expand = c(0, 0)) +
  geom_vline(xintercept = 0) +
  theme_void() + 
  scale_fill_manual(values = col.pal) +
  theme(legend.position = "none") + 
  coord_flip()

## Update y limits:
ymax <- ymax.sar
ymin <- ymin.sar

p_dynamic_biodiv_main <- ggplot(results.lag.i) + 
  geom_point(aes(x = cons.change, y = sar.change, color = labor.op), size = 1) + 
  stat_chull(aes(x=cons.change, y=sar.change, color = labor.op, fill = labor.op, lty=labor.op), 
             alpha = 0.05, geom = "polygon") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_x_continuous(limits = c(xmin, xmax), position = "top", expand = c(0, 0)) +
  scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0)) +
  xlab("") +
  ylab("sar ratio change [%]") +
  scale_color_manual(values = col.pal) +
  scale_fill_manual(values = col.pal) +
  theme_light(base_size = 11) +
  theme(legend.position = "none",
        plot.margin=grid::unit(c(0,0,0,0),"cm"))

p_dynamic_biodiv_dens_x <- ggplot(results.lag.i, aes(x = cons.change, lty=labor.op, fill = labor.op)) + 
  geom_density(alpha = 0.2, aes(y = -(..density..))) + 
  scale_x_continuous(limits = c(xmin, xmax), expand = c(0, 0)) +
  geom_vline(xintercept = 0) +
  theme_void() + 
  scale_fill_manual(values = col.pal) +
  guides(color=guide_legend(title="labor costs"), fill=guide_legend(title="labor costs"), lty=guide_legend(title="labor costs")) +
  theme(legend.position = "none")

p_dynamic_biodiv_dens_y <- ggplot(results.lag.i, aes(x = sar.change, fill = labor.op, lty=labor.op)) + 
  geom_density(alpha = 0.2) + 
  scale_x_continuous(limits = c(ymin, ymax), expand = c(0, 0)) +
  geom_vline(xintercept = 0) +
  theme_void() + 
  scale_fill_manual(values = col.pal) +
  theme(legend.position = "none") + 
  coord_flip()


### COMBINE PLOT:
plot_spacer() + p_constant_carbon_dens_x +  p_dynamic_carbon_dens_x + plot_spacer() +
  p_constant_carbon_dens_y +  p_constant_carbon_main + p_dynamic_carbon_main + p_dynamic_carbon_dens_y +
  p_constant_biodiv_dens_y + p_constant_biodiv_main + p_dynamic_biodiv_main + p_dynamic_biodiv_dens_y +
  plot_spacer() + p_constant_biodiv_dens_x +  p_dynamic_biodiv_dens_x + plot_spacer() +
  plot_layout(ncol = 4, nrow = 4, widths = c(1, 4, 4, 1), heights = c(1, 4, 4, 1))

ggsave(paste0("tradeoff_combined_new.png"), path = "03_Analyses/03_invest_biodiv/", width=10, height=10)


#######################################################################################################
#######################################################################################################
### ADDITIONAL PLOTS:

time.plot <- function(data, varname)
{
  data %>% 
    ggplot(aes(x=step, y=var.mu, color=labor.op)) +
    facet_wrap(~price.scenario) +
    geom_line(size=1) +
    geom_ribbon(aes(ymin = var.mu - var.sd, ymax= var.mu + var.sd, fill=labor.op), alpha=0.2, color=NA) +
    scale_color_manual(values = col.pal) +
    scale_fill_manual(values = col.pal) +
    guides(color="none", fill="none") +
    ylab(varname) +
    theme_light(base_size = 11)
}

## Technical inputs:
plot.tinput.mean <- results.lag %>% 
  group_by(labor.op, labor.rm, price.scenario, step) %>% 
  summarize(var.mu = mean(p.tinput.mean), var.sd = sd(p.tinput.mean)) %>% 
  time.plot(., varname="tinput")

## Hh area:
plot.hharea.mean <- results.lag %>% 
  group_by(labor.op, labor.rm, price.scenario, step) %>% 
  summarize(var.mu = mean(hh.area.mean), var.sd = sd(hh.area.mean)) %>% 
  time.plot(., varname="hh.area")

## Yield.op
plot.yield.op.mean <- results.lag %>% 
  group_by(labor.op, labor.rm, price.scenario, step) %>% 
  summarize(var.mu = mean(lut0.yield.mean), var.sd = sd(lut0.yield.mean)) %>% 
  time.plot(., varname ="yield.op")

## Yield.rm
plot.yield.rm.mean <- results.lag %>% 
  group_by(labor.op, labor.rm, price.scenario, step) %>% 
  summarize(var.mu = mean(lut1.yield.mean), var.sd = sd(lut1.yield.mean)) %>% 
  time.plot(., varname="yield.rm")

## Yield.total
plot.yield.total.mean <- results.lag %>% 
  group_by(labor.op, labor.rm, price.scenario, step) %>% 
  summarize(var.mu = mean(lut0.yield.mean + lut1.yield.mean), var.sd = sd(lut0.yield.mean + lut1.yield.mean)) %>% 
  time.plot(., varname="yield.total")

## Capitalstock
plot.capitalstock.mean <- results.lag %>% 
  group_by(labor.op, labor.rm, price.scenario, step) %>% 
  summarize(var.mu = mean(p.capitalstock.mean), var.sd = sd(p.capitalstock.mean)) %>% 
  time.plot(., varname="capitalstock")

### COMBINE PLOT:
plot.tinput.mean + plot.hharea.mean + plot.capitalstock.mean +
  plot.yield.op.mean + plot.yield.rm.mean + plot.yield.total.mean +
  plot_layout(ncol = 3, nrow = 2)  

ggsave(paste0("time_plots.png"), path = "03_Analyses/03_invest_biodiv/", width=16, height=6)
