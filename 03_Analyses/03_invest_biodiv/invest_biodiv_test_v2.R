


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
  "price_scenario" = list(values=c(rep("\"constant_prices\"", 3), rep("\"historical_trends\"", 3)))
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
                            metrics=c("item 1 item 0 carbon", "item 1 item 1 carbon",
                                      "item 0 prices", "item 1 prices",
                                      "item 0 LUT-fractions", "item 1 LUT-fractions",
                                      "count hhs", "count hhs with [h_immigrant? = TRUE]",
                                      "precision mean [h_area] of hhs 3",
                                      "mean_hh_consumption",
                                      "mean [p_tinput] of patches",
                                      "sar_ratio"),
                            variables = variables,
                            constants = constants)

nl@simdesign <- simdesign_distinct(nl = nl, nseeds = 3)

## Run simulations:
library(future)
plan(multisession)
results <- run_nl_all(nl)

## Attach output:
setsim(nl, "simoutput") <- results
saveRDS(nl, file = file.path(outpath, "sims_wages.rds"))



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
nl <- readRDS(file.path(outpath, "sims_wages.rds"))
results <- nl@simdesign@simoutput

# Remove step 0 and rename some columns:
results.lag <- results %>% 
  dplyr::filter(`[step]` > 0) %>% 
  dplyr::rename(step = `[step]`,
                cons = `mean_hh_consumption`,
                carb.op = `item 1 item 0 carbon`,
                carb.rm = `item 1 item 1 carbon`,
                sar = sar_ratio,
                tinput = `mean [p_tinput] of patches`,
                hharea = `precision mean [h_area] of hhs 3`,
                labor.op = `LUT-0-folder`,
                labor.rm = `LUT-1-folder`,
                price.scenario = price_scenario) %>% 
  dplyr::mutate(carb.total = carb.op + carb.rm) %>% 
  dplyr::mutate(labor.op = dplyr::case_when(labor.op == "oilpalm_labor_min" ~ "low",
                                            labor.op == "oilpalm_labor_med" ~ "med",
                                            labor.op == "oilpalm_labor_high" ~ "high")) %>% 
  dplyr::mutate(labor.rm = dplyr::case_when(labor.rm == "rubber_labor_min" ~ "low",
                                            labor.rm == "rubber_labor_med" ~ "med",
                                            labor.rm == "rubber_labor_high" ~ "high")) %>% 
  dplyr::mutate(price.scenario = dplyr::case_when(price.scenario == "constant_prices" ~ "constant",
                                                  price.scenario == "historical_trends" ~ "dynamic")) %>% 
  ### AGGREGATING ACROSS SEEDS:
  group_by(labor.op, labor.rm, price.scenario, step) %>% 
  summarize(cons = mean(cons), cons.sd = sd(cons),
            carb.total = mean(carb.total), carb.sd = sd(carb.total),
            sar = mean(sar), sar.sd = sd(sar),
            tinput = mean(tinput),
            hharea = mean(hharea)) %>% 
  ungroup() %>% 
  ### CALCULATING PERCENTAGE CHANGES AND TRADEOFFS:
  group_by(labor.op, labor.rm, price.scenario) %>% 
  mutate(carb.change = ((carb.total - lag(carb.total)) / lag(carb.total)) * 100,
         cons.change = ((cons - lag(cons)) / lag(cons)) * 100,
         sar.change = ((sar - lag(sar)) / lag(sar)) * 100) %>% 
  mutate(tradeoff = case_when(carb.change > 0 & cons.change > 0 & sar.change > 0 ~ "total synergy",
                              carb.change <= 0 & cons.change <= 0 & sar.change <= 0 ~ "total loss",
                              carb.change > 0 & cons.change <= 0 & sar.change <= 0 ~ "carbon gain",
                              carb.change <= 0 & cons.change > 0 & sar.change <= 0 ~ "consumption gain",
                              carb.change <= 0 & cons.change <= 0 & sar.change > 0 ~ "biodiversity gain",
                              carb.change > 0 & cons.change > 0 & sar.change <= 0 ~ "carbon and consumption gain",
                              carb.change > 0 & cons.change <= 0 & sar.change > 0 ~ "carbon and biodiversity gain",
                              carb.change <= 0 & cons.change > 0 & sar.change > 0 ~ "consumption and biodiversity gain")) %>% 
  rowwise() %>% 
  mutate(tradeoff = mean(c(carb.change, cons.change, sar.change)))
 
  

## Create a plot for each price scenario:
scenarios <- unique(results.lag$price.scenario)

for (i in 1:length(scenarios))
{
  scenario.i <- scenarios[i]
  
  ## Extract data:
  results.lag.i <- results.lag %>%
    dplyr::filter(price.scenario == scenario.i)
  
  ## Color palette for labor levels:
  col.pal <- c("low" =  "#244764",
               "med" = "#E7B800", 
               "high" = "#FC4E07")
  
  ### CARBON PLOT:
  xmax <- 40
  xmin <- -40
  ymax <-  6
  ymin <- -6
  
  
  plot1 <- ggplot(results.lag.i) + 
    geom_point(aes(x = cons.change, y = carb.change, color = labor.op), size = 1) + 
    stat_chull(aes(x=cons.change, y=carb.change, color = labor.op, fill = labor.op, lty=labor.op), 
               alpha = 0.05, geom = "polygon") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_x_continuous(limits = c(xmin, xmax)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    scale_color_manual(values = col.pal) +
    scale_fill_manual(values = col.pal) +
    theme_light(base_size = 11) +
    theme(legend.position = "none")
          
  
  dens1 <- ggplot(results.lag.i, aes(x = cons.change, fill = labor.op, lty=labor.op)) + 
    geom_density(alpha = 0.2) + 
    scale_x_continuous(limits = c(xmin, xmax)) +
    theme_void() + 
    scale_fill_manual(values = col.pal) +
    guides(color=guide_legend(title="labor costs"), fill=guide_legend(title="labor costs"), lty=guide_legend(title="labor costs")) +
    theme(legend.position = c(0.1, 0.5))
  
  dens2 <- ggplot(results.lag.i, aes(x = carb.change, fill = labor.op, lty=labor.op)) + 
    geom_density(alpha = 0.2) + 
    scale_x_continuous(limits = c(ymin, ymax)) +
    theme_void() + 
    scale_fill_manual(values = col.pal) +
    theme(legend.position = "none") + 
    coord_flip()
  
  dens1 + plot_spacer() + plot1 + dens2 + 
    plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
  
  ggsave(paste0("labor_tradeoff_carbon_", scenario.i, ".png"), path = "03_Analyses/03_invest_biodiv/", width=6, height=6)
  
  ### BIODIV PLOT:
  xmax <- 40 #max(abs(results.lag.i$cons.change))
  xmin <- -40 #xmax * -1
  ymax <-  0.1 #max(abs(results.lag.i$sar.change))
  ymin <- -0.1 #ymax * -1
  
  plot1 <- ggplot(results.lag.i) + 
    geom_point(aes(x = cons.change, y = sar.change, color = labor.op), size = 1) + 
    stat_chull(aes(x=cons.change, y=sar.change, color = labor.op, fill = labor.op, lty=labor.op), 
               alpha = 0.05, geom = "polygon") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_x_continuous(limits = c(xmin, xmax)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    scale_color_manual(values = col.pal) +
    scale_fill_manual(values = col.pal) +
    theme_light(base_size = 11) +
    theme(legend.position = "none")
  
  dens1 <- ggplot(results.lag.i, aes(x = cons.change, lty=labor.op, fill = labor.op)) + 
    geom_density(alpha = 0.2) + 
    scale_x_continuous(limits = c(xmin, xmax)) +
    theme_void() + 
    scale_fill_manual(values = col.pal) +
    guides(color=guide_legend(title="labor costs"), fill=guide_legend(title="labor costs"), lty=guide_legend(title="labor costs")) +
    theme(legend.position = c(0.1, 0.5))
  
  dens2 <- ggplot(results.lag.i, aes(x = sar.change, fill = labor.op, lty=labor.op)) + 
    geom_density(alpha = 0.2) + 
    scale_x_continuous(limits = c(ymin, ymax)) +
    theme_void() + 
    scale_fill_manual(values = col.pal) +
    theme(legend.position = "none") + 
    coord_flip()
  
  dens1 + plot_spacer() + plot1 + dens2 + 
    plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
  
  ggsave(paste0("labor_tradeoff_biodiv_", scenario.i, ".png"), path = "03_Analyses/03_invest_biodiv/", width=6, height=6)
}


col.pal <- c("low" =  "#244764",
             "med" = "#E7B800", 
             "high" = "#FC4E07")
results.lag %>% 
  gather(Variable, value, tinput, hharea) %>% 
  ggplot(aes(x=step, y=value, color=labor.op, lty=Variable)) +
  facet_wrap(~price.scenario) +
  geom_line(size=1) +
  scale_color_manual(values = col.pal) +
  theme_light(base_size = 11)

ggsave(paste0("tinput_area.png"), path = "03_Analyses/03_invest_biodiv/", width=10, height=4)
