

#### Analyse wage levels:

library(nlrx)

# Windows default NetLogo installation path (adjust to your needs!):
netlogopath <- file.path("C:/Program Files/NetLogo 6.0.3")
modelpath <- "01_EFForTS-ABM/EFForTS-ABM.nlogo"
outpath <- "03_Analyses/01_Landmarket/output/"

nl <- nl(nlversion = "6.0.3",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)

## Experiment:
nl@experiment <- experiment(expname="landmarkets",
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
                                      "item 0 bird_richness", "item 1 bird_richness", "item 2 bird_richness", "item 3 bird_richness"),
                            #metrics.turtles = list("hhs"=c("pxcor", "pycor", "h_age", "h_area", "h_wealth", "h_capitalstock", "h_netcashflow", "h_debts", "h_consumption", "h_cost_labor", "h_cost_tinput", "h_cost_capital", "h_cost_land", "h_revenue", "item 0 h_inefficiencies", "item 1 h_inefficiencies", "h_immigrant?")),
                            #metrics.patches = c("pxcor", "pycor", "p_id", "p_age", "p_landuse", "p_capitalstock", "p_carbon", "p_actual_production", "p_owner", "p_labor", "p_tinput"),
                            variables = list("LUT-0-folder" = list(values=rep(c("\"oilpalm_labor_min\"", "\"oilpalm_labor_med\"", "\"oilpalm_labor_high\""), 2)),
                                             "LUT-1-folder" = list(values=rep(c("\"rubber_labor_min\"", "\"rubber_labor_med\"", "\"rubber_labor_high\""), 2)),
                                             "price_scenario" = list(values=c(rep("\"constant_prices\"", 3), rep("\"random_walk\"", 3)))),
                            constants = list("go-once-profiler?" = "false",
                                             "SHOW-OUTPUT?" = "false",
                                             "reproducable?" = "true",
                                             "rnd-seed" = 1234, ## not used
                                             "write-maps?" = "false",
                                             "write-hh-data-to-file?" = "false",
                                             "export-view?" = "false",
                                             "show-homebases?" = "true",
                                             "show-roads?" = "true",
                                             "which-map" = "\"landmarkets1\"",
                                             "land-use-change-decision" = "\"only-one-field-per-year\"",
                                             "sim-time" = 50,
                                             "price-fluctuation-percent" = 10, ## not used
                                             "historical_smoothing" = 0, ## not used
                                             "heterogeneous-hhs?" = "true",
                                             "learning-spillover?" = "false",
                                             "setup-hh-network" = "\"hh-nw-n-nearest-neighbors\"",
                                             "hh-nw-param1" = 10,
                                             "hh-nw-param2" = 50,
                                             "spillover-share" = 1, ## not used
                                             "consumption-on?" = "true",
                                             "consumption_base" = 1000,
                                             "consumption_frac_cash" = 0.1,
                                             "consumption_frac_wealth" = 0.05,
                                             "h_debt_years_max_bankrupt" = 5,
                                             "landmarket?" = "true",
                                             "buyer_pool_n" = 10,
                                             "immigrant_probability" = 0.5,
                                             "land_price_increase" = 0.05,
                                             "initial-wealth-distribution" = "\"log-normal\"",
                                             "init-wealth-correction-factor" = 10,
                                             "wealth-log-mean" = 7,
                                             "wealth-log-sd" = 1,
                                             "wealth-constant" = 10000, ## not used
                                             "min-wealth" = 30,
                                             "time-horizon" = 10,
                                             "discount-rate" = 0.1,
                                             "land_price" = 750,
                                             "external_income" = 500,
                                             "rent_rate_capital_lend" = 0.1,
                                             "rent_rate_capital_borrow" = 0.15,
                                             "rent_rate_land" = 0.1,
                                             "hh_age_alpha" = 14.24,
                                             "hh_age_lambda" = 0.31,
                                             "hh_age_min" = 18,
                                             "hh_age_max" = 80,
                                             "age_generation" = 40,
                                             "takeover_prob" = 0.5,
                                             "LUT-0-price" = 190,
                                             "LUT-0-price-mu" = 1.9,
                                             "LUT-0-price-sd" = 1.9,
                                             "LUT-1-price" = 1100,
                                             "LUT-1-price-mu" = 11,
                                             "LUT-1-price-sd" = 11,
                                             "immigrant-xp-bonus" = "\"[0 0]\"",
                                             "immigrant-wealth-factor" = 1,
                                             "allow-fallow?" = "false",
                                             "calc_bird_richness?" = "true"
                                             ))

nl@simdesign <- simdesign_distinct(nl = nl, nseeds = 1)

## Run simulations:
library(future)
plan(multisession)
results <- run_nl_all(nl, split=6)



## Investigate dynamics:

library(ggplot2)

## hh area
ggplot(results, aes(x=`[step]`, y=`precision mean [h_area] of hhs 3`)) +
  facet_grid(`LUT-0-folder`~price_scenario) +
  geom_line()

# hhs
ggplot(results, aes(x=`[step]`, y=`count hhs`, color=`LUT-0-folder`)) +
  geom_line()

# prices
ggplot() +
  geom_line(data=results, aes(x=`[step]`, y= `item 0 prices`)) +
  geom_line(data=results, aes(x=`[step]`, y= `item 1 prices`))

# luts
ggplot() +
  geom_line(data=results, aes(x=`[step]`, y= `item 0 LUT-fractions`), color="red") +
  geom_line(data=results, aes(x=`[step]`, y= `item 1 LUT-fractions`), color="gold") +
  facet_grid(`LUT-0-folder`~price_scenario)

# carbon
ggplot() +
  geom_line(data=results, aes(x=`[step]`, y= `item 1 item 0 carbon`), color="red") +
  geom_line(data=results, aes(x=`[step]`, y= `item 1 item 1 carbon`), color="gold") +
  geom_line(data=results, aes(x=`[step]`, y= `item 1 item 0 carbon` + `item 1 item 1 carbon`), color="black") +
  facet_grid(`LUT-0-folder`~price_scenario)

# consumption
ggplot() +
  geom_line(data=results, aes(x=`[step]`, y= `mean_hh_consumption`)) +
  facet_grid(`LUT-0-folder`~price_scenario)

ggplot() +
  geom_path(data=results, aes(x=`precision mean [h_area] of hhs 3`, y= `mean_hh_consumption`), color="red") +
  geom_point(data=results, aes(x=`precision mean [h_area] of hhs 3`, y= `mean_hh_consumption`, color=`[step]`)) +
  facet_grid(`LUT-0-folder`~price_scenario)




### BIRDS:

ggplot(test, aes(x=`[step]`, y=`item 2 bird_richness`)) +
  geom_line()

