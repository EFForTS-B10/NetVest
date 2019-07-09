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
                                      "mean_hh_consumption"),
                            metrics.turtles = list("hhs"=c("pxcor", "pycor", "h_age", "h_wealth", "h_netcashflow", "h_debts")),
                            metrics.patches = c("pxcor", "pycor", "p_age", "p_landuse", "p_capitalstock", "p_carbon", "p_actual_production", "p_owner"),
                            variables = list('landmarket?' = list(values=c("true", "false")),
                                             'heterogeneous-hhs?' = list(values=c("true", "false"))),
                            constants = list('learning-spillover?' = "false",
                                             'reproducable?' = "true",
                                             'rnd-seed' = 74323,
                                             'which-map' = "\"hundred-farmers3\"",
                                             'historical_smoothing' = 0))

nl@simdesign <- simdesign_ff(nl = nl, nseeds = 1)

## Run simulations:
library(future)
plan(multisession)
results <- run_nl_all(nl, split=nrow(nl@simdesign@siminput))

## Attach output:
setsim(nl, "simoutput") <- results
saveRDS(nl, file = file.path(outpath, "sims.rds"))
nl <- readRDS(file.path(outpath, "sims.rds"))

## Convert to raster:
library(raster)
nl_r <- nl_to_raster(nl)

# Create a new column with only the p_landuse rasters:
library(purrr)
nl_r$r_p_age <- map(seq(nrow(nl_r)), function(x) {raster::subset(nl_r$spatial.raster[x][[1]], 1)})
nl_r$r_p_landuse <- map(seq(nrow(nl_r)), function(x) {raster::subset(nl_r$spatial.raster[x][[1]], 2)})
nl_r$r_p_capitalstock <- map(seq(nrow(nl_r)), function(x) {raster::subset(nl_r$spatial.raster[x][[1]], 3)})
nl_r$r_p_carbon <- map(seq(nrow(nl_r)), function(x) {raster::subset(nl_r$spatial.raster[x][[1]], 4)})


## Plot p_landuse raster:
library(ggplot2)
plottick <- 51
raster::as.data.frame(nl_r[plottick,]$r_p_landuse[[1]], xy = TRUE) %>% 
  dplyr::na_if(-100) %>% 
  ggplot() + 
  geom_raster(aes(x, y, fill=factor(p_landuse))) +
  landscapetools::theme_nlm_discrete()


## Plot p_age raster:
library(ggplot2)
plottick <- 50
raster::as.data.frame(nl_r[plottick,]$r_p_age[[1]], xy = TRUE) %>% 
  dplyr::na_if(-100) %>% 
  ggplot() + 
  geom_raster(aes(x, y, fill=factor(p_age))) +
  landscapetools::theme_nlm_discrete()


## Plot p_age raster:
library(ggplot2)
plottick <- 50
raster::as.data.frame(nl_r[plottick,]$r_p_capitalstock[[1]], xy = TRUE) %>% 
  dplyr::na_if(-100) %>% 
  ggplot() + 
  geom_raster(aes(x, y, fill=factor(p_capitalstock))) +
  landscapetools::theme_nlm_discrete()




# Calculate LSM
library(landscapemetrics)
nl_r$lsm_l_cai_cv <- calculate_lsm(nl_r$r_p_landuse, what = c("lsm_l_cai_cv"))$value
nl_r$lsm_l_cohesion <- calculate_lsm(nl_r$r_p_landuse, what = c("lsm_l_cohesion"))$value
nl_r$lsm_l_ed <- calculate_lsm(nl_r$r_p_landuse, what = c("lsm_l_ed"))$value
nl_r$lsm_l_division <- calculate_lsm(nl_r$r_p_landuse, what = c("lsm_l_division"))$value
nl_r$lsm_l_area_mn <- calculate_lsm(nl_r$r_p_landuse, what = c("lsm_l_area_mn"))$value
nl_r$lsm_l_area_mn <- calculate_lsm(nl_r$r_p_age, what = c("lsm_l_area_mn"))$value


## PLOTTING:
library(ggsci)
library(ggthemes)
ggplot(nl_r, aes(x=`[step]`, y=(lsm_l_cohesion), color=factor(`landmarket?`), lty=factor(`heterogeneous-hhs?`), shape=factor(`heterogeneous-hhs?`))) +
  #geom_smooth(se=FALSE, span=0.4) +
  geom_line() +
  scale_color_jco() +
  theme_tufte()

ggplot(nl_r, aes(x=`[step]`, y=`item 1 LUT-fractions`, color=factor(`landmarket?`), lty=factor(`heterogeneous-hhs?`))) +
  geom_line() +
  scale_color_jco() +
  theme_tufte()


################################################################

