




# Another idea:
# Which road do we take? A narrative approach
#
# Simulating price trajectories as input
# Cluster results (George Perry approach)


op.gbm <- prices.GBM.one.series(years=50, id=1) %>% 
  dplyr::filter(crop=="oilpalm") %>% 
  dplyr::select(price) %>% 
  dplyr::mutate(source="gbm")

op.wb <- tibble(price=prices.wb$oilpalm) %>% 
  dplyr::mutate(source="wb")

op.total <- op.wb %>% bind_rows(op.gbm) %>% 
  dplyr::mutate(x=dplyr::row_number())

ggplot(op.total, aes(x=x, y=price, color=source)) +
  geom_point()







#####################################
## Load libraries:
library(Refforts)
library(tidyverse)
library(raster)
library(landscapemetrics)
library(clustermq)
library(nlrx)

set.seed(6835624)

## Load prices from historic data from NetLogo parameter folders:
prices <- tibble(year=1:52,
                 rubber=as.numeric(strsplit("936.2777856 868.6767338 885.5715161 797.7484154 654.2350013 646.4374475 806.4138059 614.6540206 477.3383328 436.5650795 769.280586 699.7738209 470.2251997 640.7769637 624.1525409 649.7782415 745.8799753 765.3942667 602.6007523 474.4264919 604.6883223 556.4034636 445.4618454 411.7198385 458.67955 518.3415747 426.8280796 366.5359797 353.2880792 361.8482403 337.4771345 471.1947481 602.3906065 541.6913328 415.0078713 307.7394107 273.2871506 293.8815267 263.1991193 354.1826328 476.5637424 527.7216356 594.5683233 810.0952362 831.0592075 881.364227 697.7814902 1280.478609 1551.532108 1100 923.3988399 647.6232218", " ")[[1]]),
                 oilpalm=as.numeric(strsplit("110.4265186 117.2560383 132.1780035 110.2258817 103.6134211 78.97439936 80.41511905 108.5456568 103.5659118 79.04707803 118.4444829 172.2406675 100.6414827 93.09292488 112.279975 109.4334153 106.8430507 86.71566971 84.68391343 68.07561194 78.78228157 117.0887798 81.26734749 36.27539499 44.12632553 52.87703564 42.63976658 33.96151143 40.10286268 45.69597558 42.40370398 61.15327854 66.22869382 57.05514457 61.55720572 79.13746494 52.4229838 37.78704493 36.14812655 49.96542135 53.94628324 53.71553556 46.63382959 51.54510381 79.22715037 89.38074506 68.59332381 87.29077124 100.102921 90 78.2906484 75.17813221", " ")[[1]]),
)

# Then, we derive the absolute minimum and maximum - These willl be used as extreme cases for our boom and shock scenarios
factor.min <- 0.1
factor.max <- 10
op.min.total <- round(min(prices$oilpalm) * factor.min)
rb.min.total <- round(min(prices$rubber) * factor.min)
op.max.total <- round(max(prices$oilpalm) * factor.max)
rb.max.total <- round(max(prices$rubber) * factor.max)

## Generate price series:
n.series <- 100

path.series <- purrr::map_dfr(1:n.series, function(x){
  
  valid.series <- FALSE
  while(isFALSE(valid.series)) {
    series <- prices.GBM.one.series(years=50, id=x)
    rb.min <- min(dplyr::filter(series, crop=="rubber")$price)
    rb.max <- max(dplyr::filter(series, crop=="rubber")$price)
    op.min <- min(dplyr::filter(series, crop=="oilpalm")$price)
    op.max <- max(dplyr::filter(series, crop=="oilpalm")$price)
    if(rb.min >= rb.min.total &
       rb.max <= rb.max.total &
       op.min >= op.min.total &
       op.max <= op.max.total) {
      valid.series <- TRUE
    }
  }
  return(series)
})

# get list of cluster objects for each crop:
clusterlist <- prices.GBM.cluster(path.series, k=5)
# add cluster ids to series data:
series.cl <- prices.GBM.cluster.series(path.series, clusterlist)

# plot cluster:
library(ggplot2)
ggplot(series.cl, aes(x=time, y=price, color=factor(series))) +
  facet_grid(crop~cluster, scales="free") +
  geom_line() +
  guides(color="none") +
  scale_y_log10() +
  theme_minimal()

## Randomly pick one per cluster:
series.pick <- series.cl %>% 
  group_by(crop, cluster,series) %>% 
  dplyr::summarise(series=mean(series)) %>% 
  dplyr::sample_n(1)

series.selected <- series.cl %>% 
  dplyr::filter(paste(crop,cluster,series) %in% paste(series.pick$crop,series.pick$cluster,series.pick$series))

## Plot final series:
library(ggplot2)
ggplot(series.selected, aes(x=time, y=price, color=factor(series))) +
  facet_grid(crop~cluster, scales="free") +
  geom_line() +
  guides(color="none") +
  scale_y_log10() +
  theme_minimal()

