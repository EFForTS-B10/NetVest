

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
                            metrics=c("item 1 item 0 carbon", 
                                      "item 1 item 1 carbon",
                                      "item 0 prices", 
                                      "item 1 prices",
                                      "item 0 LUT-fractions", 
                                      "item 1 LUT-fractions",
                                      "LUT-fractions",
                                      "count hhs",
                                      "count lms",
                                      "count hhs with [h_immigrant? = TRUE]",
                                      "precision mean [h_area] of hhs 3",
                                      "mean_hh_consumption"),
                            metrics.turtles = list("lms" = c("lm_ticks",
                                                             "lm_seller_who",
                                                            "lm_seller_area",
                                                            "lm_seller_wealth",
                                                            "lm_seller_lut0_ineff",
                                                            "lm_seller_lut1_ineff",
                                                            "lm_poolall_wealth",
                                                            "lm_poolall_immigrant",
                                                            "lm_poolpot_wealth",
                                                            "lm_poolpot_immigrant",
                                                            "lm_buyer_who",
                                                            "lm_buyer_area",
                                                            "lm_buyer_wealth",
                                                            "lm_buyer_immigrant",
                                                            "lm_buyer_lut0_ineff",
                                                            "lm_buyer_lut1_ineff")),
                            variables = list('immigrant-xp-bonus' = list(values=c("\"[0.5 -0.5]\"", "\"[0.0 0.0]\""))),
                            constants = list('learning-spillover?' = "false",
                                             'reproducable?' = "true",
                                             'rnd-seed' = 74323,
                                             'which-map' = "\"hundred-farmers3\"",
                                             'historical_smoothing' = 0,
                                             'landmarket?' = "true",
                                             'heterogeneous-hhs?' = "true",
                                             'immigrant-wealth-factor' = 50))

nl@simdesign <- simdesign_distinct(nl = nl, nseeds = 1)

## Run simulations:
#library(future)
#plan(multisession)
results <- run_nl_all(nl)
## Attach and save:
setsim(nl, "simoutput") <- results
saveRDS(nl, file = file.path(outpath, "landmarket_pools.rds"))
nl <- readRDS(nl, file = file.path(outpath, "landmarket_pools.rds"))
results <- nl@simdesign@simoutput
## Unlist:
# Remove rows without lms:
results.lms <- results %>% dplyr::filter(`count lms` > 0)
setsim(nl, "simoutput") <- results.lms
res.spat <- unnest_simoutput(nl) %>% dplyr::filter(`lm_buyer_wealth` != -999)

library(ggplot2)


## Count hhs
ggplot(results) +
  facet_wrap(~`immigrant-xp-bonus`) +
  geom_point(aes(x=`[step]`, y=`count hhs`), color="black") +
  geom_point(aes(x=`[step]`, y=`count hhs with [h_immigrant? = TRUE]`), color="blue") +
  theme_minimal()

## Consumption
ggplot(results, aes(x=`[step]`, y=mean_hh_consumption, group=`[step]`)) +
  facet_wrap(~`immigrant-xp-bonus`) +
  geom_point() +
  theme_minimal()

## LUT fractions
ggplot(results) +
  facet_wrap(~`immigrant-xp-bonus`) +
  geom_line(aes(x=`[step]`, y=`item 0 LUT-fractions`), color="red") +
  geom_line(aes(x=`[step]`, y=`item 1 LUT-fractions`), color="gold") +
  theme_minimal()

## Area of hhs
ggplot(results, aes(x=`[step]`, y=`precision mean [h_area] of hhs 3`, group=`[step]`)) +
  facet_wrap(~`immigrant-xp-bonus`) +
  geom_point() +
  theme_minimal()

## Landmarket area
ggplot(res.spat, aes(x=lm_seller_area, y=lm_buyer_area)) +
  facet_wrap(~`immigrant-xp-bonus`) +
  geom_point(aes(color=lm_ticks, shape=factor(lm_buyer_immigrant))) +
  scale_color_viridis_c() +
  geom_abline(intercept=0, slope=1)

## Landmarket wealth
ggplot(res.spat, aes(x=lm_seller_wealth, y=lm_buyer_wealth)) +
  facet_wrap(~`immigrant-xp-bonus`) +
  geom_point(aes(color=lm_ticks, shape=factor(lm_buyer_immigrant))) +
  scale_color_viridis_c() +
  geom_abline(intercept=0, slope=1)

## Landmarket transactions
ggplot(res.spat, aes(x=factor(lm_buyer_who), y=lm_seller_area, color=factor(lm_buyer_immigrant), fill=lm_ticks)) +
  facet_wrap(~`immigrant-xp-bonus`, scales="free") +
  coord_flip() +
  geom_bar(stat="identity", position = "stack", size=1) +
  scale_fill_viridis_c() +
  theme_minimal(11)


## Inefficiencies
ggplot(res.spat, aes(x=`[step]`)) +
  geom_boxplot(aes(y=((lm_seller_lut0_ineff - lm_buyer_lut0_ineff)), group=`[step]`), color="red", se = FALSE, span=0.3) +
  geom_boxplot(aes(y=((lm_seller_lut1_ineff - lm_buyer_lut1_ineff)), group=`[step]`), color="gold", se=FALSE, span=0.3) +
  ylab("Efficiency gain") +
  geom_hline(yintercept = 0) +
  facet_wrap(~`immigrant-xp-bonus`) +
  #coord_flip() +
  scale_fill_viridis_c() +
  theme_minimal(11)

ggplot(results) +
  facet_wrap(~`immigrant-xp-bonus`) +
  geom_line(aes(x=`[step]`, y=`item 0 prices`), color="red") +
  geom_line(aes(x=`[step]`, y=`item 1 prices`), color="gold") +
  theme_minimal()

### Build an igraph:
library(igraph)
library(ggraph)
library(tidygraph)
res.spat <- unnest_simoutput(nl) %>% dplyr::filter(`lm_buyer_wealth` != -999)
immigrant.ids <- res.spat %>% dplyr::filter(lm_buyer_immigrant == 1) %>% dplyr::select(lm_buyer_who)
res.spat$lm_buyer_immigrant_new <- ifelse(res.spat$lm_buyer_who %in% immigrant.ids$lm_buyer_who, 1, 0)

## Loop over simulations:
res.spat.graph <- purrr::map_dfr(unique(res.spat$`immigrant-xp-bonus`), function(x) {
      
  ## Prepare vertices data frame:
  res.spat.x <- res.spat %>% dplyr::filter(res.spat$`immigrant-xp-bonus` == x) %>% 
    dplyr::select(lm_seller_who, lm_buyer_who, lm_buyer_area, lm_buyer_immigrant_new, lm_seller_lut0_ineff, lm_seller_lut1_ineff, lm_buyer_lut0_ineff, lm_buyer_lut1_ineff)
  
  res.spat.x.igraph <- tibble::tibble(`immigrant-xp-bonus` = x,
                                      graph = list(igraph::graph_from_data_frame(res.spat.x)))
  
  
})


### Create plots:
p.grobs <- list()
for(i in 1:length(res.spat.graph$graph))
{
  test <- res.spat.graph$graph[[i]]
  V(test)$immigrant <- FALSE
  V(test)[as.numeric(name) %in% immigrant.ids$lm_buyer_who]$immigrant <- TRUE
  test <- tidygraph::as_tbl_graph(test)
  
  
  pi <- ggraph(test, layout = 'kk') + 
    geom_edge_link(aes(edge_width=lm_buyer_area), show.legend = FALSE, arrow = arrow(length = unit(4, 'mm'))) + 
    scale_edge_width(range=c(1,4)) +
    geom_node_point(size=4, aes(color=immigrant)) +  
    scale_color_viridis_d() +
    theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
  p.grobs[[i]] <- pi
}

library(gridExtra)
grid.arrange(grobs=p.grobs)

