library(ggplot2)
library(raster)
#library(RColorBrewer)

map <- raster(paste("/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/", experiment, "/output/quality_c_", experiment, ".tif" ,sep=""))
plot(map)
map_df <- as.data.frame(map, xy=TRUE)
head(map_df)
#preparing raster object to plot with geom_raster in ggplot2
#points = rasterToPoints(map)
#points_df = data.frame(points)
#head(points_df) #breaks will be set to column "with values"
#points_df$cuts=cut(points_df$lulc,breaks=c(0, 1, 2, 3, 4)) #set breaks

mycol_lulc <- '#018571'
mycol_quality <- "#00441b"
mycol_impact <- "#fee391"     #b2182b

ggplot(data=map_df) + 
  geom_raster(aes(x=x,y=y,fill=factor(quality_c_forest))) + 
  landscapetools::theme_nlm_discrete(
    legend_title = "Habitat-Quality-Score",
    axis_text_size = 4,
    plot_margin = ggplot2::unit(c(1,1,1,1), "lines")) +
  scale_fill_manual(values = mycol_quality, name="Impact-Score") +
  xlab("Longitude (X)") + ylab("Latitude (Y)") #+ ggtitle("LULC map")

ggsave("quality_tif_forest.png", path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/Plots/" )#, plot="plot1",device = png) #, path = "/home/dockerj/EFForTS-ABM/01_EFForTS-ABM/tests_integration/01_unittest/Plots/")
