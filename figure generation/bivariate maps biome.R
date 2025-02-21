### Code for bivariate cloropleth maps for drought metrics
## biome level

library(tidyverse)
library(terra)
library(tidyterra)
library(biscale)
library(ggthemes)
library(cowplot)
library(sf)

forest_vect <- vect("processed data/shp/biome_vect.shp")

countries <- vect("data/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
countries <- vect("data/ne_50m_land/ne_50m_land.shp")
countries <- crop(countries, ext(-180, 180, -52, 52)) %>% 
  project("+proj=robin")

biomes <- vect("processed data/shp/biome_vect.shp")

## read in historic drought data
hist <- rast("data/drought/allmetrics_summary_hist_down.tif")[[c(5, 14)]] 
pdf("output/biome_cloropleth_hist.pdf")
for(i in c(1:5, 12, 14)){
  sub <- subset(biomes, biomes$BIOME==i)

  double <- crop(hist, sub, mask=T) %>% 
    project("+proj=robin")
  
  df <- as.data.frame(double, na.rm=T, xy=T)
  head(df)
  df$max_severe_median <- df$max_severe_median*-1
  
  df2 <- bi_class(df, x=dr_freq_median, y=max_severe_median, style="fisher", dim=3)
  interval_vals <- bi_class_breaks(df, style = "fisher",
                                   x = dr_freq_median, y = max_severe_median, dim = 3, dig_lab = c(x = 2, y = 3),
                                   split = FALSE)
  break_vals <- bi_class_breaks(df, style = "fisher",
                                x = dr_freq_median, y = max_severe_median, dim = 3, dig_lab = c(x = 2, y = 3),
                                split = TRUE)
  
  map <- ggplot()+
    geom_spatvector(data=countries, fill="light gray", color="light gray") +
    # coord_sf(crs="+init=epsg:3857") +
    geom_tile(data=df2, aes(x=x, y=y, fill=bi_class), show.legend = FALSE) +
    bi_scale_fill(pal="GrPink", dim=3)+
    bi_theme() +
    theme(axis.title = element_blank()) +
    theme(plot.title = element_text(size=18)) +
    ggtitle(paste0(sub$name, ", Historic Drought"))
  
  legend <- bi_legend(pal = "GrPink",
                      dim = 3,
                      xlab = "Higher Frequency",
                      ylab = "Higher Severity ",
                      breaks=break_vals,
                      size = 7)
  
  
  finalPlot_hist <- ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.05, 0.2, 0.18, 0.18)
  
  print(finalPlot_hist)
}
dev.off()

## rcp 2.6 mid ----
future <- rast("data/drought/allmetrics_summary_rcp26_mid_delta_down.tif")

pdf("output/biome_cloropleth_mid26.pdf")
for(i in c(1:5, 12, 14)){
  sub <- subset(biomes, biomes$BIOME==i)
  
  double <- crop(future[[c(5, 14)]], sub, mask=T) %>% 
    project("+proj=robin")
  
  df <- as.data.frame(double, na.rm=T, xy=T)
  head(df)
  df$delta_max_severe_median <- df$delta_max_severe_median*-1
  head(df)
  
  df2 <- bi_class(df, x=delta_dr_freq_median, y=delta_max_severe_median, style="fisher", dim=3)
  interval_vals <- bi_class_breaks(df, style = "fisher",
                                   x = delta_dr_freq_median, y = delta_max_severe_median, dim = 3, dig_lab = c(x = 2, y = 3),
                                   split = FALSE)
  break_vals <- bi_class_breaks(df, style = "fisher",
                                x = delta_dr_freq_median, y = delta_max_severe_median, dim = 3, dig_lab = c(x = 2, y = 3),
                                split = TRUE)
  
  ## funny code for the negative values
  break_vals$bi_x <- break_vals$bi_x[-1]
  break_vals$bi_y <- break_vals$bi_y[-1]
  
  break_vals$bi_x[1] <- break_vals$bi_x[1]*-1
  break_vals$bi_y[1] <- break_vals$bi_y[1]*-1
  
  
  
  map <- ggplot()+
    geom_spatvector(data=countries, fill="light gray", color="light gray") +
    # coord_sf(crs="+init=epsg:3857") +
    geom_tile(data=df2, aes(x=x, y=y, fill=bi_class), show.legend = FALSE) +
    bi_scale_fill(pal="GrPink", dim=3)+
    bi_theme() +
    theme(axis.title = element_blank()) +
    theme(plot.title = element_text(size=18)) +
    ggtitle(paste0(sub$name,", RCP 2.6 mid-century projection"))
  
  legend <- bi_legend(pal = "GrPink",
                      dim = 3,
                      xlab = "Increasing Frequency",
                      ylab = "Increasing Severity ",
                      breaks=break_vals,
                      size = 7)
  
  
  finalPlot_26 <- ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.05, 0.2, 0.18, 0.18)
  
  print(finalPlot_26)
}
dev.off()

