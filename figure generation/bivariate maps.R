### Code for bivariate cloropleth maps for drought metrics
## full forest extent, historic, mid- and late- century RCP 2.6 and 8.5

library(tidyverse)
library(terra)
library(tidyterra)
library(biscale)
library(ggthemes)
library(cowplot)
library(sf)

forest_vect <- vect("processed data/shp/biome_vect.shp")
agb <- rast("processed data/rast_agb.tif")
height <- rast("processed data/rast_height.tif")

countries <- vect("data/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
countries <- vect("data/ne_50m_land/ne_50m_land.shp")
countries <- crop(countries, ext(-180, 180, -52, 52)) %>% 
  project("+proj=robin")

biomes <- vect("processed data/shp/biome_vect.shp")
e <- ext(-180, 180, -52, 52)
realmvect <- vect("processed data/shp/realm_vect_full.shp") %>% 
  crop(e) %>% 
  project("+proj=robin")


## read in historic drought data
hist <- rast("data/drought/allmetrics_summary_hist_down.tif")[[c(7, 22)]] 

double <- crop(hist, forest_vect, mask=T) %>% 
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
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2, aes(x=x, y=y, fill=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink", dim=3)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  ggtitle("Historic Drought")

map2 <- map + 
  new_scale_fill() +
  geom_spatvector(data=realmvect, aes(fill=realmname), alpha=0.4) +
  theme(legend.position = 'none')

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Higher Frequency",
                    ylab = "Higher Severity ",
                    breaks=break_vals,
                    size = 9)


finalPlot_hist <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.2, 0.2)
finalPlot_histb <- ggdraw() +
  draw_plot(map2, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.2, 0.2)

## read in cp 8.5 late drought data first for legend ----
future <- rast("data/drought/allmetrics_summary_rcp85_late_delta_down.tif")

double <- crop(future[[c(7, 22)]], forest_vect, mask=T) %>% 
  project("+proj=robin")

df <- as.data.frame(double, na.rm=T, xy=T)
head(df)
df$delta_max_severe_median <- df$delta_max_severe_median*-1
head(df)

negatives <- filter(df, delta_max_severe_median<=0, delta_dr_freq_median<=0)

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
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2, aes(x=x, y=y, fill=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink", dim=3)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  ggtitle("Projected drought changes (RCP 8.5 late-century)")

map2 <- map +
  geom_tile(data=negatives, aes(x=x, y=y), color="gold", size=0.1, fill=NA, show.legend = FALSE) 

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Increasing Frequency",
                    ylab = "Increasing Severity ",
                    breaks=break_vals,
                    size = 9)


finalPlot5 <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.2, 0.2)
finalPlot5b <- ggdraw() +
  draw_plot(map2, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.2, 0.2)


## read in cp 8.5 mid drought data ----
future <- rast("data/drought/allmetrics_summary_rcp85_mid_delta_down.tif")

double <- crop(future[[c(7,22)]], forest_vect, mask=T) %>% 
  project("+proj=robin")

df <- as.data.frame(double, na.rm=T, xy=T)
head(df)
df$delta_max_severe_median <- df$delta_max_severe_median*-1
head(df)
negatives <- filter(df, delta_max_severe_median<=0, delta_dr_freq_median<=0)

## cut data

df$freq_bin <- cut(df$delta_dr_freq_median, breaks = c(min(df$delta_dr_freq_median), break_vals$bi_x[2], break_vals$bi_x[3], max(df$delta_dr_freq_median)), include.lowest = TRUE)
df$sev_bin <- cut(df$delta_max_severe_median, breaks = c(min(df$delta_max_severe_median), break_vals$bi_y[2], break_vals$bi_y[3], max(df$delta_max_severe_median)), include.lowest = TRUE)


df2 <- bi_class(df, x=freq_bin, y=sev_bin, dim=3)
interval_vals <- bi_class_breaks(df,
                                 x = freq_bin, y = sev_bin, dim = 3, dig_lab = c(x = 2, y = 3),
                                 split = FALSE)
break_vals <- bi_class_breaks(df,
                              x = freq_bin, y = sev_bin, dim = 3, dig_lab = c(x = 2, y = 3),
                              split = TRUE)

## funny code for the negative values
break_vals$bi_x <- break_vals$bi_x[-1]
break_vals$bi_y <- break_vals$bi_y[-1]

break_vals$bi_x[1] <- break_vals$bi_x[1]*-1
break_vals$bi_y[1] <- break_vals$bi_y[1]*-1



map <- ggplot()+
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2, aes(x=x, y=y, fill=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink", dim=3)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  ggtitle("Projected drought changes (RCP 8.5 mid-century)")
map2 <- map +
  geom_tile(data=negatives, aes(x=x, y=y), color="gold", size=0.1, fill=NA, show.legend = FALSE) 


# legend <- bi_legend(pal = "GrPink",
                    # dim = 3,
                    # xlab = "Increasing Frequency",
                    # ylab = "Increasing Severity ",
                    # breaks=break_vals,
                    # size = 7)


finalPlot3 <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.2, 0.2)
finalPlot3b <- ggdraw() +
  draw_plot(map2, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.2, 0.2)

## read in rcp 2.6 late drought data for legend ----
future <- rast("data/drought/allmetrics_summary_rcp26_late_delta_down.tif")

double <- crop(future[[c(7,22)]], forest_vect, mask=T) %>% 
  project("+proj=robin")

df <- as.data.frame(double, na.rm=T, xy=T)
head(df)
df$delta_max_severe_median <- df$delta_max_severe_median*-1
head(df)

negatives <- filter(df, delta_max_severe_median<=0, delta_dr_freq_median<=0)


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
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2, aes(x=x, y=y, fill=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink", dim=3)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  ggtitle("Projected drought changes (RCP 2.6 late-century)")
map2 <- map +
  geom_tile(data=negatives, aes(x=x, y=y), color="gold", size=0.1, fill=NA, show.legend = FALSE) 

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Increasing Frequency",
                    ylab = "Increasing Severity ",
                    breaks=break_vals,
                    size = 9)


finalPlot4 <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.2, 0.2)
finalPlot4b <- ggdraw() +
  draw_plot(map2, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.2, 0.2)


## read in rcp 2.6 mid drought data ----
future <- rast("data/drought/allmetrics_summary_rcp26_mid_delta_down.tif")

double <- crop(future[[c(7,22)]], forest_vect, mask=T) %>% 
  project("+proj=robin")

df <- as.data.frame(double, na.rm=T, xy=T)
head(df)
df$delta_max_severe_median <- df$delta_max_severe_median*-1
head(df)

negatives <- filter(df, delta_max_severe_median<=0, delta_dr_freq_median<=0)

## cut data

df$freq_bin <- cut(df$delta_dr_freq_median, breaks = c(min(df$delta_dr_freq_median), break_vals$bi_x[2], break_vals$bi_x[3], max(df$delta_dr_freq_median)), include.lowest = TRUE)
df$sev_bin <- cut(df$delta_max_severe_median, breaks = c(min(df$delta_max_severe_median), break_vals$bi_y[2], break_vals$bi_y[3], max(df$delta_max_severe_median)), include.lowest = TRUE)


df2 <- bi_class(df, x=freq_bin, y=sev_bin, dim=3)
interval_vals <- bi_class_breaks(df,
                              x = freq_bin, y = sev_bin, dim = 3, dig_lab = c(x = 2, y = 3),
                              split = FALSE)
break_vals <- bi_class_breaks(df,
                              x = freq_bin, y = sev_bin, dim = 3, dig_lab = c(x = 2, y = 3),
                              split = TRUE)

## funny code for the negative values
break_vals$bi_x <- break_vals$bi_x[-1]
break_vals$bi_y <- break_vals$bi_y[-1]

break_vals$bi_x[1] <- break_vals$bi_x[1]*-1
break_vals$bi_y[1] <- break_vals$bi_y[1]*-1

map <- ggplot()+
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2, aes(x=x, y=y, fill=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink", dim=3)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(size=12)) +
  ggtitle("Projected drought changes (RCP 2.6 mid-century)")
map2 <- map +
  geom_tile(data=negatives, aes(x=x, y=y), color="gold", size=0.1, fill=NA, show.legend = FALSE) 

# legend <- bi_legend(pal = "GrPink",
#                     dim = 3,
#                     xlab = "Increasing Frequency",
#                     ylab = "Increasing Severity ",
#                     breaks=break_vals,
#                     size = 7)


finalPlot2 <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.2, 0.2)
finalPlot2b <- ggdraw() +
  draw_plot(map2, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.2, 0.2)


pdf("output/cloropleth map.pdf", height=8, width=11) 
print(list(finalPlot_hist,
           finalPlot_histb,
           finalPlot2,
           finalPlot2b,
           finalPlot4,
           finalPlot4b,
           finalPlot3,
           finalPlot3b,
           finalPlot5,
           finalPlot5b))
dev.off()





