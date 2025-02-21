### Code for bivariate cloropleth maps for drought metrics
## tallest trees only

library(tidyverse)
library(terra)
library(tidyterra)
library(biscale)
library(ggthemes)
library(cowplot)
library(sf)

countries <- vect("data/ne_50m_land/ne_50m_land.shp")
countries <- crop(countries, ext(-180, 180, -52, 52)) %>% 
  project("+proj=robin")

## read in historic drought data
hist <- rast("data/drought/allmetrics_summary_hist_down.tif")[[c(5, 14)]] 

height90 <- rast("processed data/rast_height.tif") %>% 
  resample(hist, method="near")

double <- crop(hist, height90, mask=T) %>% 
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
  ggtitle("Tallest Forests, Historic Drought")

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Higher Frequency",
                    ylab = "Higher Severity ",
                    breaks=break_vals,
                    size = 8)


finalPlot_hist <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.18, 0.18)

## read in cp 8.5 late drought data first for legend ----
future <- rast("data/drought/allmetrics_summary_rcp85_late_delta_down.tif")

double <- crop(future[[c(5, 14)]], height90, mask=T) %>% 
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
  ggtitle("Tallest forests, RCP 8.5 late-century projection")

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Increasing Frequency",
                    ylab = "Increasing Severity ",
                    breaks=break_vals,
                    size = 7)


finalPlot5 <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.18, 0.18)


## read in cp 8.5 mid drought data ----
future <- rast("data/drought/allmetrics_summary_rcp85_mid_delta_down.tif")

double <- crop(future[[c(5, 14)]], height90, mask=T) %>% 
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
  ggtitle("Tallest forests, RCP 8.5 mid-century projection")

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Increasing Frequency",
                    ylab = "Increasing Severity ",
                    breaks=break_vals,
                    size = 7)

finalPlot3 <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.18, 0.18)

## read in rcp 2.6 late drought data for legend ----
future <- rast("data/drought/allmetrics_summary_rcp26_late_delta_down.tif")

double <- crop(future[[c(5, 14)]], height90, mask=T) %>% 
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
  ggtitle("Tallest forests, RCP 2.6 late-century projection")

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Increasing Frequency",
                    ylab = "Increasing Severity ",
                    breaks=break_vals,
                    size = 7)

finalPlot4 <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.18, 0.18)


## read in rcp 2.6 mid drought data ----
future <- rast("data/drought/allmetrics_summary_rcp26_mid_delta_down.tif")

double <- crop(future[[c(5, 14)]], height90, mask=T) %>% 
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
  ggtitle("Tallest forests, RCP 2.6 mid-century projection")

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Increasing Frequency",
                    ylab = "Increasing Severity ",
                    breaks=break_vals,
                    size = 7)

finalPlot2 <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.18, 0.18)


pdf("output/cloropleth map_tallest.pdf", height=8, width=11) 
print(finalPlot_hist)
print(finalPlot2)
print(finalPlot4)
print(finalPlot3)
print(finalPlot5)
dev.off()