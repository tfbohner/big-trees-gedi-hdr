### Code for bivariate cloropleth maps for drought metrics
## full forest extent, historic, mid- and late- century RCP 2.6 and 8.5
## compare different breaks and colormaps

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


## read in cp 8.5 late drought data first for legend ----
future <- rast("data/drought/allmetrics_summary_rcp85_late_delta_down.tif")

double <- crop(future[[c(5, 14)]], forest_vect, mask=T) %>% 
  project("+proj=robin")

df <- as.data.frame(double, na.rm=T, xy=T)
head(df)
df$delta_max_severe_median <- df$delta_max_severe_median*-1
head(df)

## read in cp 8.5 mid drought data first for legend ----
future_m <- rast("data/drought/allmetrics_summary_rcp85_mid_delta_down.tif")

double_m <- crop(future_m[[c(5, 14)]], forest_vect, mask=T) %>% 
  project("+proj=robin")

df_m <- as.data.frame(double_m, na.rm=T, xy=T)
head(df_m)
df_m$delta_max_severe_median <- df_m$delta_max_severe_median*-1
head(df_m)

## fisher breaks 3 dim----
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
  geom_tile(data=df2, aes(x=x, y=y, fill=bi_class, color=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink", dim=3)+
  bi_scale_color(pal="GrPink", dim=3)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Increasing Frequency",
                    ylab = "Increasing Severity ",
                    breaks=break_vals,
                    size = 7) +
  theme(plot.margin = unit(c(0,0.5,0,0), "cm"))


df_m$freq_bin <- cut(df_m$delta_dr_freq_median, breaks = break_vals$bi_x, include.lowest = T)
df_m$sev_bin <- cut(df_m$delta_max_severe_median, breaks = break_vals$bi_y, include.lowest = T)

df2_m <- bi_class(df_m, x = freq_bin, y = sev_bin, style = "quantile", dim = 3)


map_m <- ggplot()+
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2_m, aes(x=x, y=y, fill=bi_class, color=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink", dim=3)+
  bi_scale_color(pal="GrPink", dim=3)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(size=18)) +
  theme(plot.margin = unit(c(0.2,0,0,0), "cm"))

title <- ggdraw() + 
  draw_label(
    "3-dim fisher breaks",
    fontface = 'bold',
    hjust = 0.5,
    vjust = 0
  ) 
fisher3 <- plot_grid(
  title, plot_grid(plot_grid(map_m, map, ncol=1, labels=c("mid-century", "late-century")),
                   legend, 
                   ncol=2, rel_widths = c(1, 0.2), align='h', axis='b'),
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

## fisher breaks 4dim----
df2 <- bi_class(df, x=delta_dr_freq_median, y=delta_max_severe_median, style="fisher", dim=4)
interval_vals <- bi_class_breaks(df, style = "fisher",
                                 x = delta_dr_freq_median, y = delta_max_severe_median, dim = 4, dig_lab = c(x = 2, y = 3),
                                 split = FALSE)
break_vals <- bi_class_breaks(df, style = "fisher",
                              x = delta_dr_freq_median, y = delta_max_severe_median, dim = 4, dig_lab = c(x = 2, y = 3),
                              split = TRUE)

## funny code for the negative values
break_vals$bi_x <- break_vals$bi_x[-1]
break_vals$bi_y <- break_vals$bi_y[-1]

break_vals$bi_x[1] <- break_vals$bi_x[1]*-1
break_vals$bi_y[1] <- break_vals$bi_y[1]*-1

map <- ggplot()+
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2, aes(x=x, y=y, fill=bi_class, color=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink2", dim=4)+
  bi_scale_color(pal="GrPink2", dim=4)+
  bi_theme() +
  theme(axis.title = element_blank())+
  theme(plot.margin = unit(c(0.2,0,0,0), "cm"))

legend <- bi_legend(pal = "GrPink2",
                    dim = 4,
                    xlab = "Increasing Frequency",
                    ylab = "Increasing Severity ",
                    breaks=break_vals,
                    size = 7)+
  theme(plot.margin = unit(c(0,0.5,0,0), "cm"))


df_m$freq_bin <- cut(df_m$delta_dr_freq_median, breaks = break_vals$bi_x, include.lowest = T)
df_m$sev_bin <- cut(df_m$delta_max_severe_median, breaks = break_vals$bi_y, include.lowest = T)

df2_m <- bi_class(df_m, x = freq_bin, y = sev_bin, style = "quantile", dim = 4)

map_m <- ggplot()+
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2_m, aes(x=x, y=y, fill=bi_class, color=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink2", dim=4)+
  bi_scale_color(pal="GrPink2", dim=4)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.margin = unit(c(0.2,0,0,0), "cm"))

title <- ggdraw() + 
  draw_label(
    "4-dim fisher breaks",
    fontface = 'bold',
    hjust = 0.5,
    vjust = 0
  ) 
fisher4 <- plot_grid(
  title, plot_grid(plot_grid(map_m, map, ncol=1, labels=c("mid-century", "late-century")),
                   legend, 
                   ncol=2, rel_widths = c(1, 0.2), align='h', axis='b'),
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

## manual 3----
df$freq_bin <- cut(df$delta_dr_freq_median, breaks = c(min(df$delta_dr_freq_median), 0, 
                                                       mean(df$delta_dr_freq_median), 
                                                       max(df$delta_dr_freq_median)), include.lowest = T)
df$sev_bin <- cut(df$delta_max_severe_median, breaks = c(min(df$delta_max_severe_median), 0, 
                                                       mean(df$delta_max_severe_median), 
                                                       max(df$delta_max_severe_median)), include.lowest = T)

  
df2 <- bi_class(df, x = freq_bin, y = sev_bin, style = "quantile", dim = 3)

interval_vals <- bi_class_breaks(df, style = "quantile",
                                 x = freq_bin, y = sev_bin, dim = 3, dig_lab = c(x = 2, y = 3),
                                 split = FALSE)
break_vals <- bi_class_breaks(df, style = "quantile",
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
  geom_tile(data=df2, aes(x=x, y=y, fill=bi_class, color=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink", dim=3)+
  bi_scale_color(pal="GrPink", dim=3)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Increasing Frequency",
                    ylab = "Increasing Severity ",
                    breaks=break_vals,
                    size = 7) +
  theme(plot.margin = unit(c(0,0.5,0,0), "cm"))


df_m$freq_bin <- cut(df_m$delta_dr_freq_median, breaks = break_vals$bi_x, include.lowest = T)
df_m$sev_bin <- cut(df_m$delta_max_severe_median, breaks = break_vals$bi_y, include.lowest = T)

df2_m <- bi_class(df_m, x = freq_bin, y = sev_bin, style = "quantile", dim = 3)


map_m <- ggplot()+
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2_m, aes(x=x, y=y, fill=bi_class, color=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink", dim=3)+
  bi_scale_color(pal="GrPink", dim=3)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(size=18)) +
  theme(plot.margin = unit(c(0.2,0,0,0), "cm"))

title <- ggdraw() + 
  draw_label(
    "3-dim neg, 0, mean, max breaks",
    fontface = 'bold',
    hjust = 0.5,
    vjust = 0
  ) 
manual3 <- plot_grid(
  title, plot_grid(plot_grid(map_m, map, ncol=1, labels=c("mid-century", "late-century")),
                   legend, 
                   ncol=2, rel_widths = c(1, 0.2), align='h', axis='b'),
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

## manual 3b----
df$freq_bin <- cut(df$delta_dr_freq_median, breaks = c(min(df$delta_dr_freq_median), 0, 
                                                       quantile(df$delta_dr_freq_median[df$delta_dr_freq_median>0], probs=0.5), 
                                                       max(df$delta_dr_freq_median)), include.lowest = T)
df$sev_bin <- cut(df$delta_max_severe_median, breaks = c(min(df$delta_max_severe_median), 0, 
                                                         quantile(df$delta_max_severe_median[df$delta_max_severe_median>0], probs=0.5), 
                                                         max(df$delta_max_severe_median)), include.lowest = T)

df2 <- bi_class(df, x = freq_bin, y = sev_bin, style = "quantile", dim = 3)

interval_vals <- bi_class_breaks(df, style = "quantile",
                                 x = freq_bin, y = sev_bin, dim = 3, dig_lab = c(x = 2, y = 3),
                                 split = FALSE)
break_vals <- bi_class_breaks(df, style = "quantile",
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
  geom_tile(data=df2, aes(x=x, y=y, fill=bi_class, color=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink", dim=3)+
  bi_scale_color(pal="GrPink", dim=3)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Increasing Frequency",
                    ylab = "Increasing Severity ",
                    breaks=break_vals,
                    size = 7) +
  theme(plot.margin = unit(c(0,0.5,0,0), "cm"))


df_m$freq_bin <- cut(df_m$delta_dr_freq_median, breaks = break_vals$bi_x, include.lowest = T)
df_m$sev_bin <- cut(df_m$delta_max_severe_median, breaks = break_vals$bi_y, include.lowest = T)

df2_m <- bi_class(df_m, x = freq_bin, y = sev_bin, style = "quantile", dim = 3)


map_m <- ggplot()+
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2_m, aes(x=x, y=y, fill=bi_class, color=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink", dim=3)+
  bi_scale_color(pal="GrPink", dim=3)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(size=18)) +
  theme(plot.margin = unit(c(0.2,0,0,0), "cm"))

title <- ggdraw() + 
  draw_label(
    "3-dim neg-0 bin, positive quantile categories",
    fontface = 'bold',
    hjust = 0.5,
    vjust = 0
  ) 
manual3b <- plot_grid(
  title, plot_grid(plot_grid(map_m, map, ncol=1, labels=c("mid-century", "late-century")),
                   legend, 
                   ncol=2, rel_widths = c(1, 0.2), align='h', axis='b'),
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
## manual 3c----
df$freq_bin <- cut(df$delta_dr_freq_median, breaks = c(min(df$delta_dr_freq_median), 0, 
                                                       max(df$delta_dr_freq_median)/2, 
                                                       max(df$delta_dr_freq_median)), include.lowest = T)
df$sev_bin <- cut(df$delta_max_severe_median, breaks = c(min(df$delta_max_severe_median), 0, 
                                                         max(df$delta_max_severe_median)/2, 
                                                         max(df$delta_max_severe_median)), include.lowest = T)

df2 <- bi_class(df, x = freq_bin, y = sev_bin, style = "quantile", dim = 3)

interval_vals <- bi_class_breaks(df, style = "quantile",
                                 x = freq_bin, y = sev_bin, dim = 3, dig_lab = c(x = 2, y = 3),
                                 split = FALSE)
break_vals <- bi_class_breaks(df, style = "quantile",
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
  geom_tile(data=df2, aes(x=x, y=y, fill=bi_class, color=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink", dim=3)+
  bi_scale_color(pal="GrPink", dim=3)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"))

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Increasing Frequency",
                    ylab = "Increasing Severity ",
                    breaks=break_vals,
                    size = 7) +
  theme(plot.margin = unit(c(0,0.5,0,0), "cm"))


df_m$freq_bin <- cut(df_m$delta_dr_freq_median, breaks = break_vals$bi_x, include.lowest = T)
df_m$sev_bin <- cut(df_m$delta_max_severe_median, breaks = break_vals$bi_y, include.lowest = T)

df2_m <- bi_class(df_m, x = freq_bin, y = sev_bin, style = "quantile", dim = 3)


map_m <- ggplot()+
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2_m, aes(x=x, y=y, fill=bi_class, color=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink", dim=3)+
  bi_scale_color(pal="GrPink", dim=3)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.title = element_text(size=18)) +
  theme(plot.margin = unit(c(0.2,0,0,0), "cm"))

title <- ggdraw() + 
  draw_label(
    "3-dim neg-0 bin, even positive categories",
    fontface = 'bold',
    hjust = 0.5,
    vjust = 0
  ) 
manual3c <- plot_grid(
  title, plot_grid(plot_grid(map_m, map, ncol=1, labels=c("mid-century", "late-century")),
                   legend, 
                   ncol=2, rel_widths = c(1, 0.2), align='h', axis='b'),
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

## manual 4b----
df$freq_bin <- cut(df$delta_dr_freq_median, breaks = c(min(df$delta_dr_freq_median), 0, 
                                                       quantile(df$delta_dr_freq_median[df$delta_dr_freq_median>0], probs=0.33), 
                                                       quantile(df$delta_dr_freq_median[df$delta_dr_freq_median>0], probs=0.66), 
                                                       max(df$delta_dr_freq_median)), include.lowest = T)
df$sev_bin <- cut(df$delta_max_severe_median, breaks = c(min(df$delta_max_severe_median), 0, 
                                                         quantile(df$delta_max_severe_median[df$delta_max_severe_median>0], probs=0.33), 
                                                         quantile(df$delta_max_severe_median[df$delta_max_severe_median>0], probs=0.66), 
                                                         max(df$delta_max_severe_median)), include.lowest = T)

df2 <- bi_class(df, x = freq_bin, y = sev_bin, style = "quantile", dim = 4)

interval_vals <- bi_class_breaks(df, style = "quantile",
                                 x = freq_bin, y = sev_bin, dim = 4, dig_lab = c(x = 2, y = 3),
                                 split = FALSE)
break_vals <- bi_class_breaks(df, style = "quantile",
                              x = freq_bin, y = sev_bin, dim = 4, dig_lab = c(x = 2, y = 3),
                              split = TRUE)

## funny code for the negative values
break_vals$bi_x <- break_vals$bi_x[-1]
break_vals$bi_y <- break_vals$bi_y[-1]

break_vals$bi_x[1] <- break_vals$bi_x[1]*-1
break_vals$bi_y[1] <- break_vals$bi_y[1]*-1



map <- ggplot()+
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2, aes(x=x, y=y, fill=bi_class, color=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink2", dim=4)+
  bi_scale_color(pal="GrPink2", dim=4)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.margin = unit(c(0.2,0,0,0), "cm"))

legend <- bi_legend(pal = "GrPink2",
                    dim = 4,
                    xlab = "Increasing Frequency",
                    ylab = "Increasing Severity ",
                    breaks=break_vals,
                    size = 7)+
  theme(plot.margin = unit(c(0,0.5,0,0), "cm"))


df_m$freq_bin <- cut(df_m$delta_dr_freq_median, breaks = break_vals$bi_x, include.lowest = T)
df_m$sev_bin <- cut(df_m$delta_max_severe_median, breaks = break_vals$bi_y, include.lowest = T)

df2_m <- bi_class(df_m, x = freq_bin, y = sev_bin, style = "quantile", dim = 4)

map_m <- ggplot()+
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2_m, aes(x=x, y=y, fill=bi_class, color=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink2", dim=4)+
  bi_scale_color(pal="GrPink2", dim=4)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.margin = unit(c(0.2,0,0,0), "cm"))

title <- ggdraw() + 
  draw_label(
    "4-dim neg-0 bin, positive quantile categories",
    fontface = 'bold',
    hjust = 0.5,
    vjust = 0
  ) 
manual4b <- plot_grid(
  title, plot_grid(plot_grid(map_m, map, ncol=1, labels=c("mid-century", "late-century")),
                   legend, 
                   ncol=2, rel_widths = c(1, 0.2), align='h', axis='b'),
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

## manual 4c----
df$freq_bin <- cut(df$delta_dr_freq_median, breaks = c(min(df$delta_dr_freq_median), 0, 
                                                       max(df$delta_dr_freq_median)*0.33, 
                                                       max(df$delta_dr_freq_median)*0.66, 
                                                       max(df$delta_dr_freq_median)), include.lowest = T)
df$sev_bin <- cut(df$delta_max_severe_median, breaks = c(min(df$delta_max_severe_median), 0, 
                                                         max(df$delta_max_severe_median)*0.33, 
                                                         max(df$delta_max_severe_median)*0.66, 
                                                         max(df$delta_max_severe_median)), include.lowest = T)

df2 <- bi_class(df, x = freq_bin, y = sev_bin, style = "quantile", dim = 4)

interval_vals <- bi_class_breaks(df, style = "quantile",
                                 x = freq_bin, y = sev_bin, dim = 4, dig_lab = c(x = 2, y = 3),
                                 split = FALSE)
break_vals <- bi_class_breaks(df, style = "quantile",
                              x = freq_bin, y = sev_bin, dim = 4, dig_lab = c(x = 2, y = 3),
                              split = TRUE)

## funny code for the negative values
break_vals$bi_x <- break_vals$bi_x[-1]
break_vals$bi_y <- break_vals$bi_y[-1]

break_vals$bi_x[1] <- break_vals$bi_x[1]*-1
break_vals$bi_y[1] <- break_vals$bi_y[1]*-1



map <- ggplot()+
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2, aes(x=x, y=y, fill=bi_class, color=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink2", dim=4)+
  bi_scale_color(pal="GrPink2", dim=4)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.margin = unit(c(0.2,0,0,0), "cm"))

legend <- bi_legend(pal = "GrPink2",
                    dim = 4,
                    xlab = "Increasing Frequency",
                    ylab = "Increasing Severity ",
                    breaks=break_vals,
                    size = 7)+
  theme(plot.margin = unit(c(0,0.5,0,0), "cm"))


df_m$freq_bin <- cut(df_m$delta_dr_freq_median, breaks = break_vals$bi_x, include.lowest = T)
df_m$sev_bin <- cut(df_m$delta_max_severe_median, breaks = break_vals$bi_y, include.lowest = T)

df2_m <- bi_class(df_m, x = freq_bin, y = sev_bin, style = "quantile", dim = 4)

map_m <- ggplot()+
  geom_spatvector(data=countries, fill=NA, color="light gray") +
  # coord_sf(crs="+init=epsg:3857") +
  geom_tile(data=df2_m, aes(x=x, y=y, fill=bi_class, color=bi_class), show.legend = FALSE) +
  bi_scale_fill(pal="GrPink2", dim=4)+
  bi_scale_color(pal="GrPink2", dim=4)+
  bi_theme() +
  theme(axis.title = element_blank()) +
  theme(plot.margin = unit(c(0.2,0,0,0), "cm"))

title <- ggdraw() + 
  draw_label(
    "4-dim neg-0 bin, even positive categories",
    fontface = 'bold',
    hjust = 0.5,
    vjust = 0
  ) 
manual4c <- plot_grid(
  title, plot_grid(plot_grid(map_m, map, ncol=1, labels=c("mid-century", "late-century")),
                   legend, 
                   ncol=2, rel_widths = c(1, 0.2), align='h', axis='b'),
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

pdf("output/cloropleth_alt_scales_rcp85.pdf", height=8, width=10)
print(fisher3)
print(manual3)
print(manual3b)
print(manual3c)
print(fisher4)
print(manual4b)
print(manual4c)
dev.off()
