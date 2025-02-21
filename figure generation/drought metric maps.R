library(terra)
library(tidyverse)
library(viridis)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)

df_r <- read.csv("processed data/xy_fine_1.csv") %>% 
  bind_rows(read.csv("processed data/xy_fine_2.csv")) %>% 
  bind_rows(read.csv("processed data/xy_fine_3.csv")) %>% 
  bind_rows(read.csv("processed data/xy_fine_4.csv")) %>% 
  bind_rows(read.csv("processed data/xy_fine_5.csv")) %>% 
  bind_rows(read.csv("processed data/xy_fine_12.csv")) %>% 
  bind_rows(read.csv("processed data/xy_fine_14.csv"))

big_points <- vect(df_r, geom=c("x", "y"), crs="+proj=longlat +datum=WGS84")

layernames <- c("months per decade under drought (min)",
                "months per decade under drought (median)",
                "months per decade under drought (max)",
                "drought event frequency (min)",
                "drought event frequency (median)",
                "drought event frequency (max)",
                "maximum drought event duration (min)",
                "maximum drought event duration (median)",
                "maximum drought event duration (max)",
                "median drought event duration (min)",
                "median drought event duration (median)",
                "median drought event duration (max)",
                "maximum drought severity (min)",
                "maximum drought severity (median)",
                "maximum drought severity (max)",
                "median drought severity (min)",
                "median drought severity (median)",
                "median drought severity (max)"
                )

file_list <- list.files("data/drought/")
file_list_sub <- file_list[grep("allmetrics*", file_list)]
groups <- c("Historic",
            "Late Century RCP 2.6 change",
            "Late Century RCP 2.6",
            "Mid Century RCP 2.6 change",
            "Mid Century RCP 2.6",
            "Late Century RCP 8.5 change",
            "Late Century RCP 8.5",
            "Mid Century RCP 8.5 change",
            "Mid Century RCP 8.5"
               )

world <- ne_coastline(scale = "medium", returnclass = "sf")
class(world)

for(f in 1:9){
  lay <- rast(paste0("data/drought/", file_list_sub[f]))
  lay <- lay %>% mask(big_points)
  
  out_file <- paste0("output/", groups[f], " maps.pdf")
  
  pdf(file=out_file)
  for(i in 1:18) {
    drought_df <- as.data.frame(lay[[i]], xy=T) 
    names(drought_df) <- c("x", "y", "value")
    
    if(i<=12){
      map <- ggplot(data = world) +
        geom_tile(data=drought_df, aes(x,y, fill=value)) +
        geom_sf() +
        scale_fill_viridis(option='rocket', direction = -1)+
        theme_map() +
        theme(legend.position = 'center', legend.title = element_blank())+
        ggtitle(paste0(groups[f], " \n", layernames[i])) +
        theme(legend.position='bottom') +
        theme(plot.title=element_text(size=12,face="bold"),
              legend.text = element_text(size=8))
      print(map)
    } else{ 
      map <- ggplot(data = world) +
        geom_tile(data=drought_df, aes(x,y, fill=value)) +
        geom_sf() +
        scale_fill_viridis(option='rocket', direction = 1)+
        theme_map() +
        theme(legend.position = 'center', legend.title = element_blank())+
        ggtitle(paste0(groups[f], " \n", layernames[i])) +
        theme(legend.position='bottom') +
        theme(plot.title=element_text(size=12,face="bold"),
              legend.text = element_text(size=8))
      print(map)
    }
    
    
    
  }
  dev.off()
  
}

