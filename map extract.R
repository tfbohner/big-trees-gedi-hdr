# library(sp)
# library(rgdal)
# library(raster)
library(terra)
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)


path <- "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/GEDI_L3_LandSurface_Metrics_V2_1952/data/"

rh1 <- rast(x= paste0(path, "GEDI03_rh100_mean_2019108_2021104_002_02", ".tif"))

elev <- rast(x= paste0(path, "GEDI03_elev_lowestmode_mean_2019108_2020287_002_02", ".tif"))
str(rh1)
# plot(rh100)

rh100 <- terra::project(rh1, "epsg:4326")

e <- ext(-180, 180, -52, 52)
rh100 <- terra::crop(rh100, e)

path2 <- "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/wwf/"

biomes <- vect(
  paste0(path2, "wwf_terr_ecos.shp"))

biomes$REALM[is.na(biomes$REALM)] <- "NeA"

biomes.df <- as.data.frame(biomes)

test <- group_by(biomes.df, REALM, BIOME) %>% 
  summarize()

# test2 <- filter(test, REALM=="NA")

## For each biogeographic realm, subset the vector, then 
## For each biome within the realm, subset the vector
## highlight and fill in the area on a global map
## extract GEDI heights for the polygon
## plot a histogram of gedi heights
## two plots per page (map, histogram)
## each page realm and biome. Create a table for name reference.

e <- ext(-180, 180, -52, 52)
biomes.crop <- crop(biomes, e)

biomes.crop$REALM[biomes.crop$REALM==""] <- "NeA"


r="AA"
b=2

## if dim(x3)[1]==0, then skip

## stuff for plotting
biomenames <- read.csv("data/biomenames.csv")
realmnames <- read.csv("data/realmnames.csv")


realmnames$REALM[5]="NeA"

# world <- ne_countries(scale = "medium", returnclass = "sf")
world <- ne_coastline(scale = "medium", returnclass = "sf")
class(world)


pdf("output/map_hist2.pdf")
for(r in unique(realmnames$REALM)){
  x <- subset(biomes.crop, biomes.crop$REALM==r)
  if(dim(x)[1]==0) next
  x2 <- aggregate(x, "BIOME")
  realmname <- realmnames$name[realmnames$REALM==r]
  for(b in 1:14){
    x3 <- subset(x2, x2$BIOME==b)
    # print(dim(x3)[1])}
    if(dim(x3)[1]==0) next
    
    # par(mfrow=c(2,1))
    biomename <- biomenames$name[biomenames$BIOME==b]
    # plot(x3, "BIOME", col=rainbow(14), main=paste0(realmname, ", ", biomename))
    

    y <- terra::extract(rh100, x3, na.rm=T)
    
    
    names(y) <- c("ID", "height")
    
    # write.csv(paste0("processed data/realm_", r, "__biome_" , b, ".csv"))
    
    # hist(y$height, main= "GEDI height")
    
    x4 <- sf::st_as_sf(x3)
    
    
    map <-ggplot(data = world) +
      geom_sf() +
      geom_sf(data = x4, fill="red", color="red", lwd=1) +
      theme_test() +
      ggtitle(paste0(realmname, ", ", biomename))
    
    hgram <- ggplot(y, aes(height)) +
      geom_histogram()+
      theme_test()+
      ggtitle("GEDI height")
    
    print(plot_grid(map, hgram, ncol=1))
    
    
  }
}
dev.off()


