library(terra)
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)
library(viridis)
library(tidyterra)
# library(rgdal)

## Script to match drought metrics with GEDI biomass and height, WWF Biomes, 
## outputs: matched csv files for gedi and drought in each biome as well as rasters at different grids for subsequent analysis and plots
mainpath <-"/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees"

## 0. Drought layers----
## need these here to coarsen all the other layers to match
path <- paste0(mainpath, "/data/drought/")
e <- ext(-180, 180, -52, 52)

dr_hist <- rast(x= paste0(path, "months_neg2_drought_hist_down", ".tif")) %>% 
  crop(e)
dr_mid26 <- rast(x= paste0(path, "months_neg2_drought_mid_rcp26", ".tif")) %>% 
  crop(e)
dr_late26 <- rast(x= paste0(path, "months_neg2_drought_late_rcp26", ".tif")) %>% 
  crop(e)
dr_mid85 <- rast(x= paste0(path, "months_neg2_drought_mid_rcp85", ".tif")) %>% 
  crop(e)
dr_late85 <- rast(x= paste0(path, "months_neg2_drought_late_rcp85", ".tif")) %>% 
  crop(e)

## 1. GEDI and WWF Biomes ----

## biomes ----
path3 <- paste0(mainpath, "/data/wwf/")

biomes <- vect(
  paste0(path3, "wwf_terr_ecos.shp"))
# biomes <- readOGR(
#   paste0(path3, "wwf_terr_ecos.shp"))

biomes$REALM[is.na(biomes$REALM)] <- "NeA"

biomes <- crop(biomes, e)

biomenames <- read.csv(paste0(mainpath,"/data/biomenames.csv"))

names(biomenames)[2] <- "longname"

biomenames$name <- c("Tropical Moist Broadleaf",
                     "Tropical Dry Broadleaf",
                     "Tropical Conifer",
                     "Temperate Broadleaf",
                     "Temperate Conifer",
                     "Boreal",
                     "Tropical Savannas",
                     "Temperate Savannas",
                     "Flooded Savannas",
                     "Montane Shrublands",
                     "Tundra",
                     "Mediterranean Forests",
                     "Deserts",
                     "Mangroves")

## Get only forest biomes, remove boreal from list too
forests <- dplyr::filter(biomenames, grepl("Forest|Mangrove", longname)) %>%
  dplyr::filter( !grepl("Boreal", longname))
# forests <- dplyr::filter(biomenames, grepl("Forest", longname)) %>%
#   dplyr::filter( !grepl("Boreal", longname))

fbiomes <- subset(biomes, biomes$BIOME%in%forests$BIOME) 

# fbiomes$realm_biome <- paste0(fbiomes$REALM, fbiomes$BIOME)
# fbiomes2 <- aggregate(fbiomes, "realm_biome")
fbiomes2 <- aggregate(fbiomes, "BIOME")

values(fbiomes2) <- forests

realms <- aggregate(fbiomes, "REALM")
values(realms)

realmnames <- read.csv(paste0(mainpath,"/data/realmnames.csv")) %>% 
  rename(realmname=name)

## some funny code to make sure that all the realms match up correctly
realmnames$REALM[is.na(realmnames$REALM)] <- "NeA"
realmnames <- filter(realmnames, REALM !="AN")
realmnames$realmno <- c(1:3, 5, 4,6, 7)
realmnames <- arrange(realmnames, realmno)


# add a data.frame
values(realms) <- realmnames

writeVector(fbiomes2, "processed data/biome_vect.shp")
writeVector(realms, "processed data/realm_vect.shp")


## gedi layers----

## biomass 
path2 <- paste0(mainpath,"/data/GEDI_L4B_Gridded_Biomass_2017/data/")
agb <- rast(x=paste0(path2, "GEDI04_B_MW019MW138_02_002_05_R01000M_MU.tif"))
agb <- project(agb, "EPSG:4326") %>% 
  crop(e)

## height
path2 <- paste0(mainpath,"/data/GEDI June 2022/GEDI_L3_LandSurface_Metrics_V2_1952/data/")
rh100 <- rast(x= paste0(path2, "GEDI03_rh100_mean_2019108_2022019_002_03", ".tif"))

rh_coarse <- raster::resample(rh100, dr_hist, method='max')
plot(rh_coarse)

rh_coarse <- crop(rh_coarse, e)

rh100 <- project(rh100, "EPSG:4326")
rh100 <- crop(rh100, e)


## rasterize wwf layers to match
b_rast <- terra::rasterize(fbiomes, rh100, "BIOME")
r_rast <- rasterize(realms, rh100, "realmno", getCover=T)
plot(b_rast)

NAflag(b_rast) <-NA
NAflag(r_rast) <-NA
NAflag(rh100) <-NA


height_r <- c(rh100, r_rast)

keep <- tmpFiles(current=TRUE)

forests$BIOME


# ## For CI
# write.csv(realmnames, "GIS layers for CI/realmnames.csv")
# write.csv(biomenames, "GIS layers for CI/biomenames.csv")
# 
# writeRaster(b_rast, "GIS layers for CI/biome_all_1km.tif")
# writeRaster(r_rast, "GIS layers for CI/bgrealm_all_1km.tif")
# writeRaster(rh100, "GIS layers for CI/height_all_1km.tif")
# 
# b_rast <- rast("GIS layers for CI/biome_all_1km.tif")
# r_rast <- rast("GIS layers for CI/bgrealm_all_1km.tif")
# rh100 <- rast("GIS layers for CI/height_all_1km.tif")

biome <- b_rast
biome[!(biome%in%c(1,2,3,4,5,12,14))] <- NA
temp_rast <- mask(agb, biome)

writeRaster(temp_rast, "processed data/rast_agb.tif")

temp_points <- as.points(temp_rast, na.rm=T)
xy <- crds(temp_points)

realm <- mask(r_rast, temp_rast)
values2 <- values(realm)
values2_short <- values2[!is.na(values2),]



### produce intermediate data for all gedi cells----
for(b in c(1,2,3,4,5,12,14)){
  print(b)
  biome <- b_rast
  biome[biome!=b] <- NA
  
  temp_rast <- mask(rh100, biome)
  
  values1 <- values(temp_rast)
  upper <- quantile(values1, 0.9, na.rm=T)
  
  temp_rast2 <- temp_rast
  temp_rast2[temp_rast2<upper] <- NA
  temp_points <- as.points(temp_rast2, na.rm=T)
  xy <- crds(temp_points)
  
  realm <- mask(r_rast, temp_rast2)
  values2 <- values(realm)
  values2_short <- values2[!is.na(values2),]
  
  df_r <- bind_cols(temp_points$GEDI03_rh100_mean_2019108_2021104_002_02, values2_short, xy[,1], xy[,2])
  names(df_r) <- c("height", "realm", "x", "y")
  df_r$biome <- b
  
  
  name <- paste0(mainpath,"/processed data/xy_fine_", b, ".csv")
  write.csv(df_r, name, row.names = F) 
  
}


## produce intermediate data for 90th percentile cells----
for(b in c(1,2,3,4,5,12,14)){
  print(b)
  biome <- b_rast
  biome[biome!=b] <- NA
  
  temp_rast <- mask(rh100, biome)
  
  values1 <- values(temp_rast)
  upper <- quantile(values1, 0.9, na.rm=T)
  
  temp_rast2 <- temp_rast
  temp_rast2[temp_rast2<upper] <- NA
  
  
  name <- paste0(mainpath,"/processed data/rast", b, ".tif")
  writeRaster(temp_rast2, name, overwrite=TRUE)
  
}

path2 <- paste0(mainpath,"/processed data/rast")

r1 <- rast(x= paste0(path2, "1", ".tif"))
r2 <- rast(x= paste0(path2, "2", ".tif"))
r3 <- rast(x= paste0(path2, "3", ".tif"))
r4 <- rast(x= paste0(path2, "4", ".tif"))
r5 <- rast(x= paste0(path2, "5", ".tif"))
r12 <- rast(x= paste0(path2, "12", ".tif"))
r14 <- rast(x= paste0(path2, "14", ".tif"))

big_rast <- merge(r1, r2, r3, r4, r5, r12, r14)

name <- paste0(mainpath,"/processed data/rast_height.tif")
writeRaster(big_rast, name, overwrite=TRUE)

temp_rast <- mask(b_rast, big_rast)
name <- paste0(mainpath,"/processed data/rast_biome.tif")
writeRaster(temp_rast, name, overwrite=TRUE)

temp_rast <- mask(r_rast, big_rast)
name <- paste0(mainpath,"/processed data/rast_realm.tif")
writeRaster(temp_rast, name, overwrite=TRUE)

world <- ne_coastline(scale = "medium", returnclass = "sf")
class(world)

map <- ggplot(data = world) +
  geom_sf() +
  geom_tile(data=df_r, aes(x, y), color='red') +
  # scale_fill_viridis(option='plasma', direction=-1)+
  theme_map() +
  ylim(c(-52,52)) 

df_r <- read.csv("processed data/xy_fine_1.csv") %>% 
  bind_rows(read.csv("processed data/xy_fine_2.csv")) %>% 
  bind_rows(read.csv("processed data/xy_fine_3.csv")) %>% 
  bind_rows(read.csv("processed data/xy_fine_4.csv")) %>% 
  bind_rows(read.csv("processed data/xy_fine_5.csv")) %>% 
  bind_rows(read.csv("processed data/xy_fine_12.csv")) %>% 
  bind_rows(read.csv("processed data/xy_fine_14.csv"))

big_points <- vect(df_r, geom=c("x", "y"), crs="+proj=longlat +datum=WGS84")

## 2. Drought stuff----
## drought frequencies at the coarse raster scale

hist_med <- app(dr_hist, median) %>% 
  mask(big_points)
mid_26 <- app(dr_mid26, median) %>% 
  mask(big_points)
late_26 <- app(dr_late26, median) %>% 
  mask(big_points)
mid_85 <- app(dr_mid85, median) %>% 
  mask(big_points)
late_85 <- app(dr_late85, median) %>% 
  mask(big_points)

dif_m26 <- mid_26-hist_med

drought_all <- c(mid_26, late_26, mid_85, late_85)
names(drought_all) <- c("mid_26", "late_26", "mid_85", "late_85")
dif_all <- drought_all - hist_med


writeRaster(hist_med, "processed data/historic_drought.tif")
writeRaster(drought_all, "processed data/future_drought.tif")
writeRaster(dif_all, "processed data/future_dif.tif")

drought_df <- as.data.frame(dif_all, xy=T)

drought_long2 <- drought_df %>% 
  pivot_longer(cols = -c(x,y), names_to = "group", values_to="freq_dif")


## maps and such ----
world <- ne_coastline(scale = "medium", returnclass = "sf")
class(world)

a <- ggplot(data = world) +
  geom_sf() +
  geom_tile(data=filter(drought_long2, group=="mid_26"), aes(x,y, fill=freq_dif)) +
  scale_fill_viridis(option='rocket', direction = -1, limits=c(-3, 63))+
  theme_map() +
  theme(legend.position = 'right', legend.title = element_blank())+
  ggtitle("Mid century RCP 2.6") +
  theme(legend.position='none') +
  theme(plot.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=8))
a

b <- ggplot(data = world) +
  geom_sf() +
  geom_tile(data=filter(drought_long2, group=="mid_85"), aes(x,y, fill=freq_dif)) +
  scale_fill_viridis(option='rocket', direction = -1, limits=c(-3, 63))+
  theme_map() +
  theme(legend.position = 'right', legend.title = element_blank())+
  ggtitle("Mid century RCP 8.5") +
  theme(legend.position='none') +
  theme(plot.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=8))
b

c <- ggplot(data = world) +
  geom_sf() +
  geom_tile(data=filter(drought_long2, group=="late_26"), aes(x,y, fill=freq_dif)) +
  scale_fill_viridis(option='rocket', direction = -1, limits=c(-3, 63))+
  theme_map() +
  theme(legend.position = 'right', legend.title = element_blank())+
  ggtitle("Late century RCP 2.6") +
  theme(legend.position='none') +
  theme(plot.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=8))
c

d <- ggplot(data = world) +
  geom_sf() +
  geom_tile(data=filter(drought_long2, group=="late_85"), aes(x,y, fill=freq_dif)) +
  scale_fill_viridis(option='rocket', direction = -1, limits=c(-3, 63))+
  theme_map() +
  theme(legend.position = 'right', legend.title = element_blank())+
  ggtitle("Late century RCP 8.5") +
  # guides(fill=guide_legend(title="months/decade")) +
  theme(plot.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=8))

d

legend <- get_legend(d)

d <- d +
  theme(legend.position='none') 


plot_grid(plot_grid(a,b,c,d, ncol=2, align="hv", axis="tblr"), legend,
          rel_widths = c(1, 0.1))



## fine scale stuff
coarse_fun <- function(layer, points, group_lab){
  dr_sub <- mask(layer, points)
  
  dr_sub <- as.data.frame(dr_sub, xy=T, na.rm=T)
  
  names(dr_sub)[3:11] <- c("mo_AWI_CM", "mo_EC_Earth3", "mo_EC_Earth3_Veg", 
                           "mo_GFDL_ESM4", "mo_INM_CM4_8", "mo_INM_CM5_0", 
                           "mo_MPI_ESM1_2", "mo_MRI_ESM2_0", "mo_NorESM2_MM")
  
  dr_match2 <- dr_sub %>% 
    mutate(group=group_lab)
}


## get historic and all rcp
dr_hist_match <- coarse_fun(dr_hist, big_points, 'historic')
dr_mid_match26 <- coarse_fun(dr_mid26, big_points, 'mid rcp 2.6')
dr_late_match26 <- coarse_fun(dr_late26, big_points, 'late rcp 2.6')
dr_mid_match85 <- coarse_fun(dr_mid85, big_points, 'mid rcp 8.5')
dr_late_match85 <- coarse_fun(dr_late85, big_points, 'late rcp 8.5')


world <- ne_coastline(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world)  +
  geom_sf()+
  geom_tile(data=dr_hist_match, aes(x, y, fill=mo_AWI_CM)) +
  scale_color_viridis(option='plasma', direction=-1)+
  theme_map() +
  ylim(c(-52,52)) 


## wide format
drought_all <- bind_rows(dr_hist_match, dr_mid_match26) %>% 
  bind_rows(dr_late_match26) %>% 
  bind_rows(dr_mid_match85) %>% 
  bind_rows(dr_late_match85) %>% 
  mutate(group=fct_relevel(group, c("historic", "mid rcp 2.6", "late rcp 2.6", "mid rcp 8.5", "late rcp 8.5")),
         rcp=ifelse(str_detect(group, "2.6")==T, 2.6, 
                    ifelse(str_detect(group, "8.5")==T, 8.5, NA))) %>% 
  rowwise() %>% 
  mutate(mo_med=median(c_across(starts_with("mo"))),
         across(starts_with("mo"), ~ifelse(.x>=10, "yes", "no"), .names = "over10_{col}")) 

## long table including historic
drought_long <- drought_all %>% 
  dplyr::select(!starts_with("over10")) %>% 
  pivot_longer(cols=starts_with("mo"), names_to="model", names_prefix = "mo_", values_to="months") %>% 
  mutate(over10=ifelse(months>=10, "yes", "no")) 

## for historic comparison
historic <- filter(drought_long, group=="historic") %>% 
  dplyr::select(c(x, y, model, "hist_months"="months"))

## long table with historic comparison
drought_long2 <- filter(drought_long, group!="historic") %>% 
  left_join(historic) %>% 
  mutate(freq_dif=months-hist_months,
         time=word(group, 1),
         time = fct_relevel(time, "mid", "late"),
         rcp=as.factor(rcp))






### fine scale 
extract_fun <- function(layer, xy, group_lab){
  dr_sub <- terra::extract(layer, xy[,3:4])
  
  names(dr_sub)[2:10] <- c("mo_AWI_CM", "mo_EC_Earth3", "mo_EC_Earth3_Veg", 
                           "mo_GFDL_ESM4", "mo_INM_CM4_8", "mo_INM_CM5_0", 
                           "mo_MPI_ESM1_2", "mo_MRI_ESM2_0", "mo_NorESM2_MM")
  
  dr_match2 <- dr_sub %>% 
    bind_cols(xy) %>% 
    mutate(group=group_lab)
}

## get historic and all rcp
dr_hist_match <- extract_fun(dr_hist, df_r, 'historic')
dr_mid_match26 <- extract_fun(dr_mid26, df_r, 'mid rcp 2.6')
dr_late_match26 <- extract_fun(dr_late26, df_r, 'late rcp 2.6')
dr_mid_match85 <- extract_fun(dr_mid85, df_r, 'mid rcp 8.5')
dr_late_match85 <- extract_fun(dr_late85, df_r, 'late rcp 8.5')

test <- dr_hist_match[1:100,] %>% 
  rowwise() %>% 
  mutate(mo_med=median(c_across(starts_with("mo"))),
         mo_med_check=median(c(mo_AWI_CM, mo_EC_Earth3, mo_EC_Earth3_Veg, 
                               mo_GFDL_ESM4, mo_INM_CM4_8, mo_INM_CM5_0, 
                               mo_MPI_ESM1_2, mo_MRI_ESM2_0, mo_NorESM2_MM))) 


## wide format
drought_all <- bind_rows(dr_hist_match, dr_mid_match26) %>% 
  bind_rows(dr_late_match26) %>% 
  bind_rows(dr_mid_match85) %>% 
  bind_rows(dr_late_match85) %>% 
  mutate(group=fct_relevel(group, c("historic", "mid rcp 2.6", "late rcp 2.6", "mid rcp 8.5", "late rcp 8.5")),
         rcp=ifelse(str_detect(group, "2.6")==T, 2.6, 
                    ifelse(str_detect(group, "8.5")==T, 8.5, NA))) 

drought_unique <- drought_all %>% 
  group_by(group, mo_AWI_CM, mo_EC_Earth3, mo_EC_Earth3_Veg, 
           mo_GFDL_ESM4, mo_INM_CM4_8, mo_INM_CM5_0, 
           mo_MPI_ESM1_2, mo_MRI_ESM2_0, mo_NorESM2_MM) %>% 
  tally() %>% 
  rowwise() %>% 
  rename(n_dups=n) %>% 
  mutate(mo_med=median(c_across(starts_with("mo"))))

drought_all <- drought_all %>% 
  left_join(drought_unique)

write.csv(drought_all, "processed data/drought_fine.csv", row.names = F)
drought_all <- read.csv("processed data/drought_fine.csv")


