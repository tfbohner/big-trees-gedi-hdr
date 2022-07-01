library(terra)
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)
library(viridis)
# library(rgdal)

## 0. Drought layers----
## need these here to coarsen all the other layers to match
path <- "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/drought/"
e <- ext(-180, 180, -52, 52)

dr_hist <- rast(x= paste0(path, "months_neg2_drought_hist", ".tif")) %>% 
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
path3 <- "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/wwf/"

biomes <- vect(
  paste0(path3, "wwf_terr_ecos.shp"))
# biomes <- readOGR(
#   paste0(path3, "wwf_terr_ecos.shp"))

biomes$REALM[is.na(biomes$REALM)] <- "NeA"

biomes <- crop(biomes, e)

biomenames <- read.csv("/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/biomenames.csv")

## Get only forest biomes, remove boreal from list too
forests <- dplyr::filter(biomenames, grepl("Forest|Mangrove", name)) %>% 
  dplyr::filter( !grepl("Boreal", name)) 

fbiomes <- subset(biomes, biomes$BIOME%in%forests$BIOME) 

# fbiomes$realm_biome <- paste0(fbiomes$REALM, fbiomes$BIOME)
# fbiomes2 <- aggregate(fbiomes, "realm_biome")
fbiomes2 <- aggregate(fbiomes, "BIOME")

realms <- aggregate(fbiomes, "REALM")
values(realms)

realmnames <- read.csv("/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/realmnames.csv") %>% 
  rename(realmname=name)

## some funny code to make sure that all the realms match up correctly
realmnames$REALM[is.na(realmnames$REALM)] <- "NeA"
realmnames <- filter(realmnames, REALM !="AN")
realmnames$realmno <- c(1:3, 5, 4,6, 7)
realmnames <- arrange(realmnames, realmno)


# add a data.frame
values(realms) <- realmnames


## gedi stuff----
path2 <- "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/GEDI_L3_LandSurface_Metrics_V2_1952/data/"

rh100 <- rast(x= paste0(path2, "GEDI03_rh100_mean_2019108_2021104_002_02", ".tif"))

rh_coarse <- raster::resample(rh100, dr_hist, method='max')
plot(rh_coarse)

rh_coarse <- crop(rh_coarse, e)

## rasterize wwf layers to match
b_rast <- rasterize(fbiomes, rh_coarse, "BIOME")
r_rast <- rasterize(realms, rh_coarse, "realmno", getCover=T)
plot(r_rast)


height_r <- c(rh_coarse, b_rast, r_rast)

height <- as.data.frame(height_r, xy=T) %>% 
  rename(height=GEDI03_rh100_mean_2019108_2021104_002_02) %>% 
  left_join(biomenames) %>% 
  left_join(realmnames)

## Select the 90 percentile pixels globally----
height90 <- height %>% 
  group_by(BIOME) %>% 
  mutate(q90=quantile(height, .90)) %>% 
  filter(height>=q90) %>% 
  dplyr::select(-q90)

# height_quant <- height %>% 
#   mutate(decile_rank = ntile(height,10),
#          BIOME=b,
#          name=biomenames$name[which(biomenames$BIOME==b)]) 
# 
# height_quant2 <- height_quant %>% 
#   mutate(percentile = ifelse(decile_rank==10, ">90th",
#                              ifelse(decile_rank==9, "80-90th", "<80th")),
#          percentile=fct_relevel(percentile, "<80th", "80-90th", ">90th")) %>%
#   dplyr::select(x, y, percentile)

world <- ne_coastline(scale = "medium", returnclass = "sf")
class(world)

## Select the 80/90 percentile pixels within biomes----
pdf('/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/output/biome_maps.pdf')
for(b in forests$BIOME){
  biome_layer <- subset(biomes, biomes$BIOME==b)
  b_height <- terra::mask(rh_coarse, biome_layer, updatevalue=NA)
  for(r in realmnames$REALM) {
    realm_layer <- subset(realms, realms$REALM==r)
    r_height <- terra::mask(b_height, realm_layer, updatevalue=NA)

    r_height <- as.data.frame(r_height, xy=T) %>%
      rename(height=GEDI03_rh100_mean_2019108_2021104_002_02) %>%
      mutate(REALM=r,
             realmname=realmnames$name[which(realmnames$REALM==r)])
    if(r=="AA") {
      height <- r_height
    } else {
      height <- bind_rows(height, r_height)
    }
  }


  height90 <- height %>%
    mutate(q90=quantile(height, .90),
           BIOME=b,
           name=biomenames$name[which(biomenames$BIOME==b)]) %>%
    filter(height>=q90) %>%
    dplyr::select(-q90)
  
  height80 <- height %>%
    mutate(q80=quantile(height, .80),
           BIOME=b,
           name=biomenames$name[which(biomenames$BIOME==b)]) %>%
    filter(height>=q80) %>%
    dplyr::select(-q80)

  height_quant <- height %>%
    mutate(decile_rank = ntile(height,10),
           BIOME=b,
           name=biomenames$name[which(biomenames$BIOME==b)])

  height_quant2 <- height_quant %>%
    mutate(percentile = ifelse(decile_rank==10, ">90th",
                               ifelse(decile_rank==9, "80-90th", "<80th")),
           percentile=fct_relevel(percentile, "<80th", "80-90th", ">90th")) %>%
    dplyr::select(x, y, BIOME, name, percentile)

  map <- ggplot(data = world) +
    geom_sf() +
    geom_tile(data=height_quant2, aes(x, y, fill=percentile)) +
    scale_fill_viridis(discrete=T, option='rocket', direction = -1)+
    theme_map() +
    ggtitle(forests$name[which(forests$BIOME==b)])

  print(map)

  if(b==1) {
    all_biome_height <- height90
    all_biome_height2 <- height80
    all_biome_dec <- height_quant
  } else {
    all_biome_height <- bind_rows(all_biome_height, height90)
    all_biome_height2 <- bind_rows(all_biome_height2, height80)
    all_biome_dec <- bind_rows(all_biome_dec, height_quant)
  }
}

dev.off()

all_biome_dec2 <- all_biome_dec %>% 
  mutate(percentile = ifelse(decile_rank==10, ">90th",
                             ifelse(decile_rank==9, "80-90th", "<80th")),
         percentile=fct_relevel(percentile, "<80th", "80-90th", ">90th"),
         eighty=ifelse(decile_rank>=9, ">80th", "<80th"),
         eighty=fct_relevel(eighty, "<80th", ">80th")) %>%
  # mutate(percentile = ifelse(decile_rank==10, 90, 
                             # ifelse(decile_rank==9, 80, 0))) %>%
  dplyr::select(x, y, percentile, eighty, name)

## ****Choose percentile threshold here ----
height90 <- all_biome_height2

height90r <- rast(height90[,1:3], crs="epsg:4326")
plot(height90r)

match_height <- height90 %>% 
  left_join(realmnames)


## plots and maps----

a<- ggplot(data = world) +
  geom_tile(data=all_biome_dec2, aes(x, y, fill=percentile)) +
  geom_sf() +
  scale_fill_viridis(discrete=T, option="rocket", direction = -1, begin = 0.3, end=1)+
  theme_map() +
  theme(legend.position = 'bottom')

a 

alt <- ggplot(data = world) +
  geom_tile(data=all_biome_dec2, aes(x, y, fill=name, alpha=eighty)) +
  geom_sf() +
  # scale_fill_viridis(discrete=T, option='rocket', direction = -1)+
  theme_map() +
  theme(legend.position = 'bottom')

alt

b <- ggplot(data = world) +
  geom_sf() +
  geom_tile(data=match_height, aes(x,y, fill=name)) +
  theme_map() +
  theme(legend.position = 'bottom')

b


ggplot(match_height, aes(height, fill=name, color=name)) +
  geom_density(alpha=0.4)+
  theme_test() +
  theme(legend.position = 'bottom', 
        legend.title = element_blank())

ggplot(match_height, aes(height, fill=realmname, color=realmname)) +
  geom_density(alpha=0.4)+
  theme_test() +
  facet_wrap(~name, scales="free_y") +
  theme(legend.position = 'bottom', 
        legend.title = element_blank())

ggplot(match_height, aes(height, fill=realmname, color=realmname)) +
  geom_histogram()+
  theme_test() +
  facet_wrap(~name, scales="free_x")

ggplot(match_height, aes(realmname, fill=name)) +
  geom_bar()+
  theme_test() +
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        axis.title.x=element_blank())

ggplot(match_height, aes(height, fill=name, color=name)) +
  geom_density(alpha=0.6) +
  theme_test() +
  theme(legend.position = 'bottom', legend.title = element_blank())


ggplot(data = world) +
  geom_sf() +
  geom_tile(data=height90, aes(x, y, fill=height)) +
  scale_fill_viridis(option = 'turbo')+
  theme_map()


## 2. Drought stuff----
## drought frequencies

extract_fun <- function(layer, group_lab){
  dr_sub <- crop(layer, ext(height90r)) %>% 
    terra::mask(height90r)
  
  dr_match <- terra::extract(dr_sub, fbiomes,  xy=T, exact=T,  na.rm=T) %>%
    na.omit()
  
  names(dr_match)[2:10] <- c("mo_AWI_CM", "mo_EC_Earth3", "mo_EC_Earth3_Veg", 
                                  "mo_GFDL_ESM4", "mo_INM_CM4_8", "mo_INM_CM5_0", 
                                  "mo_MPI_ESM1_2", "mo_MRI_ESM2_0", "mo_NorESM2_MM")
  
  dr_match2 <- dr_match %>% 
    rename(x=fraction, y=x, fraction=y) %>% 
    group_by(x,y) %>% 
    slice(which.max(fraction)) %>% 
    mutate(group=group_lab)
}

## get historic and all rcp
dr_hist_match <- extract_fun(dr_hist, 'historic')
dr_mid_match26 <- extract_fun(dr_mid26, 'mid rcp 2.6')
dr_late_match26 <- extract_fun(dr_late26, 'late rcp 2.6')
dr_mid_match85 <- extract_fun(dr_mid85, 'mid rcp 8.5')
dr_late_match85 <- extract_fun(dr_late85, 'late rcp 8.5')

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
  dplyr::select(c(ID, x, y, model, "hist_months"="months"))

## long table with historic comparison
drought_long2 <- filter(drought_long, group!="historic") %>% 
  left_join(historic) %>% 
  mutate(freq_dif=months-hist_months,
         time=word(group, 1),
         time = fct_relevel(time, "mid", "late"),
         rcp=as.factor(rcp))

## plots and maps----
c <- ggplot(data = world) +
  geom_sf() +
  geom_tile(data=filter(drought_long2, model=="med", group=="mid rcp 2.6"), aes(x,y, fill=freq_dif)) +
  scale_fill_viridis(option='rocket', direction = -1, limits=c(-3, 63))+
  theme_map() +
  theme(legend.position = 'right', legend.title = element_blank())+
  ggtitle("Mid century RCP 2.6") +
  theme(legend.position='none') +
  theme(plot.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=8))
c

d <- ggplot(data = world) +
  geom_sf() +
  geom_tile(data=filter(drought_long2, model=="med", group=="mid rcp 8.5"), aes(x,y, fill=freq_dif)) +
  scale_fill_viridis(option='rocket', direction = -1, limits=c(-3, 63))+
  theme_map() +
  theme(legend.position = 'right', legend.title = element_blank())+
  ggtitle("Mid century RCP 8.5") +
  theme(legend.position='none') +
  theme(plot.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=8))
d

e <- ggplot(data = world) +
  geom_sf() +
  geom_tile(data=filter(drought_long2, model=="med", group=="late rcp 2.6"), aes(x,y, fill=freq_dif)) +
  scale_fill_viridis(option='rocket', direction = -1, limits=c(-3, 63))+
  theme_map() +
  theme(legend.position = 'right', legend.title = element_blank())+
  ggtitle("Late century RCP 2.6") +
  theme(legend.position='none') +
  theme(plot.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=8))
e

f <- ggplot(data = world) +
  geom_sf() +
  geom_tile(data=filter(drought_long2, model=="med", group=="late rcp 8.5"), aes(x,y, fill=freq_dif)) +
  scale_fill_viridis(option='rocket', direction = -1, limits=c(-3, 63))+
  theme_map() +
  theme(legend.position = 'right', legend.title = element_blank())+
  ggtitle("Late century RCP 8.5") +
  # guides(fill=guide_legend(title="months/decade")) +
  theme(plot.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=8))

f

legend <- get_legend(f)

f <- f +
  theme(legend.position='none') 


plot_grid(plot_grid(c,d,e,f, ncol=2, align="hv", axis="tblr"), legend,
          rel_widths = c(1, 0.1))



g <- ggplot(data = world) +
  geom_sf() +
  geom_tile(data=filter(drought_long2, model=="med", group=="late rcp 8.5"), aes(x,y, fill=hist_months)) +
  scale_fill_viridis(option='mako', direction = -1)+
  theme_map() +
  theme(legend.position = 'right', legend.title = element_blank())+
  ggtitle("Historic drought average (months/decade)") +
  # guides(fill=guide_legend(title="months/decade")) +
  theme(plot.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=8))
g



a <- a + 
  theme(legend.position = 'right', legend.title = element_blank()) +
  ggtitle("GEDI height percentile") +
  # guides(fill=guide_legend(title="Percentile")) +
  theme(plot.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=8))
b <- b + 
  theme(legend.position = 'right', legend.title = element_blank())+
  ggtitle("WWF biome") +
  theme(plot.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=8))

alt <- alt + 
  theme(legend.position = 'right', legend.title = element_blank())+
  ggtitle("WWF biome") +
  guides(alpha=F) +
  theme(plot.title=element_text(size=12,face="bold"),
        legend.text = element_text(size=8))


plot_grid(a, b, g, ncol=1, align="hv", axis="tblr")
plot_grid(alt, g, ncol=1, align="hv", axis="tblr")


ggplot(drought_all, aes(group, fill=over10_mo_med)) +
  geom_bar(position='dodge')+
  scale_fill_viridis(discrete = T, option='plasma') +
  theme_test()

ggplot(filter(drought_all, rcp==2.6|is.na(rcp)), aes(mo_med, fill=group)) +
  geom_histogram(alpha=0.4, binwidth = 1, position = 'identity') +
  scale_fill_viridis(discrete = T, option='plasma') +
  xlab("Months per decade SPEI < -2") +
  theme_test()

ggplot(filter(drought_all, rcp==8.5|is.na(rcp)), aes(mo_med, fill=group)) +
  geom_histogram(alpha=0.4, binwidth = 1, position="identity") +
  scale_fill_viridis(discrete = T, option='plasma') +
  xlab("Months per decade SPEI < -2") +
  theme_test()

ggplot(drought_all, aes(mo_med, fill=group)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = T, option = 'plasma') +
  xlab("Months per decade SPEI < -2") +
  theme_test()

ggplot(drought_long2, aes(freq_dif, model, fill=group)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis(discrete = T, option = 'plasma') +
  xlab("Difference Months per decade SPEI < -2") +
  theme_test() +
  facet_wrap(~rcp) +
  xlim(c(-5, 50)) +
  geom_vline(xintercept = 0, linetype="dashed")

ggplot(filter(drought_long2, model=="med"), aes(freq_dif, fill=time)) +
  # geom_density(alpha=0.4) +
  geom_histogram(position="identity", alpha=0.4)+
  scale_fill_viridis(discrete = T) +
  xlab("Difference Months per decade SPEI < -2") +
  theme_test() +
  facet_wrap(~rcp, scales="free_x") +
  geom_vline(xintercept = 0, linetype="dashed")

ggplot(drought_long, aes(months, group, fill=model)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(geom = "point", 
               data=filter(drought_long, model=="med"), aes(months, group), 
               fun = "median", size=3) +
  scale_fill_viridis(discrete = T, option = 'plasma') +
  xlab("Months per decade SPEI < -2") +
  theme_test() +
  xlim(c(0,60))


test <- match_height %>% 
  group_by(name, realmname) %>% 
  tally()

drought_height <- match_height %>% 
  left_join(drought_long, by=c("x", "y"))

drought_height2 <- match_height %>% 
  left_join(drought_long2, by=c("x", "y")) %>% 
  group_by(name, realmname, group, model) %>% 
  add_tally()

drought_prop <- drought_height %>% 
  group_by(name, realmname, group, model) %>% 
  summarize(ntot=length(over10),
            nover10 = length(which(over10=="yes")), 
            prop_over10 = nover10/ntot)

ggplot(filter(drought_prop, model=="med"), aes(group, prop_over10, fill=group)) +
  geom_bar(stat='identity', position = 'dodge') +
  scale_fill_viridis(discrete = T, option = 'mako', direction=-1) +
  facet_grid(realmname~name, scales = 'free') +
  theme_test() +
  geom_text(mapping = aes(x = -Inf, y = Inf, label = ntot), 
            size    = 3,
            hjust   = -1.15,
            vjust   = 1.5) +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = 'bottom',
        legend.title=element_blank()) +
  ylab("Proportion of pixels >1 month/year SPEI < -2")

ggplot(filter(drought_height2, model=="med"), aes(rcp, freq_dif, fill=time)) +
  geom_bar(stat='identity', position = 'dodge') +
  scale_fill_viridis(discrete = T, option = 'mako', direction=-1) +
  facet_grid(realmname~name, scales = 'free') +
  theme_test() +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_text(mapping = aes(x = -Inf, y = Inf, label = n), 
            size    = 3,
            hjust   = -1.15,
            vjust   = 1.5) +
  theme(
        axis.title.x = element_blank(),
        legend.position = 'bottom',
        legend.title=element_blank()) +
  ylab("Proportion of pixels >1 month/year SPEI < -2")
  

ggplot(filter(filter(drought_height, model=="med"), over10=="yes"), aes(name, fill=group)) +
  geom_bar(position = 'dodge') +
  scale_fill_viridis(discrete = T, option = 'plasma') +
  facet_grid(realmname~name, scales = 'free') +
  theme_test()
  

ggplot(drought_height, aes(months, name, fill=group)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis(discrete = T) +
  xlab("Months per decade SPEI < -2") +
  theme_test() +
  facet_wrap(~model) +
  theme(axis.title.y = element_blank())

ggplot(filter(drought_height, model=="med"), aes(months, fill=REALM, color=REALM)) +
  geom_density(alpha=0.6) +
  scale_fill_viridis(discrete=T) +
  scale_color_viridis(discrete=T) +
  facet_wrap(~name, scales="free_y") +
  theme_test() +
  xlab("Months per decade SPEI < -2") +
  theme(legend.position = "bottom", legend.title=element_blank())

ggplot(filter(drought_height2, model=="med", group=="late rcp 8.5"), aes(freq_dif, fill=REALM, color=REALM)) +
  geom_histogram(alpha=0.6) +
  scale_fill_viridis(discrete=T) +
  scale_color_viridis(discrete=T) +
  facet_wrap(~name, scales="free_y") +
  theme_test() +
  xlab("Difference Months per decade SPEI < -2") +
  theme(legend.position = "bottom", legend.title=element_blank())

ggplot(filter(drought_height2, model=="med"), aes(freq_dif, name, fill=group)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis(discrete = T) +
  xlab("Months per decade SPEI < -2") +
  theme_test() +
  facet_wrap(~model) +
  theme(axis.title.y = element_blank()) +
  geom_vline(xintercept = 0, linetype='dashed')

ggplot(filter(drought_height2, model=="med"), aes(freq_dif, realmname, fill=group)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis(discrete = T) +
  xlab("Months per decade SPEI < -2") +
  theme_test() +
  facet_wrap(~model) +
  theme(axis.title.y = element_blank()) +
  geom_vline(xintercept = 0, linetype='dashed')

