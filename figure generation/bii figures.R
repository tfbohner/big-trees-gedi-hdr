library(tidyverse)
library(ggplot2)
library(ggstance)
library(readr)

## Mid-century----
ddf_wide <- read_delim("processed data/ddf_summary_wide.csv", ",", col_names = T) %>% 
  filter(!(name=="Mangroves"&realmname=="Nearctic"))
ddf_sum <- read_delim("processed data/ddf_summary.csv", ",", col_names = T) %>% 
  mutate(tallfor=ifelse(tallfor=="top10", "Tallest forests", "All other forests")) %>% 
  filter(!(name=="Mangroves"&realmname=="Nearctic"))

ddf_long <- read_delim("processed data/drought26_w_gedi.csv", ",", col_names = T)
hist_long <- read_delim("processed data/hist_w_gedi.csv", ",", col_names = T)

ddf_long <- ddf_long %>% 
  bind_cols(hist_long[,1:2])

ddf_drought <- ddf_long %>% 
  mutate(tallfor=ifelse(is.na(height90), "top90", "else")) %>% 
  group_by(biome, realmno, dr_freq_median, delta_dr_freq_median, max_severe_median, delta_max_severe_median, tallfor) %>% 
  summarize(tot_biomass=sum(mean_biomass, na.rm=T),
            mean_biomass=mean(mean_biomass, na.rm=T),
            mean_height=mean(height, na.rm=T))

biomenames <- read_delim("processed data/biomenames.csv", ",", col_names = T)
biomenames[,1] <- NULL
realmnames <- read_delim("processed data/realmnames.csv", ",", col_names = T)
realmnames[,1] <- NULL

ddf_long <- ddf_long %>% 
  filter(!is.na(biome)) 

# ddf_long <- ddf_long %>% 
#   left_join(biomenames, by = c("biome"="BIOME"))

# test <- ddf_long[1:2000,]
# 
# ggplot(test, aes(delta_dr_freq_median, weight=mean_biomass)) +
#   geom_histogram()+
#   ylab("Biomass")


tot_biomass_risk_long <- ddf_long %>% 
  filter(!is.na(mean_biomass)) %>%
  mutate(thresh_freq=ifelse(delta_dr_freq_median>0.5, "over", "under"),
         thresh_sev=ifelse((delta_max_severe_median*-1)>5, "over", "under"),
         tallfor=ifelse(is.na(height90), "top90", "else")) %>% 
  left_join(biomenames, by=c("biome"="BIOME"))

p2 <- c("#276845", "#ddb340", "#22bf92", "#468bc3", "#b85a3d", "#44498b", "#c88ebe") 
tot_biomass_risk_long <- tot_biomass_risk_long %>% 
  mutate(across(c(name), ~forcats::fct_relevel(., "Tropical Moist Broadleaf", "Tropical Dry Broadleaf", "Tropical Conifer", 
                                               "Temperate Broadleaf", "Temperate Conifer", "Mediterranean Forests", "Mangroves")))

ggplot(tot_biomass_risk_long,aes(x=delta_dr_freq_median, fill=tallfor)) +
  # geom_histogram(bins=20, position='identity', alpha=0.5) 
  geom_density(aes(y=..scaled..), adjust=8, alpha=0.3) 

ddf_long2 <- ddf_long %>% 
  mutate(tallfor=ifelse(is.na(height90), "top90", "else")) %>% 
  left_join(dplyr::select(biomenames, c(BIOME, name)), by=c("biome"="BIOME")) %>% 
  left_join(realmnames)

test <- ddf_long2 %>% 
  group_by(realmname, name, tallfor) %>% 
  tally()



library('broom')


test <- ddf_long2 %>% 
  filter(realmname!="Nearctic"&name!="Mangroves") %>% 
  group_by(realmname, name, tallfor) %>% 
  nest() %>% 
  spread(key = tallfor, value = data) %>% 
  mutate(
    t_test = map2(`else`, top90, ~{t.test(.x$delta_dr_freq_median, .y$delta_dr_freq_median) %>% tidy()}),
    `else` = map(`else`, nrow),
    top90 = map(top90, nrow)
  ) %>% 
  unnest()

test2 <- ddf_long2 %>% 
  filter(realmname!="Nearctic"&name!="Mangroves") %>% 
  group_by(realmname, name, tallfor) %>% 
  nest() %>% 
  spread(key = tallfor, value = data) %>% 
  mutate(
    t_test = map2(`else`, top90, ~{t.test(.x$delta_max_severe_median, .y$delta_max_severe_median) %>% tidy()}),
    `else` = map(`else`, nrow),
    top90 = map(top90, nrow)
  ) %>% 
  unnest()

freq_bar <- ggplot(tot_biomass_risk_long, aes(x=delta_dr_freq_median, weight=mean_biomass)) +
  geom_histogram(bins=20, alpha=0.8) +
  # geom_density(adjust=10)+
  theme_test() +
  geom_vline(xintercept = 0.5, linetype="dashed", color="red", linewidth=1.5) +
  ylab("Total Biomass") +
  xlab("Projected drought frequency change") +
  ggtitle("RCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.position = 'none')

freq_bar2 <- ggplot(tot_biomass_risk_long, aes(x=delta_dr_freq_median, weight=mean_biomass, fill=name)) +
  geom_histogram(bins=20, alpha=0.8) +
  scale_fill_manual(values=p2) +
  theme_test() +
  geom_vline(xintercept = 0.5, linetype="dashed", color="red", linewidth=1.5) +
  ylab("Total Biomass") +
  xlab("Projected drought frequency change") +
  ggtitle("RCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.position = 'none')

freq_bar3 <- ggplot(tot_biomass_risk_long, aes(x=delta_max_severe_median*-1, weight=mean_biomass)) +
  geom_histogram(bins=20, alpha=0.8) +
  # geom_density(adjust=10)+
  theme_test() +
  geom_vline(xintercept = 5, linetype="dashed", color="red", linewidth=1.5) +
  ylab("Total Biomass") +
  xlab("Projected drought severity change") +
  ggtitle("RCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.position = 'none')

freq_bar4 <- ggplot(tot_biomass_risk_long, aes(x=delta_max_severe_median*-1, weight=mean_biomass, fill=name)) +
  geom_histogram(bins=20, alpha=0.8) +
  scale_fill_manual(values=p2) +
  theme_test() +
  geom_vline(xintercept = 5, linetype="dashed", color="red", linewidth=1,5) +
  ylab("Total Biomass") +
  xlab("Projected drought severity change") +
  ggtitle("RCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.position = 'none')


pdf("output/potential long fig2s.pdf", width = 8, height=6)
print(freq_bar)
print(freq_bar2)
print(freq_bar3)
print(freq_bar4)
dev.off()

realmcol <- scales::viridis_pal()(7)


medit_df <- filter(tot_biomass_risk_long, biome==12) %>% 
  left_join(realmnames)

unique(medit_df$realmname)
pal <- realmcol[c(1,2,4,5,7)]

medit1 <- ggplot(medit_df,aes(x=delta_dr_freq_median, weight=mean_biomass, fill=realmname)) +
  geom_histogram(bins=20, position='identity', alpha=0.5) +
  scale_fill_manual(values=pal) +
  theme_test() +
  ylab("Total Biomass") +
  xlab("Projected drought frequency change") +
  ggtitle("Mediterranean Forests \nRCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.title = element_blank())

medit1d <- ggplot(medit_df,aes(x=delta_dr_freq_median, weight=mean_biomass, fill=realmname, color=realmname)) +
  geom_density(aes(y=..scaled..), adjust=6, alpha=0.3) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  theme_test() +
  ylab("Proportion biomass scaled by land area") +
  xlab("Projected drought frequency change") +
  ggtitle("Mediterranean Forests \nRCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.title = element_blank())
  

medit2 <- ggplot(medit_df,aes(x=delta_max_severe_median*-1, weight=mean_biomass, fill=realmname)) +
  geom_histogram(bins=20, position='identity', alpha=0.5) +
  scale_fill_manual(values=pal) +
  theme_test() +
  ylab("Total Biomass") +
  xlab("Projected drought severity change") +
  ggtitle("Mediterranean Forests \nRCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.title = element_blank())

medit2d <- ggplot(medit_df,aes(x=delta_max_severe_median*-1, weight=mean_biomass, fill=realmname, color=realmname)) +
  geom_density(aes(y=..scaled..), adjust=6, alpha=0.3) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  theme_test() +
  ylab("Proportion biomass scaled by land area") +
  xlab("Projected drought severity change") +
  ggtitle("Mediterranean Forests \nRCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.title = element_blank())


tmoist_df <- filter(tot_biomass_risk_long, biome==1) %>% 
  left_join(realmnames)

unique(tmoist_df$realmname)
pal <- realmcol[c(1:4,6,7)]

tmoist1 <- ggplot(tmoist_df,aes(x=delta_dr_freq_median, weight=mean_biomass, fill=realmname)) +
  geom_histogram(bins=20, position='identity', alpha=0.5) +
  scale_fill_manual(values=pal) +
  theme_test() +
  ylab("Total Biomass") +
  xlab("Projected drought frequency change") +
  ggtitle("Tropical Moist Broadleaf \nRCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.title = element_blank())

tmoist1d <- ggplot(tmoist_df,aes(x=delta_dr_freq_median, weight=mean_biomass, fill=realmname, color=realmname)) +
  geom_density(aes(y=..scaled..), adjust=6, alpha=0.3) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  theme_test() +
  ylab("Proportion biomass scaled by land area") +
  xlab("Projected drought frequency change") +
  ggtitle("Tropical Moist Broadleaf \nRCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.title = element_blank())


tmoist2 <- ggplot(tmoist_df,aes(x=delta_max_severe_median*-1, weight=mean_biomass, fill=realmname)) +
  geom_histogram(bins=20, position='identity', alpha=0.5) +
  scale_fill_manual(values=pal) +
  theme_test() +
  ylab("Total Biomass") +
  xlab("Projected drought severity change") +
  ggtitle("Tropical Moist Broadleaf \nRCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.title = element_blank())

tmoist2d <- ggplot(tmoist_df,aes(x=delta_max_severe_median*-1, weight=mean_biomass, fill=realmname, color=realmname)) +
  geom_density(aes(y=..scaled..), adjust=6, alpha=0.3) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  theme_test() +
  ylab("Proportion biomass scaled by land area") +
  xlab("Projected drought severity change") +
  ggtitle("Tropical Moist Broadleaf \nRCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.title = element_blank())

tdry_df <- filter(tot_biomass_risk_long, biome==2) %>% 
  left_join(realmnames)

unique(tdry_df$realmname)
pal <- realmcol[c(1:6)]

tdry1 <- ggplot(tdry_df,aes(x=delta_dr_freq_median, weight=mean_biomass, fill=realmname)) +
  geom_histogram(bins=20, position='identity', alpha=0.5) +
  scale_fill_manual(values=pal) +
  theme_test() +
  ylab("Total Biomass") +
  xlab("Projected drought frequency change") +
  ggtitle("Tropical Dry Broadleaf \nRCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.title = element_blank())

tdry1d <- ggplot(tdry_df,aes(x=delta_dr_freq_median, weight=mean_biomass, fill=realmname, color=realmname)) +
  geom_density(aes(y=..scaled..), adjust=6, alpha=0.3) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  theme_test() +
  ylab("Proportion biomass scaled by land area") +
  xlab("Projected drought frequency change") +
  ggtitle("Tropical Dry Broadleaf \nRCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.title = element_blank())


tdry2 <- ggplot(tdry_df,aes(x=delta_max_severe_median*-1, weight=mean_biomass, fill=realmname)) +
  geom_histogram(bins=20, position='identity', alpha=0.5) +
  scale_fill_manual(values=pal) +
  theme_test() +
  ylab("Total Biomass") +
  xlab("Projected drought severity change") +
  ggtitle("Tropical Dry Broadleaf \nRCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.title = element_blank())

tdry2d <- ggplot(tdry_df,aes(x=delta_max_severe_median*-1, weight=mean_biomass, fill=realmname, color=realmname)) +
  geom_density(aes(y=..scaled..), adjust=6, alpha=0.3) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  theme_test() +
  ylab("Proportion biomass scaled by land area") +
  xlab("Projected drought severity change") +
  ggtitle("Tropical Dry Broadleaf \nRCP 2.6 mid-century projections") +
  # theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.title = element_blank())

pdf("output/potential biome figs.pdf", width = 8, height=6)
print(medit1)
print(medit1d)
print(medit2)
print(medit2d)
print(tmoist1)
print(tmoist1d)
print(tmoist2)
print(tmoist2d)
print(tdry1)
print(tdry1d)
print(tdry2)
print(tdry2d)
dev.off()


tot_biomass_risk <- ddf_long %>% 
  filter(!is.na(biome), !is.na(mean_biomass)) %>%
  mutate(thresh=ifelse(delta_dr_freq_median>0.5, "over", "under"),
         tallfor=ifelse(is.na(height90), "top90", "else")) %>% 
  group_by(biome, realmno, thresh) %>% 
  summarize(thresh_biomass=sum(mean_biomass),
            n_tall=length(which(tallfor=="top90")),
            n_trees=length(tallfor))



tot_biomass <- ddf_long %>% 
  filter(!is.na(biome), !is.na(mean_biomass)) %>%
  group_by(biome, realmno) %>% 
  summarize(tot_biomass=sum(mean_biomass))

tot_biomass_risk2 <- tot_biomass_risk %>% 
  left_join(tot_biomass) %>% 
  filter(!(biome==14&realmno==5)) %>% 
  mutate(frac_biomass=thresh_biomass/tot_biomass,
         frac_tall=n_tall/n_trees)

tot_biomass_risk3 <- tot_biomass_risk2 %>% 
  group_by(thresh) %>% 
  summarize(thresh_biomass= sum(thresh_biomass),
            tot_biomass = sum(tot_biomass),
            n_tall=sum(n_tall),
            n_trees=sum(n_trees))%>% 
  mutate(frac_biomass=thresh_biomass/tot_biomass,
         frac_tall=n_tall/n_trees)
  

ddf_tot <- ddf_long %>%
  filter(!is.na(biome)) %>%
  mutate(tallfor=ifelse(is.na(height90), "Tallest forests", "All other forests")) %>% 
  ungroup() %>% 
  group_by(biome, realmno, tallfor) %>% 
  summarize(biomass=sum(mean_biomass, na.rm=T),
            area=length(height),
            height=median(height, na.rm=T),
            delta_freq=median(delta_dr_freq_median),
            delta_sev=median(delta_max_severe_median)) %>% 
  left_join(biomenames, by=c("biome"="BIOME"))  %>% 
  left_join(realmnames, by="realmno")

ddf_tot <- ddf_tot %>% 
  filter(!(name=="Mangroves"&realmname=="Nearctic")) 

ddf_long <- ddf_long %>% 
  left_join(realmnames) %>% 
  left_join(biomenames, by=c("biome"="BIOME")) %>% 
  filter(!(name=="Mangroves"&realmname=="Nearctic")) 

ddf_long <- ddf_long %>% 
  mutate(tallfor=ifelse(is.na(height90), "Tallest forests", "All other forests")) %>% 
  dplyr::select(-c(biome, realmno, longname, height90)) 

ddf_long <- ddf_long %>% 
  filter(!is.na(name)) %>% 
  mutate(across(c(name), ~forcats::fct_relevel(., "Tropical Moist Broadleaf", "Tropical Dry Broadleaf", "Tropical Conifer", 
                                               "Temperate Broadleaf", "Temperate Conifer", "Mediterranean Forests", "Mangroves")))
ddf_quant <- ddf_long %>% 
  group_by(name, realmname, tallfor) %>% 
  summarize(freq25=quantile(delta_dr_freq_median, probs=0.25),
            freq50=quantile(delta_dr_freq_median, probs=0.50),
            freq75=quantile(delta_dr_freq_median, probs=0.75))

ddf_wide2 <- ddf_tot %>%
  mutate(tallfor = ifelse(tallfor=="Tallest forests", "top10", "else")) %>% 
  pivot_wider(names_from=tallfor, values_from=c(delta_freq, delta_sev, biomass, height, area)) %>% 
  mutate(delta_freq_diff=delta_freq_top10-delta_freq_else,
         delta_sev_diff=delta_sev_top10-delta_sev_else,
         height_diff=height_top10-height_else,
         biomass_diff=biomass_top10-biomass_else)

p2 <- c("#276845", "#ddb340", "#22bf92", "#468bc3", "#b85a3d", "#44498b", "#c88ebe") 
ddf_wide <- ddf_wide %>% 
  filter(!is.na(name)) %>% 
  mutate(across(c(name), ~forcats::fct_relevel(., "Tropical Moist Broadleaf", "Tropical Dry Broadleaf", "Tropical Conifer", 
                                               "Temperate Broadleaf", "Temperate Conifer", "Mediterranean Forests", "Mangroves"))) %>%  
  mutate(freq_sign=ifelse(delta_freq_diff<0, "negative", "positive")) 

ddf_wide2 <- ddf_wide2 %>% 
  filter(!is.na(name)) %>% 
  mutate(across(c(name), ~forcats::fct_relevel(., "Tropical Moist Broadleaf", "Tropical Dry Broadleaf", "Tropical Conifer", 
                                               "Temperate Broadleaf", "Temperate Conifer", "Mediterranean Forests", "Mangroves"))) %>%  
  mutate(freq_sign=ifelse(delta_freq_diff<0, "negative", "positive")) 

ddf_sum <- ddf_sum %>% 
  filter(!is.na(name)) %>% 
  mutate(across(c(name), ~forcats::fct_relevel(., "Tropical Moist Broadleaf", "Tropical Dry Broadleaf", "Tropical Conifer", 
                                               "Temperate Broadleaf", "Temperate Conifer", "Mediterranean Forests", "Mangroves")))
pr2 <- ggplot(ddf_quant, aes(name, freq50, group=realmname, color=name, alpha=tallfor)) +
  geom_pointrange(aes(ymin=freq25, ymax=freq75),
                  position=position_dodge2(width = 0.9, preserve = "single")) +
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_text(data=filter(ddf_quant, tallfor=="Tallest forests"), 
            aes(name, freq50, label=realmname, group=realmname), 
            position=position_dodge2(width = 0.9, preserve = "single"), 
            hjust=-0.2, angle=90) +
  scale_color_manual(values=p2) +
  scale_alpha_manual(values = c(0.5, 1))+
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  guides(alpha="none", color="none", fill="none") +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  ylim(c(-0.5, 2.2)) +
  theme(legend.title=element_blank()) +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt"))

pr3 <- ggplot(ddf_quant, aes(name, freq50, group=realmname, color=name, alpha=tallfor)) +
  geom_pointrange(aes(ymin=freq25, ymax=freq75),
                  position=position_dodge2(width = 0.9, preserve = "single")) +
  geom_hline(yintercept = 0, linetype="dashed")+
  scale_color_manual(values=p2) +
  scale_alpha_manual(values = c(0.3, 1))+
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  guides(alpha="none", color="none", fill="none") +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  ylim(c(-0.5, 2.2)) +
  theme(legend.title=element_blank()) +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt"))

long1 <- ggplot(ddf_long, aes(name, delta_dr_freq_median, fill=name, 
                              group=interaction(tallfor, realmname, name), alpha=tallfor)) +
  geom_boxplot(position=position_dodge2(width = 0.9, preserve = "single"), outlier.shape=NA) +
  scale_fill_manual(values=p2) +
  scale_alpha_discrete(c(0.5,1)) + 
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt"))

long2 <- ggplot(ddf_long, aes(name, delta_dr_freq_median, fill=name, 
                              group=interaction(tallfor, realmname, name), alpha=tallfor)) +
  geom_boxplot(position=position_dodge2(width = 0.9, preserve = "single"), outlier.shape = NA) +
  geom_text(data=filter(ddf_sum, scenario=="rcp 2.6", tallfor=="Tallest forests"), 
            aes(name, delta_freq, label=realmname, group=interaction(realmname, name)), 
            position=position_dodge2(width = 0.9, preserve = "single"), vjust=0.5, hjust=-0.8, angle=90) +
  scale_fill_manual(values=p2) +
  scale_alpha_discrete(c(0.5,1)) + 
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt"))


bptot <- ggplot(ddf_wide2, 
                aes(name, delta_freq_top10, group=realmname, fill=name)) +
  geom_linerange(aes(ymin=delta_freq_top10, ymax=delta_freq_else, color=freq_sign), 
                 position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(size=biomass_top10), shape=21, stroke=1.0, 
             position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(y=delta_freq_else, size=biomass_else), shape=21, stroke=1.0, 
             position=position_dodge2(width = 0.9, preserve = "single"),
             alpha=0.5, color="gray") +
  scale_size(range = c(1, 15), name="Total biomass") +
  # facet_wrap(~name) +
  scale_color_manual(values=c("gray", "black")) +
  scale_fill_manual(values=p2) +
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  guides(alpha="none", color="none", fill="none") +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt"))

bptot2 <- ggplot(ddf_wide2, 
                aes(name, delta_freq_top10, group=realmname, fill=name)) +
  geom_linerange(aes(ymin=delta_freq_top10, ymax=delta_freq_else, color=freq_sign), 
                 position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(size=biomass_top10), shape=21, stroke=1.0, 
             position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(y=delta_freq_else, size=biomass_else), shape=21, stroke=1.0, 
             position=position_dodge2(width = 0.9, preserve = "single"),
             alpha=0.5, color="gray") +
  geom_text(aes(name, delta_freq_top10, label=realmname, group=realmname), 
            position=position_dodge2(width = 0.9, preserve = "single"), vjust=0.5, hjust=-0.3, angle=90) +
  scale_size(range = c(1, 15), name="Total biomass") +
  # facet_wrap(~name) +
  scale_color_manual(values=c("gray", "black")) +
  scale_fill_manual(values=p2) +
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  ylim(c(-0.5, 2.2)) + 
  guides(alpha="none", color="none", fill="none") +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt")) +
  theme(legend.position = 'none')



bp2 <- ggplot(filter(ddf_wide, scenario=="rcp 2.6"), 
              aes(name, delta_freq_top10, group=realmname, fill=name)) +
  geom_linerange(aes(ymin=delta_freq_top10, ymax=delta_freq_else, color=freq_sign), 
                 position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(size=biomass_top10), shape=21, stroke=1.0, 
             position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(y=delta_freq_else, size=biomass_else), shape=21, stroke=1.0, 
             position=position_dodge2(width = 0.9, preserve = "single"),
             alpha=0.5, color="gray") +
  geom_hline(yintercept = 0, linetype='dashed', color='red')+
  scale_size(range = c(.1, 15), name="Biomass") +
  # facet_wrap(~name) +
  scale_color_manual(values=c("gray", "black")) +
  scale_fill_manual(values=p2) +
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  guides(alpha="none", color="none", fill="none") +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt"))

bp3 <- ggplot(filter(ddf_wide, scenario=="rcp 2.6"), 
              aes(name, delta_freq_top10, group=realmname, fill=name)) +
  geom_linerange(aes(ymin=delta_freq_top10, ymax=delta_freq_else, color=freq_sign), 
                 position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(size=biomass_top10), shape=21, stroke=1.0, 
             position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(y=delta_freq_else, size=biomass_else), shape=21, stroke=1.0, 
             position=position_dodge2(width = 0.9, preserve = "single"),
             alpha=0.5, color="gray") +
  geom_text(aes(name, delta_freq_top10, label=realmname, group=realmname), 
            position=position_dodge2(width = 0.9, preserve = "single"), vjust=0.5, hjust=-0.3, angle=90) +
  geom_hline(yintercept = 0, linetype='dashed', color='red')+
  scale_size(range = c(.1, 15), name="Biomass") +
  # facet_wrap(~name) +
  scale_color_manual(values=c("gray", "black")) +
  scale_fill_manual(values=p2) +
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylim(c(-0.5, 2.2)) +
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt"))

bp4 <- ggplot(filter(ddf_wide, scenario=="rcp 2.6"), 
              aes(name, delta_sev_top10*-1, group=realmname, fill=name)) +
  geom_linerange(aes(ymin=delta_sev_top10*-1, ymax=delta_sev_else*-1, color=freq_sign), 
                 position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(size=biomass_top10), shape=21, stroke=1.0, 
             position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(y=delta_sev_else*-1, size=biomass_else), shape=21, stroke=1.0, 
             position=position_dodge2(width = 0.9, preserve = "single"),
             alpha=0.5, color="gray") +
  geom_hline(yintercept = 0, linetype='dashed', color='red')+
  scale_size(range = c(.1, 15), name="Biomass") +
  # facet_wrap(~name) +
  scale_color_manual(values=c("gray", "black")) +
  scale_fill_manual(values=p2) +
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylab("Projected change in drought severity") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  guides(alpha="none", color="none", fill="none") +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt"))

bp5 <- ggplot(filter(ddf_wide, scenario=="rcp 2.6"), 
              aes(name, delta_sev_top10*-1, group=realmname, fill=name)) +
  geom_linerange(aes(ymin=delta_sev_top10*-1, ymax=delta_sev_else*-1, color=freq_sign), 
                 position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(size=biomass_top10), shape=21, stroke=1.0, 
             position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(y=delta_sev_else*-1, size=biomass_else), shape=21, stroke=1.0, 
             position=position_dodge2(width = 0.9, preserve = "single"),
             alpha=0.5, color="gray") +
  geom_text(aes(name, delta_sev_top10*-1, label=realmname, group=realmname), 
            position=position_dodge2(width = 0.9, preserve = "single"), vjust=0.5, hjust=-0.3, angle=90) +
  geom_hline(yintercept = 0, linetype='dashed', color='red')+
  scale_size(range = c(.1, 15), name="Biomass") +
  # facet_wrap(~name) +
  scale_color_manual(values=c("gray", "black")) +
  scale_fill_manual(values=p2) +
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylim(c(-5, 22)) +
  ylab("Projected change in drought severity") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt"))

points2 <- ggplot(filter(ddf_wide, scenario=="rcp 2.6"), aes(name, delta_freq_top10,  group=realmname, color=name)) +
  geom_linerange(aes(ymin=delta_freq_top10, ymax=delta_freq_else, alpha=freq_sign), 
                 position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(position = position_dodge2(width = 0.9, preserve = "single"), size=4) +
  geom_point(aes(y=delta_freq_else), position=position_dodge2(width = 0.9, preserve = "single"),
             alpha=0.5, size=4) +
  geom_text(aes(name, delta_freq_top10, label=realmname, group=realmname), 
            position=position_dodge2(width = 0.9, preserve = "single"), vjust=-0.5, angle=90) +
  scale_color_manual(values=p2) +
  scale_alpha_manual(values = c(0.3, 1))+
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  guides(alpha="none", color="none", fill="none") +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  ylim(c(-0.5, 2.2)) +
  theme(legend.title=element_blank()) +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt"))

points3 <- ggplot(filter(ddf_wide, scenario=="rcp 2.6"), aes(name, delta_freq_top10,  group=realmname, color=name)) +
  geom_linerange(aes(ymin=delta_freq_top10, ymax=delta_freq_else, alpha=freq_sign), 
                 position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(position = position_dodge2(width = 0.9, preserve = "single"), size=4) +
  geom_point(aes(y=delta_freq_else), position=position_dodge2(width = 0.9, preserve = "single"),
             alpha=0.5, size=4) +
  scale_color_manual(values=p2) +
  scale_alpha_manual(values = c(0.3, 1))+
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(axis.title.x = element_blank()) +
  theme(text = element_text(size=16)) +
  guides(alpha="none", color="none", fill="none") +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  ylim(c(-0.5, 2.2)) +
  theme(legend.title=element_blank()) +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt"))

bar1 <- ggplot(filter(ddf_sum, scenario=="rcp 2.6")) +
  geom_bar(aes(name, delta_freq, fill=name, group=interaction(tallfor, realmname),  
               alpha=tallfor), stat="identity", position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(data=filter(ddf_sum, scenario=="rcp 2.6", tallfor=="Tallest forests"), 
            aes(name, delta_freq, label=realmname, group=realmname), 
            position=position_dodge2(width = 0.9, preserve = "single"), hjust=0, angle=90) +
  scale_fill_manual(values=p2) +
  scale_alpha_manual(values = c(0.5, 1)) +
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(text = element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  ylim(c(-0.5, 2.2)) +
  theme(legend.title=element_blank()) +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt"))

bar2 <- ggplot(filter(ddf_sum, scenario=="rcp 2.6")) +
  geom_bar(aes(name, delta_freq, fill=name, group=interaction(tallfor, realmname),  
               alpha=tallfor), stat="identity", position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(data=filter(ddf_sum, scenario=="rcp 2.6", tallfor=="Tallest forests"), 
            aes(name, delta_freq, label=realmname, group=realmname), 
            position=position_dodge2(width = 0.9, preserve = "single"), hjust=0, angle=90) +
  scale_fill_manual(values=p2) +
  scale_alpha_manual(values = c(0.5, 1)) +
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(text = element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  ylim(c(-0.5, 2.2)) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt"))

bar3 <- ggplot(filter(ddf_sum, scenario=="rcp 2.6")) +
  geom_bar(aes(name, delta_freq, fill=name, group=interaction(tallfor, realmname),  
               alpha=tallfor), stat="identity", position = position_dodge2(width = 0.9, preserve = "single")) +
  scale_fill_manual(values=p2) +
  scale_alpha_manual(values = c(0.5, 1)) +
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ) 
  ) +
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(text = element_text(size=16)) +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.8, hjust=0.8),
        axis.title.x = element_blank()) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 40, unit = "pt"))

pdf("output/potential long figs.pdf", width = 8, height=6)
print(long1)
dev.off()



pdf("output/potential figs.pdf", width = 8, height=6)
print(pr2)
print(pr3)
print(bptot)
print(bptot2)
print(bp2)
print(bp3)
print(bp4)
print(bp5)
print(points2)
print(points3)
print(bar1)
print(bar2)
print(bar3)

dev.off()

pdf("/Users/teresabohner/Google Drive/Shared drives/GEDI_HDR/tall trees figures:maps/potential bii figs.pdf", width = 8, height=6)
print(pr2)
print(pr3)
print(bptot)
print(bptot2)
print(bp2)
print(bp3)
print(bp4)
print(bp5)
print(points2)
print(points3)
print(bar1)
print(bar2)
print(bar3)

dev.off()


biome_level <- ddf_sum %>% 
  group_by(name, scenario) %>% 
  summarize(freq=mean(delta_freq))

biome_realm <- ddf_sum %>% 
  group_by(name, realmname, scenario) %>% 
  summarize(freq=mean(delta_freq))

tall_level <- ddf_sum %>% 
  group_by(tallfor, scenario) %>% 
  summarize(freq=mean(delta_freq))

## Late-century----
