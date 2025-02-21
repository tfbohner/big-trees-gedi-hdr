library(tidyverse)
library(ggplot2)
library(ggstance)
library(readr)

## This script produces summary tables for drought metrics in each biome/realm for historic and future scenarios

hist_long <- read_delim("processed data/hist_w_gedi.csv", ",", col_names = T)
realmnames <- read_csv("processed data/realmnames.csv")[,2:4]
biomenames <- read_csv("processed data/biomenames.csv")[,2:4]

mean_biome_realm_table <- hist_long %>% 
  group_by(biome, realmno) %>% 
  mutate(max_severe_median= (-1)*max_severe_median) %>% 
  summarize_at(.vars = c("dr_freq_median", "max_severe_median"), mean, na.rm=T) %>% 
  rename_at(vars(dr_freq_median:max_severe_median), ~paste0("mean_", .))

median_biome_realm_table <- hist_long %>% 
  group_by(biome, realmno) %>% 
  mutate(max_severe_median= (-1)*max_severe_median) %>% 
  summarize_at(.vars = c("dr_freq_median", "max_severe_median"), median, na.rm=T) %>% 
  rename_at(vars(dr_freq_median:max_severe_median), ~paste0("median_", .))

sd_biome_realm_table <- hist_long %>% 
  group_by(biome, realmno) %>% 
  mutate(max_severe_median= (-1)*max_severe_median) %>% 
  summarize_at(.vars = c("dr_freq_median", 
                         "max_severe_median"), sd, na.rm=T) %>% 
  rename_at(vars(dr_freq_median:max_severe_median), ~paste0("sd_", .))

sum_biome_realm_table <- hist_long %>% 
  group_by(biome, realmno) %>% 
  summarize(tot_biomass=sum(mean_biomass, na.rm=T),
         ncell=length(mean_biomass))

sum_biome_realm_table2 <- hist_long %>% 
  group_by(biome, realmno) %>% 
  summarize(mean_agb=mean(mean_biomass, na.rm=T),
            ncell=length(mean_biomass),
            tot_biomass=mean_agb*ncell)

sum_biome_realm_table2$diff = sum_biome_realm_table2$tot_biomass - sum_biome_realm_table$tot_biomass


plot(sum_biome_realm_table$tot_biomass, sum_biome_realm_table2$tot_biomass)
abline(0,1)


drought_thresholds <- left_join(mean_biome_realm_table, sd_biome_realm_table) %>% 
  left_join(sum_biome_realm_table) %>% 
  left_join(realmnames) %>% 
  left_join(biomenames, by=c("biome"="BIOME")) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  mutate(freq1=mean_dr_freq_median + sd_dr_freq_median,
         freq2=mean_dr_freq_median + 2*sd_dr_freq_median,
         freq3=mean_dr_freq_median + 3*sd_dr_freq_median,
         sev1=mean_max_severe_median - sd_max_severe_median,
         sev2=mean_max_severe_median - 2*sd_max_severe_median,
         sev3=mean_max_severe_median - 3*sd_max_severe_median)

ggplot(hist_long, aes(dr_freq_median)) +
  geom_density(adjust=5)

hist90 <- filter(hist_long, !is.na(height90))

ggplot(hist90, aes(dr_freq_median, fill=as.factor(biome))) +
  geom_boxplot()

ggplot(hist90, aes(max_severe_median, fill=as.factor(biome))) +
  geom_boxplot()

  
  



for(i in c("drought26", "drought26_late", "drought85", "drought85_late")) {
  ddf_long <- read_delim(paste0("processed data/",i,"_w_gedi.csv"), ",", col_names = T)
  
  ddf_long <- ddf_long %>% 
    bind_cols(hist_long[,1:2]) %>% 
    mutate(tallfor=ifelse(is.na(height90), "top90", "else"),
           novel_drought=ifelse(max_severe_median==0, "yes", "no"),
           freq_percent=delta_dr_freq_median/dr_freq_median*100,
           sev_percent=delta_max_severe_median/max_severe_median*100) %>% 
    left_join(biomenames, by=c("biome"="BIOME")) %>% 
    left_join(realmnames)
  
  ddf_drought <- ddf_long %>% 
    group_by(name, realmname, dr_freq_median, delta_dr_freq_median, freq_percent, 
             max_severe_median, delta_max_severe_median, sev_percent,
             novel_drought, tallfor) %>% 
    summarize(ncells=length(mean_biomass),
              tot_biomass=sum(mean_biomass, na.rm=T),
              mean_biomass=mean(mean_biomass, na.rm=T),
              mean_height=mean(height, na.rm=T)) 
  
  mean_biome_realm_table <- ddf_long %>% 
    group_by(name, realmname, tallfor, novel_drought) %>% 
    summarize_at(.vars = c("dr_freq_median", "delta_dr_freq_median", "freq_percent", 
                           "max_severe_median", "delta_max_severe_median", "sev_percent",
                           "mean_biomass", "height"), mean, na.rm=T) %>% 
    rename_at(vars(dr_freq_median:height), ~paste0("mean_", .))
  
  sd_biome_realm_table <- ddf_long %>% 
    group_by(name, realmname, tallfor, novel_drought) %>% 
    summarize_at(.vars = c("dr_freq_median", "delta_dr_freq_median", "freq_percent", 
                           "max_severe_median", "delta_max_severe_median", "sev_percent",
                           "mean_biomass", "height"), sd, na.rm=T) %>% 
    rename_at(vars(dr_freq_median:height), ~paste0("sd_", .))
  
  sum_biome_realm_table <- ddf_drought %>% 
    group_by(name, realmname, tallfor, novel_drought) %>% 
    summarize_at(.vars = c("tot_biomass", "ncells"), sum, na.rm=T)
  
  biome_realm_table <- left_join(mean_biome_realm_table, sd_biome_realm_table) %>% 
    left_join(sum_biome_realm_table) %>% 
    mutate_if(is.numeric, round, 3)

  write_csv(biome_realm_table, paste0("processed data/biome_realm_summary_", i, ".csv"))
  write_csv(ddf_drought, paste0("processed data/drought_res_summary_", i, ".csv"))
}

for(i in c("drought26", "drought85")) {
  ddf_long <- read_delim(paste0("processed data/",i,"_w_gedi.csv"), ",", col_names = T)
  
  ddf_late <- read_delim(paste0("processed data/",i,"_late_w_gedi.csv"), ",", col_names = T)
  
  ddf_long <- ddf_long %>% 
    bind_cols(hist_long[,1:2]) %>% 
    mutate(tallfor=ifelse(is.na(height90), "top90", "else"),
           novel_drought=ifelse(max_severe_median==0, "yes", "no"),
           freq_percent=delta_dr_freq_median/dr_freq_median*100,
           sev_percent=delta_max_severe_median/max_severe_median*100) %>% 
    left_join(biomenames, by=c("biome"="BIOME")) %>% 
    left_join(realmnames)
  
  ddf_late <- ddf_late %>% 
    bind_cols(hist_long[,1:2]) %>% 
    mutate(freq_percent=delta_dr_freq_median/dr_freq_median*100,
           sev_percent=delta_max_severe_median/max_severe_median*100) %>% 
    rename_at(c("delta_dr_freq_median", "freq_percent", "delta_max_severe_median", "sev_percent"), list( ~paste0(., "_late") ))
  
  ddf_long2 <- ddf_long %>% 
    rename_at(c("delta_dr_freq_median", "freq_percent", "delta_max_severe_median", "sev_percent"), list( ~paste0(., "_mid") )) %>% 
    bind_cols(ddf_late[,c("delta_dr_freq_median_late", "freq_percent_late", "delta_max_severe_median_late", "sev_percent_late")]) %>% 
    mutate(freq_percent_diff=freq_percent_late-freq_percent_mid,
           sev_percent_diff=sev_percent_late-sev_percent_mid,
           freq_diff=delta_dr_freq_median_late-delta_dr_freq_median_mid,
           sev_diff=delta_max_severe_median_late-delta_max_severe_median_mid)
  
  
  
  ddf_drought_diff <- ddf_long2 %>% 
    group_by(name, realmname, dr_freq_median, freq_diff, freq_percent_diff, 
             max_severe_median, sev_diff, sev_percent_diff,
             novel_drought, tallfor) %>% 
    summarize(ncells=length(mean_biomass),
              tot_biomass=sum(mean_biomass, na.rm=T),
              mean_biomass=mean(mean_biomass, na.rm=T),
              mean_height=mean(height, na.rm=T)) 

  write_csv(ddf_drought_diff, paste0("processed data/drought_diff_summary_", i, ".csv"))
}

for(i in c("drought26", "drought26_late", "drought85", "drought85_late")) {
  summary <- read_csv(paste0("processed data/biome_realm_summary_", i, ".csv")) %>% 
    mutate(scenario=i)
  ddf_drought <- read_csv(paste0("processed data/drought_res_summary_", i, ".csv")) %>% 
    mutate(scenario=i)
  if(i=="drought26"){
    summary2 <- summary
    ddf_drought2 <- ddf_drought
  } else{
    summary2 <- bind_rows(summary2, summary)
    ddf_drought2 <- bind_rows(ddf_drought2, ddf_drought)
  }
}


ddf_drought_mid <- ddf_drought_mid %>% 
  rename_at(c("delta_dr_freq_median", "freq_percent", "delta_max_severe_median", "sev_percent"), list( ~paste0(., "_mid") ))

ddf_drought_late <- ddf_drought_late %>% 
  rename_at(c("delta_dr_freq_median", "freq_percent", "delta_max_severe_median", "sev_percent"), list( ~paste0(., "_late") ))

ddf_drought3 <- ddf_drought_mid %>% 
  bind_cols(ddf_drought_late[,c("delta_dr_freq_median_late", "freq_percent_late", "delta_max_severe_median_late", "sev_percent_late")]) %>% 
  ungroup() %>% 
  mutate(freq_percent_diff=freq_percent_late-freq_percent_mid,
         sev_percent_diff=sev_percent_late-sev_percent_mid)


ddf_drought2 <- ddf_drought2 %>% 
  mutate(time=ifelse(str_detect(scenario, "late")==T, "late", "mid"),
         scenario2=ifelse(str_detect(scenario, "26")==T, "rcp 2.6", "rcp 8.5")) %>% 
  filter(!(name=="Mangroves"&realmname=="Nearctic"))

summary2 <- summary2 %>% 
  mutate(time=ifelse(str_detect(scenario, "late")==T, "late", "mid"),
         scenario2=ifelse(str_detect(scenario, "26")==T, "rcp 2.6", "rcp 8.5")) %>% 
  filter(!(name=="Mangroves"&realmname=="Nearctic"))

### z scores----
ddf_zscore <- ddf_drought2 %>% 
  mutate(freq_future=dr_freq_median + delta_dr_freq_median,
         sev_future=(-1)*max_severe_median + (-1)*delta_max_severe_median) %>% 
  left_join(dplyr::select(drought_thresholds, name, realmname, mean_dr_freq_median, sd_dr_freq_median, mean_max_severe_median, sd_max_severe_median)) %>% 
  mutate(freq_score = (freq_future - mean_dr_freq_median)/sd_dr_freq_median,
         sev_score = (sev_future - mean_max_severe_median)/sd_max_severe_median)
scen.labs <- c("RCP 2.6", "RCP 8.5")
names(scen.labs) <- c("rcp 2.6", "rcp 8.5")

ddf_zscore$time2 <- ifelse(ddf_zscore$time=="mid", "2030 - 2065", "2065 - 2100")
ddf_zscore$time <- factor(ddf_zscore$time, levels = c("mid", "late"))

p_freq<- ggplot(ddf_zscore, aes(x=freq_score, weight=tot_biomass, fill=time)) +
  geom_histogram(alpha=0.5, position = "identity") +
  facet_wrap(~scenario2, scales="free_x", ncol=1, 
             labeller = labeller(scenario2=scen.labs)) +
  scale_fill_discrete(labels=c('Mid-century', 'Late-century')) +
  theme_test() +
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_line(color='gray', linewidth = 0.2))  +
  xlab("Drought frequency change (Z-score)") +
  ylab(expression(paste("Total Biomass (Mg ", ha^-1, ")"))) +
  # scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))+
  labs(fill = "") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust=0, size=10),
    axis.title = element_text(size=12)
  ) +
  theme(legend.position = "none", axis.title.y=element_blank())+
  geom_vline(xintercept = c(2, 5, 10), linetype="dashed", linewidth=0.3)

a <- ggplot(filter(ddf_zscore, scenario2=="rcp 2.6"), aes(x=freq_score, weight=tot_biomass, fill=time)) +
  geom_histogram(alpha=0.5, position = "identity") +
  scale_fill_discrete(labels=c('Mid-century', 'Late-century')) +
  theme_test() +
  ggtitle("RCP 2.6") +
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_line(color='gray', linewidth = 0.2))  +
  xlab("Drought frequency change (Z-score)") +
  ylab(expression(paste("Total Biomass (Mg ", ha^-1, ")"))) +
  # scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))+
  labs(fill = "") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust=0, size=10),
    axis.title = element_text(size=12),
    axis.title.x = element_blank(),
    plot.title = element_text(size=12)
  ) +
  theme(legend.position = "none")+
  geom_vline(xintercept = c(2, 5, 10), linetype="dashed", linewidth=0.3)

c <- ggplot(filter(ddf_zscore, scenario2=="rcp 8.5"), aes(x=freq_score, weight=tot_biomass, fill=time)) +
  geom_histogram(alpha=0.5, position = "identity") +
  scale_fill_discrete(labels=c('Mid-century', 'Late-century')) +
  theme_test() +
  ggtitle("RCP 8.5") +
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_line(color='gray', linewidth = 0.2))  +
  xlab("Drought frequency change (Z-score)") +
  ylab(expression(paste("Total Biomass (Mg ", ha^-1, ")"))) +
  # scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))+
  labs(fill = "") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust=0, size=10),
    axis.title = element_text(size=12),
    plot.title = element_text(size=12)
  ) +
  theme(legend.position = "none")+
  geom_vline(xintercept = c(2, 5, 10), linetype="dashed", linewidth=0.3)


p_sev <- ggplot(filter(ddf_zscore, sev_score<75), aes(x=sev_score, weight=tot_biomass, fill=time)) +
  geom_histogram(alpha=0.5, position = "identity") +
  facet_wrap(~scenario2, scales="free_x", ncol=1, 
             labeller = labeller(scenario2=scen.labs)) +
  scale_fill_discrete(labels=c('Mid-century', 'Late-century')) +
  theme_test() +
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_line(color='gray', linewidth = 0.2))  +
  xlab("Drought severity change (Z-score)") +
  ylab(expression(paste("Total Biomass (Mg ", ha^-1, ")"))) +
  labs(fill = "") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust=0, size=10),
    axis.title = element_text(size=12)
  ) +
  geom_vline(xintercept = c(2, 5, 10), linetype="dashed", linewidth=0.3)

legend <- get_legend(p_sev + theme_minimal()) 
  

p_sev <- p_sev + theme(legend.position = "none", axis.title.y=element_blank())

b <- ggplot(filter(ddf_zscore, sev_score<75, scenario2=="rcp 2.6"), aes(x=sev_score, weight=tot_biomass, fill=time)) +
  geom_histogram(alpha=0.5, position = "identity") +
  ggtitle("RCP 2.6") +
  scale_fill_discrete(labels=c('Mid-century', 'Late-century')) +
  theme_test() +
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_line(color='gray', linewidth = 0.2))  +
  xlab("Drought severity change (Z-score)") +
  ylab("") +
  labs(fill = "") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust=0, size=10),
    axis.title = element_text(size=12),
    plot.title = element_text(size=12),
    legend.position = "none",
    axis.title.x = element_blank()
  ) +
  geom_vline(xintercept = c(2, 5, 10), linetype="dashed", linewidth=0.3)

d <- ggplot(filter(ddf_zscore, sev_score<75, scenario2=="rcp 8.5"), aes(x=sev_score, weight=tot_biomass, fill=time)) +
  geom_histogram(alpha=0.5, position = "identity") +
  ggtitle("RCP 8.5") +
  scale_fill_discrete(labels=c('Mid-century', 'Late-century')) +
  theme_test() +
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_line(color='gray', linewidth = 0.2))  +
  xlab("Drought severity change (Z-score)") +
  ylab("") +
  labs(fill = "") +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust=0, size=10),
    axis.title = element_text(size=12),
    plot.title = element_text(size=12),
    legend.position = "none"
  ) +
  geom_vline(xintercept = c(2, 5, 10), linetype="dashed", linewidth=0.3)

ylab <- ggdraw() +
  draw_label(expression(paste("Total Biomass (Mg ", ha^-1, ")")), angle=90, size=12, vjust=0.8) 

ggdraw() +
  draw_plot(plot_grid(ylab, p_freq, p_sev, nrow=1, rel_widths = c(0.1,1,1)), 0, 0, 1, 1) +
  draw_plot(legend, 0.8, 0.2, 0.18, 0.18)

pdf("output/zcore_fig.pdf")
print(ggdraw() +
  draw_plot(plot_grid(a, b, c, d, ncol=2, labels=c("a.", "b.", "c.", "d."), align = "vh"), 0, 0, 1, 1) +
  draw_plot(legend, 0.8, 0.16, 0.2, 0.2))
dev.off()

p_sev + annotate("text", label = "Test", size = 4, x = 0, y = 5)

tots <- ddf_zscore %>% 
  group_by(scenario, tallfor) %>% 
  summarize(total_biomass=sum(tot_biomass))

ddf_zscore2 <- ddf_zscore %>%
  mutate(total_biomass=sum(tot_biomass, na.rm=T), .by=c(scenario, tallfor)) 
  

## problems with too many rows...probably indexing issue

ddf_zcsore2 <- ddf_zscore %>% 
  group_by(scenario, tallfor) %>%
  summarize(total_biomass=sum(tot_biomass, na.rm=T),
            freq2 = sum(tot_biomass[freq_score>2], na.rm=T),
            freq2p = freq2/total_biomass,
            sev2 = sum(tot_biomass[sev_score>2], na.rm=T),
            sev2p = sev2/total_biomass,
          freq5 = sum(tot_biomass[freq_score>5], na.rm=T),
          freq5p = freq5/total_biomass,
          freq10 = sum(tot_biomass[freq_score>10], na.rm=T),
          freq10p = freq10/total_biomass,
          freq20 = sum(tot_biomass[freq_score>20], na.rm=T),
          freq20p = freq20/total_biomass,
          sev5 = sum(tot_biomass[sev_score>5], na.rm=T),
          sev5p = sev5/total_biomass,
          sev10 = sum(tot_biomass[sev_score>10], na.rm=T),
          sev10p = sev10/total_biomass,
          sev20 = sum(tot_biomass[sev_score>20], na.rm=T),
          sev20p = sev20/total_biomass)

ddf_zcsore2b <- ddf_zscore %>% 
  group_by(scenario, tallfor) %>%
  summarize(total_biomass=sum(tot_biomass, na.rm=T),
            freq2 = sum(tot_biomass[freq_score>2], na.rm=T),
            sev2 = sum(tot_biomass[sev_score>2], na.rm=T),
            freq10 = sum(tot_biomass[freq_score>10], na.rm=T),
            sev10 = sum(tot_biomass[sev_score>10], na.rm=T)) %>% 
  group_by(scenario) %>% 
  mutate(tot2f = sum(freq2),
         tot2s=sum(sev2),
         tot10f=sum(freq10),
         tot10s=sum(sev10)) %>% 
  group_by(scenario, tallfor) %>%
  mutate(freq2b = freq2/tot2f,
            sev2b = sev2/tot2s,
            freq10b = freq10/tot10f,
            sev10b = sev10/tot10s)

ddf_zscore3 <- ddf_zscore %>%
  group_by(scenario) %>% 
  summarize(total_biomass=sum(tot_biomass, na.rm=T),
            freq2 = sum(tot_biomass[freq_score>2], na.rm=T),
            freq2p = freq2/total_biomass,
            sev2 = sum(tot_biomass[sev_score>2], na.rm=T),
            sev2p = sev2/total_biomass,
            freq5 = sum(tot_biomass[freq_score>5], na.rm=T),
            freq5p = freq5/total_biomass,
            freq10 = sum(tot_biomass[freq_score>10], na.rm=T),
            freq10p = freq10/total_biomass,
            freq20 = sum(tot_biomass[freq_score>20], na.rm=T),
            freq20p = freq20/total_biomass,
            sev5 = sum(tot_biomass[sev_score>5], na.rm=T),
            sev5p = sev5/total_biomass,
            sev10 = sum(tot_biomass[sev_score>10], na.rm=T),
            sev10p = sev10/total_biomass,
            sev20 = sum(tot_biomass[sev_score>20], na.rm=T),
            sev20p = sev20/total_biomass)

ddf_zscore4 <- ddf_zscore %>%
  group_by(scenario, tallfor, biome) %>% 
  summarize(total_biomass=sum(tot_biomass, na.rm=T),
            freq2 = sum(tot_biomass[freq_score>2], na.rm=T),
            freq2p = freq2/total_biomass,
            sev2 = sum(tot_biomass[sev_score>2], na.rm=T),
            sev2p = sev2/total_biomass,
            freq5 = sum(tot_biomass[freq_score>5], na.rm=T),
            freq5p = freq5/total_biomass,
            freq10 = sum(tot_biomass[freq_score>10], na.rm=T),
            freq10p = freq10/total_biomass,
            freq20 = sum(tot_biomass[freq_score>20], na.rm=T),
            freq20p = freq20/total_biomass,
            sev5 = sum(tot_biomass[sev_score>5], na.rm=T),
            sev5p = sev5/total_biomass,
            sev10 = sum(tot_biomass[sev_score>10], na.rm=T),
            sev10p = sev10/total_biomass,
            sev20 = sum(tot_biomass[sev_score>20], na.rm=T),
            sev20p = sev20/total_biomass)

ddf_zscore5 <- ddf_zscore %>%
  group_by(scenario, biome) %>% 
  summarize(total_biomass=sum(tot_biomass, na.rm=T),
            freq2 = sum(tot_biomass[freq_score>2], na.rm=T),
            freq2p = freq2/total_biomass,
            sev2 = sum(tot_biomass[sev_score>2], na.rm=T),
            sev2p = sev2/total_biomass,
            freq5 = sum(tot_biomass[freq_score>5], na.rm=T),
            freq5p = freq5/total_biomass,
            freq10 = sum(tot_biomass[freq_score>10], na.rm=T),
            freq10p = freq10/total_biomass,
            freq20 = sum(tot_biomass[freq_score>20], na.rm=T),
            freq20p = freq20/total_biomass,
            sev5 = sum(tot_biomass[sev_score>5], na.rm=T),
            sev5p = sev5/total_biomass,
            sev10 = sum(tot_biomass[sev_score>10], na.rm=T),
            sev10p = sev10/total_biomass,
            sev20 = sum(tot_biomass[sev_score>20], na.rm=T),
            sev20p = sev20/total_biomass)


ggplot(ddf_drought2, aes(x=freq_percent, weight=ncells, fill=time)) +
  geom_histogram(alpha=0.5, position = "identity") +
  facet_wrap(~scenario2, scales="free_x")

ggplot(ddf_drought2, aes(x=sev_percent, weight=ncells)) +
  geom_histogram() +
  facet_grid(scenario2~time)

ggplot(filter(summary2, novel_drought=="no", tallfor=="top90"), aes(x=mean_freq_percent, y=interaction(realmname, name), color=name, alpha=time)) +
  scale_alpha_manual(values=c(1, 0.5)) +
  geom_point() +
  facet_wrap(~scenario2, scales="free_x") +
  theme_bw()

ggplot(filter(summary2, novel_drought=="no", tallfor=="top90"), aes(x=mean_sev_percent, y=interaction(realmname, name), color=name, alpha=time)) +
  scale_alpha_manual(values=c(1, 0.5)) +
  geom_point() +
  facet_wrap(~scenario2, scales="free_x") +
  theme_bw()

ggplot(filter(summary2, novel_drought=="no", tallfor=="top90"), aes(x=mean_delta_dr_freq_median, y=interaction(realmname, name), color=name, alpha=time)) +
  scale_alpha_manual(values=c(1, 0.5)) +
  geom_point() +
  facet_wrap(~scenario2, scales="free_x") +
  theme_bw()


### test
hist_long2 <- hist_long[,1:2] %>% 
  rename("hist_freq"="dr_freq_median", "hist_sev"="max_severe_median")

ddf_long <- ddf_long %>% 
  bind_cols(hist_long2) %>% 
  mutate(tallfor=ifelse(is.na(height90), "top90", "else"),
         novel_drought=ifelse(max_severe_median==0, "yes", "no")) %>% 
  left_join(biomenames, by=c("biome"="BIOME")) %>% 
  left_join(realmnames)

ddf_zscore2 <- ddf_long %>% 
  mutate(fut_freq=dr_freq_median,
         fut_sev=(-1)*max_severe_median,
         hist_sev = (-1)*fut_sev) %>% 
  left_join(dplyr::select(drought_thresholds, name, realmname, mean_dr_freq_median, sd_dr_freq_median, mean_max_severe_median, sd_max_severe_median)) %>% 
  mutate(freq_score = (fut_freq - mean_dr_freq_median)/sd_dr_freq_median,
         sev_score = (fut_sev - mean_max_severe_median)/sd_max_severe_median)
hist(ddf_zscore2$sev_score)
