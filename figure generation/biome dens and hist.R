library(tidyverse)
library(ggplot2)
library(ggstance)
library(readr)
library(scales)
library(cowplot)

ddf_long <- read_delim("processed data/drought26_w_gedi.csv", ",", col_names = T)
ddf_long2 <- read_delim("processed data/drought26_late_w_gedi.csv", ",", col_names = T)
ddf_long3 <- read_delim("processed data/drought85_w_gedi.csv", ",", col_names = T)
ddf_long4 <- read_delim("processed data/drought85_late_w_gedi.csv", ",", col_names = T)

biomenames <- read_delim("processed data/biomenames.csv", ",", col_names = T)
biomenames[,1] <- NULL
realmnames <- read_delim("processed data/realmnames.csv", ",", col_names = T)
realmnames[,1] <- NULL

ddf_long <- ddf_long %>% 
  filter(!is.na(biome), !is.na(mean_biomass)) %>%
  mutate(tallfor=ifelse(is.na(height90), "top90", "else"),
         delta_max_severe_median = delta_max_severe_median*(-1)) 
ddf_long2 <- ddf_long2 %>% 
  filter(!is.na(biome), !is.na(mean_biomass)) %>%
  mutate(tallfor=ifelse(is.na(height90), "top90", "else"),
         delta_max_severe_median = delta_max_severe_median*(-1)) 
ddf_long3 <- ddf_long3 %>% 
  filter(!is.na(biome), !is.na(mean_biomass)) %>%
  mutate(tallfor=ifelse(is.na(height90), "top90", "else"),
         delta_max_severe_median = delta_max_severe_median*(-1)) 
ddf_long4 <- ddf_long4 %>% 
  filter(!is.na(biome), !is.na(mean_biomass)) %>%
  mutate(tallfor=ifelse(is.na(height90), "top90", "else"),
         delta_max_severe_median = delta_max_severe_median*(-1)) 

ddf_long$tallfor <- ifelse(!is.na(ddf_long$height90), "top90", "else")

ggplot(ddf_long, aes(x=delta_dr_freq_median, fill=tallfor)) +
  geom_density(aes(y=..scaled..))
  ylab(expression(paste("Total Biomass (Mg ", ha^-1, ")"))) +
  xlab(paste0("Drought ", drought, " change")) +
  ggtitle(scenario) +
  theme_test() +
  theme( # remove the vertical grid lines
    panel.grid.major.y = element_line(color='gray', linewidth = 0.2))  +
  # scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))+
  theme(
    # axis.title.y = element_blank(),
    plot.title = element_text(hjust=0, size=10),
    axis.title = element_text(size=12)
  ) 


realm_hex <- hue_pal()(7)
realm_col <- realmnames %>% 
  arrange(realmname) %>% 
  mutate(hex=realm_hex)

scen.labs <- c("RCP 2.6", "RCP 8.5")
vars <- c("delta_dr_freq_median", "delta_max_severe_median")
names(vars) <- c("frequency", "severity")

plotfun <- function(data, drought, scenario){
  p1 <- ggplot(data,aes(x=!!sym(vars[drought]), weight=mean_biomass, fill=realmname)) +
    geom_histogram(bins=20, position='identity', alpha=0.5) +
    scale_fill_manual(values=realm_col$hex[realm_col$realmname%in%(unique(data$realmname))]) +
    ylab(expression(paste("Total Biomass (Mg ", ha^-1, ")"))) +
    xlab(paste0("Drought ", drought, " change")) +
    ggtitle(scenario) +
    theme_test() +
    theme( # remove the vertical grid lines
      panel.grid.major.y = element_line(color='gray', linewidth = 0.2))  +
    # scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))+
    theme(
      # axis.title.y = element_blank(),
      plot.title = element_text(hjust=0, size=10),
      axis.title = element_text(size=12)
    ) 
}



pdf("output/biome_hist_mid.pdf")
for(i in c(1:5, 12, 14) ){
  dat <- filter(ddf_long, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  dat3 <- filter(ddf_long3, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  p1 <- plotfun(dat, "frequency", "RCP 2.6") +
    theme(legend.position="none") +
    theme(axis.title.x = element_blank())
  p2 <- plotfun(dat, "severity", "RCP 2.6") +
    theme(legend.position="none")+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  p3 <- plotfun(dat3, "frequency", "RCP 8.5") +
    theme(legend.position="none") 
  p4 <- plotfun(dat3, "severity", "RCP 8.5") +
    theme(axis.title.y = element_blank())
  
  # get joint legend
  legend <- get_legend(p4 + 
                         theme_minimal() + 
                         theme(legend.title = element_blank(),
                               legend.position = "bottom"))
  
  p4 <- p4 +
    theme(legend.position="none")
  
  # make title 
  title <- ggdraw() +
    draw_label(paste0(biomenames$name[i], ", Mid-century"), 
               size=12,
               x = 0,
               hjust = 0) +
    theme(
      plot.margin = margin(0, 0, 0, 30)) 
  
  ## arrange things
  print(
    plot_grid(plot_grid(title, plot_grid(p1,p2,p3,p4, ncol=2, align='vh'), 
                        nrow=2, rel_heights = c(0.1,1,1)),
              legend, ncol=1, rel_heights = c(1,0.1))
  )
  
}

dev.off()

pdf("output/biome_hist_late.pdf")
for(i in c(1:5, 12, 14) ){
  dat <- filter(ddf_long2, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  dat3 <- filter(ddf_long4, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  p1 <- plotfun(dat, "frequency", "RCP 2.6") +
    theme(legend.position="none") +
    theme(axis.title.x = element_blank())
  p2 <- plotfun(dat, "severity", "RCP 2.6") +
    theme(legend.position="none")+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  p3 <- plotfun(dat3, "frequency", "RCP 8.5") +
    theme(legend.position="none") 
  p4 <- plotfun(dat3, "severity", "RCP 8.5") +
    theme(axis.title.y = element_blank())
  
  # get joint legend
  legend <- get_legend(p4 + 
                         theme_minimal() + 
                         theme(legend.title = element_blank(),
                               legend.position = "bottom"))
  
  p4 <- p4 +
    theme(legend.position="none")
  
  # make title 
  title <- ggdraw() +
    draw_label(paste0(biomenames$name[i], ", Late-century"), 
               size=12,
               x = 0,
               hjust = 0) +
    theme(
      plot.margin = margin(0, 0, 0, 30)) 
  
  ## arrange things
  print(
    plot_grid(plot_grid(title, plot_grid(p1,p2,p3,p4, ncol=2, align='vh'), 
                        nrow=2, rel_heights = c(0.1,1,1)),
              legend, ncol=1, rel_heights = c(1,0.1))
  )
  
}

dev.off()

### Denisty plots----
plotfun2 <- function(data, drought, scenario){
  p1 <- ggplot(data,aes(x=!!sym(vars[drought]), weight=mean_biomass, fill=realmname, color=realmname)) +
    geom_density(adjust=8, alpha=0.3) +
    # geom_histogram(bins = 20, position = "identity", alpha = 0.3,
    #                mapping = aes(y = stat(ncount))) +
    scale_fill_manual(values=realm_col$hex[realm_col$realmname%in%(unique(data$realmname))]) +
    scale_color_manual(values=realm_col$hex[realm_col$realmname%in%(unique(data$realmname))]) +
    ylab("Biomass density") +
    xlab(paste0("Drought ", drought, " change")) +
    ggtitle(scenario) +
    theme_test() +
    theme( # remove the vertical grid lines
      panel.grid.major.y = element_line(color='gray', linewidth = 0.2))  +
    # scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))+
    theme(
      # axis.title.y = element_blank(),
      plot.title = element_text(hjust=0, size=10),
      axis.title = element_text(size=12)
    ) 

  }


pdf("output/biome_dens_mid.pdf")
for(i in c(1:5, 12, 14) ){
  dat <- filter(ddf_long, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  dat3 <- filter(ddf_long3, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  p1 <- plotfun2(dat, "frequency", "RCP 2.6") +
    theme(legend.position="none") +
    theme(axis.title.x = element_blank())
  p2 <- plotfun2(dat, "severity", "RCP 2.6") +
    theme(legend.position="none")+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  p3 <- plotfun2(dat3, "frequency", "RCP 8.5") +
    theme(legend.position="none") 
  p4 <- plotfun2(dat3, "severity", "RCP 8.5") +
    theme(axis.title.y = element_blank())
  
  # get joint legend
  legend <- get_legend(p4 + 
                         theme_minimal() + 
                         theme(legend.title = element_blank(),
                               legend.position = "bottom"))
  
  p4 <- p4 +
    theme(legend.position="none")
  
  # make title 
  title <- ggdraw() +
    draw_label(paste0(biomenames$name[i], ", Mid-century"), 
               size=12,
               x = 0,
               hjust = 0) +
    theme(
      plot.margin = margin(0, 0, 0, 30)) 
  
  ## arrange things
  print(
    plot_grid(plot_grid(title, plot_grid(p1,p2,p3,p4, ncol=2, align='vh'), 
                        nrow=2, rel_heights = c(0.1,1,1)),
              legend, ncol=1, rel_heights = c(1,0.1))
  )
  
}

dev.off()

pdf("output/biome_dens_late.pdf")
for(i in c(1:5, 12, 14) ){
  dat <- filter(ddf_long2, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  dat3 <- filter(ddf_long4, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  p1 <- plotfun2(dat, "frequency", "RCP 2.6") +
    theme(legend.position="none") +
    theme(axis.title.x = element_blank())
  p2 <- plotfun2(dat, "severity", "RCP 2.6") +
    theme(legend.position="none")+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  p3 <- plotfun2(dat3, "frequency", "RCP 8.5") +
    theme(legend.position="none") 
  p4 <- plotfun2(dat3, "severity", "RCP 8.5") +
    theme(axis.title.y = element_blank())
  
  # get joint legend
  legend <- get_legend(p4 + 
                         theme_minimal() + 
                         theme(legend.title = element_blank(),
                               legend.position = "bottom"))
  
  p4 <- p4 +
    theme(legend.position="none")
  
  # make title 
  title <- ggdraw() +
    draw_label(paste0(biomenames$name[i], ", Late-century"), 
               size=12,
               x = 0,
               hjust = 0) +
    theme(
      plot.margin = margin(0, 0, 0, 30)) 
  
  ## arrange things
  print(
    plot_grid(plot_grid(title, plot_grid(p1,p2,p3,p4, ncol=2, align='vh'), 
                        nrow=2, rel_heights = c(0.1,1,1)),
              legend, ncol=1, rel_heights = c(1,0.1))
  )
  
}

dev.off()

### Combined plots----
plotfun <- function(data, drought, scenario){
  p1 <- ggplot(data,aes(x=!!sym(vars[drought]), weight=mean_biomass, fill=realmname)) +
    geom_histogram(bins=20, alpha=0.5) +
    scale_fill_manual(values=realm_col$hex[realm_col$realmname%in%(unique(data$realmname))]) +
    ylab(expression(paste("Total Biomass (Mg ", ha^-1, ")"))) +
    xlab(paste0("Drought ", drought, " change")) +
    ggtitle(scenario) +
    theme_test() +
    theme( # remove the vertical grid lines
      panel.grid.major.y = element_line(color='gray', linewidth = 0.2))  +
    # scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))+
    theme(
      # axis.title.y = element_blank(),
      plot.title = element_text(hjust=0, size=10),
      axis.title = element_text(size=12)
    ) 
}
plotfun3 <- function(data, drought, scenario){
  p1 <- ggplot(data,aes(x=!!sym(vars[drought]), weight=mean_biomass, fill=realmname, color=realmname)) +
    geom_density(adjust=8, alpha=0.3, position="fill") +
    # geom_histogram(bins = 20, position = "identity", alpha = 0.3,
    #                mapping = aes(y = stat(ncount))) +
    scale_fill_manual(values=realm_col$hex[realm_col$realmname%in%(unique(data$realmname))]) +
    scale_color_manual(values=realm_col$hex[realm_col$realmname%in%(unique(data$realmname))]) +
    ylab("Proportion biomass") +
    xlab(paste0("Drought ", drought, " change")) +
    ggtitle(scenario) +
    theme_test() +
    theme( # remove the vertical grid lines
      panel.grid.major.y = element_line(color='gray', linewidth = 0.2))  +
    # scale_y_continuous(labels = scales::number_format(accuracy = 1, big.mark = ","))+
    theme(
      # axis.title.y = element_blank(),
      plot.title = element_text(hjust=0, size=10),
      axis.title = element_text(size=12)
    ) 
  
}

pdf("output/frequency_biome26mid.pdf", width=10, height=11.5)
for(i in c(1:5, 12, 14) ){
  dat <- filter(ddf_long, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  dat3 <- filter(ddf_long2, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  p1 <- plotfun(dat, "frequency", "RCP 2.6") +
    theme(legend.position="none") +
    theme(axis.title.x = element_blank())
  p2 <- plotfun2(dat, "frequency", "RCP 2.6") +
    theme(legend.position="none")+
    theme(axis.title.x = element_blank())
  p3 <- plotfun(dat3, "frequency", "RCP 8.5") +
    theme(legend.position="none") 
  p4 <- plotfun2(dat3, "frequency", "RCP 8.5") 
  
  p5 <- plotfun3(dat, "frequency", "RCP 2.6") +
    theme(legend.position="none") +
    theme(axis.title.x = element_blank()) 
  p6 <- plotfun3(dat3, "frequency", "RCP 8.5") +
    theme(legend.position="none") 
  
  # get joint legend
  legend <- get_legend(p4 + 
                         theme_minimal() + 
                         theme(legend.title = element_blank(),
                               legend.position = "bottom"))
  
  p4 <- p4 +
    theme(legend.position="none")
  
  # make title 
  title <- ggdraw() +
    draw_label(paste0(biomenames$name[i], ", Mid-century"), 
               size=12,
               x = 0,
               hjust = 0) +
    theme(
      plot.margin = margin(0, 0, 0, 30)) 
  
  # ## arrange things
  # print(
  #   plot_grid(plot_grid(title, plot_grid(p1,p2,p3,p4, ncol=2, align='vh'), 
  #                       nrow=2, rel_heights = c(0.1,1,1)),
  #             legend, ncol=1, rel_heights = c(1,0.1))
  # )
  
  print(
    plot_grid(plot_grid(title, plot_grid(p1,p2,p5,p3,p4,p6, ncol=3, align='vh'), 
                        nrow=2, rel_heights = c(0.1,1,1)),
              legend, ncol=1, rel_heights = c(1,0.1))
  )
  
}

dev.off()


pdf("output/severity_biome26mid.pdf", width=10, height=11.5)
for(i in c(1:5, 12, 14) ){
  dat <- filter(ddf_long, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  dat3 <- filter(ddf_long2, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  p1 <- plotfun(dat, "severity", "RCP 2.6") +
    theme(legend.position="none") +
    theme(axis.title.x = element_blank())
  p2 <- plotfun2(dat, "severity", "RCP 2.6") +
    theme(legend.position="none")+
    theme(axis.title.x = element_blank())
  p3 <- plotfun(dat3, "severity", "RCP 8.5") +
    theme(legend.position="none") 
  p4 <- plotfun2(dat3, "severity", "RCP 8.5") 
  p5 <- plotfun3(dat, "severity", "RCP 2.6") +
    theme(legend.position="none") +
    theme(axis.title.x = element_blank()) 
  p6 <- plotfun3(dat3, "severity", "RCP 8.5") +
    theme(legend.position="none") 
  
  # get joint legend
  legend <- get_legend(p4 + 
                         theme_minimal() + 
                         theme(legend.title = element_blank(),
                               legend.position = "bottom"))
  
  p4 <- p4 +
    theme(legend.position="none")
  
  # make title 
  title <- ggdraw() +
    draw_label(paste0(biomenames$name[i], ", Mid-century"), 
               size=12,
               x = 0,
               hjust = 0) +
    theme(
      plot.margin = margin(0, 0, 0, 30)) 
  
  ## arrange things
  print(
    plot_grid(plot_grid(title, plot_grid(p1,p2,p5,p3,p4,p6, ncol=3, align='vh'), 
                        nrow=2, rel_heights = c(0.1,1,1)),
              legend, ncol=1, rel_heights = c(1,0.1))
  )
  
  
}

dev.off()

## tallest only----
ddf_tall <- filter(ddf_long, tallfor=="top90")
ddf_tall2 <- filter(ddf_long2, tallfor=="top90")

pdf("output/frequency_biome26mid_tallest.pdf", width=10, height=11.5)
for(i in c(1:5, 12, 14) ){
  dat <- filter(ddf_tall, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  dat3 <- filter(ddf_tall2, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  p1 <- plotfun(dat, "frequency", "RCP 2.6") +
    theme(legend.position="none") +
    theme(axis.title.x = element_blank())
  p2 <- plotfun2(dat, "frequency", "RCP 2.6") +
    theme(legend.position="none")+
    theme(axis.title.x = element_blank())
  p3 <- plotfun(dat3, "frequency", "RCP 8.5") +
    theme(legend.position="none") 
  p4 <- plotfun2(dat3, "frequency", "RCP 8.5") 
  p5 <- plotfun3(dat, "frequency", "RCP 2.6") +
    theme(legend.position="none") +
    theme(axis.title.x = element_blank()) 
  p6 <- plotfun3(dat3, "frequency", "RCP 8.5") +
    theme(legend.position="none") 
  
  # get joint legend
  legend <- get_legend(p4 + 
                         theme_minimal() + 
                         theme(legend.title = element_blank(),
                               legend.position = "bottom"))
  
  p4 <- p4 +
    theme(legend.position="none")
  
  # make title 
  title <- ggdraw() +
    draw_label(paste0(biomenames$name[i], ", Mid-century, Tall forests"), 
               size=12,
               x = 0,
               hjust = 0) +
    theme(
      plot.margin = margin(0, 0, 0, 30)) 
  
  ## arrange things
  print(
    plot_grid(plot_grid(title, plot_grid(p1,p2,p5,p3,p4,p6, ncol=3, align='vh'), 
                        nrow=2, rel_heights = c(0.1,1,1)),
              legend, ncol=1, rel_heights = c(1,0.1))
  )
  
  
}

dev.off()

pdf("output/severity_biome26mid_tallest.pdf", width=10, height=11.5)
for(i in c(1:5, 12, 14) ){
  dat <- filter(ddf_tall, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  dat3 <- filter(ddf_tall2, biome==i) %>% 
    left_join(realmnames) %>% 
    left_join(biomenames, by=c("biome"="BIOME"))
  p1 <- plotfun(dat, "severity", "RCP 2.6") +
    theme(legend.position="none") +
    theme(axis.title.x = element_blank())
  p2 <- plotfun2(dat, "severity", "RCP 2.6") +
    theme(legend.position="none")+
    theme(axis.title.x = element_blank())
  p3 <- plotfun(dat3, "severity", "RCP 8.5") +
    theme(legend.position="none") 
  p4 <- plotfun2(dat3, "severity", "RCP 8.5") 
  p5 <- plotfun3(dat, "severity", "RCP 2.6") +
    theme(legend.position="none") +
    theme(axis.title.x = element_blank()) 
  p6 <- plotfun3(dat3, "severity", "RCP 8.5") +
    theme(legend.position="none") 
  
  # get joint legend
  legend <- get_legend(p4 + 
                         theme_minimal() + 
                         theme(legend.title = element_blank(),
                               legend.position = "bottom"))
  
  p4 <- p4 +
    theme(legend.position="none")
  
  # make title 
  title <- ggdraw() +
    draw_label(paste0(biomenames$name[i], ", Mid-century, Tall forests"), 
               size=12,
               x = 0,
               hjust = 0) +
    theme(
      plot.margin = margin(0, 0, 0, 30)) 
  
  ## arrange things
  print(
    plot_grid(plot_grid(title, plot_grid(p1,p2,p5,p3,p4,p6, ncol=3, align='vh'), 
                        nrow=2, rel_heights = c(0.1,1,1)),
              legend, ncol=1, rel_heights = c(1,0.1))
  )
  
  
}

dev.off()

