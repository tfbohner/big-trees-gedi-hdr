library(tidyverse)
library(ggplot2)
library(ggstance)
library(readr)

## Mid-century----
ddf_wide <- read_delim("processed data/ddf_summary_wide.csv", ",", col_names = T) %>% 
  filter(!(name=="Mangroves"&realmname=="Nearctic"))

ddf_wide_late <- read_delim("processed data/ddf_summary_wide_late.csv", ",", col_names = T) %>% 
  filter(!(name=="Mangroves"&realmname=="Nearctic"))

p2 <- c("#276845", "#ddb340", "#22bf92", "#468bc3", "#b85a3d", "#44498b", "#c88ebe") 
ddf_wide <- ddf_wide %>% 
  filter(!is.na(name)) %>% 
  mutate(across(c(name), ~forcats::fct_relevel(., "Tropical Moist Broadleaf", "Tropical Dry Broadleaf", "Tropical Conifer", 
                                               "Temperate Broadleaf", "Temperate Conifer", "Mediterranean Forests", "Mangroves"))) %>%  
  mutate_at(c("delta_sev_else", "delta_sev_top10", "delta_sev_diff"), .funs = function(x){x*(-1)}) %>% 
  mutate(freq_sign=ifelse(delta_freq_diff<0, "negative", "positive"),
         sev_sign=ifelse(delta_sev_diff<0, "negative", "positive")) 

ddf_wide_late <- ddf_wide_late %>% 
  filter(!is.na(name)) %>% 
  mutate(across(c(name), ~forcats::fct_relevel(., "Tropical Moist Broadleaf", "Tropical Dry Broadleaf", "Tropical Conifer", 
                                               "Temperate Broadleaf", "Temperate Conifer", "Mediterranean Forests", "Mangroves"))) %>%  
  mutate_at(c("delta_sev_else", "delta_sev_top10", "delta_sev_diff"), .funs = function(x){x*(-1)}) %>% 
  mutate(freq_sign=ifelse(delta_freq_diff<0, "negative", "positive"),
         sev_sign=ifelse(delta_sev_diff<0, "negative", "positive")) 


## less busy plots ----
ggplot(filter(ddf_wide, scenario=="rcp 2.6"), 
       aes(name, delta_freq_top10, group=realmname, color=name)) +
  geom_point(position = position_dodge2(width = 0.8, preserve = "single"), alpha=0.6) +
  geom_point(data=filter(ddf_wide, scenario=="rcp 8.5"), aes(name, delta_freq_top10, group=realmname, color=name),
             position = position_dodge2(width = 0.8, preserve = "single")) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype='dashed', color='red')+
  # facet_wrap(~name) +
  scale_color_manual(values=p2) +
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_blank()
  ) +
  geom_vline(xintercept=seq(1.5, length(unique(ddf_wide$name))-0.5, 1), 
             lwd=.2, colour="black")+
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(text = element_text(size=16)) +
  guides(alpha="none", color="none", fill="none") +
  theme(axis.title.y = element_blank())  +
  geom_text(aes(name, delta_freq_top10, label=realmname, group=realmname),
            position=position_dodge2(width = 0.9, preserve = "single"), vjust=0.5, hjust=-0.3)

ggplot(filter(ddf_wide, scenario=="rcp 2.6"), 
             aes(name, delta_freq_top10, group=realmname, color=name)) +
  geom_point(position = position_dodge2(width = 0.8, preserve = "single")) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype='dashed', color='red')+
  # facet_wrap(~name) +
  scale_color_manual(values=p2) +
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_blank()
  ) +
  geom_vline(xintercept=seq(1.5, length(unique(ddf_wide$name))-0.5, 1), 
             lwd=.2, colour="black")+
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(text = element_text(size=16)) +
  guides(alpha="none", color="none", fill="none") +
  theme(axis.title.y = element_blank())  +
  geom_text(aes(name, delta_freq_top10, label=realmname, group=realmname),
            position=position_dodge2(width = 0.9, preserve = "single"), vjust=0.5, hjust=-0.3)



ggplot(filter(ddf_wide, scenario=="rcp 2.6"), 
       aes(name, delta_freq_top10, group=realmname, color=realmname)) +
  geom_point(position = position_dodge2(width = 0.8, preserve = "single")) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype='dashed', color='red')+
  theme_bw() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.minor.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_blank()
  ) +
  geom_vline(xintercept=seq(1.5, length(unique(ddf_wide$name))-0.5, 1), 
             lwd=.2, colour="black")+
  ylab("Projected change in drought events per decade") +
  ggtitle("RCP 2.6 mid-century projections") +
  theme(text = element_text(size=16)) +
  guides(alpha="none", color="none", fill="none") +
  theme(axis.title.y = element_blank())  +
  geom_text(aes(name, delta_freq_top10, label=realmname, group=realmname),
            position=position_dodge2(width = 0.9, preserve = "single"), vjust=0.5, hjust=-0.3)


## original plots ----

f1 <- ggplot(filter(ddf_wide, scenario=="rcp 2.6"), 
              aes(name, delta_freq_top10, group=realmname, fill=name)) +
  geom_linerange(aes(ymin=delta_freq_top10, ymax=delta_freq_else, color=freq_sign), 
                 position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(size=biomass_top10), shape=21, stroke=1.0, 
             position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(y=delta_freq_else, size=biomass_else), shape=21, stroke=1.0, 
             position=position_dodge2(width = 0.9, preserve = "single"),
             alpha=0.5, color="gray") +
  coord_flip() +
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
  theme(text = element_text(size=16)) +
  guides(alpha="none", color="none", fill="none") +
  theme(axis.title.y = element_blank()) 

f2 <- ggplot(filter(ddf_wide, scenario=="rcp 8.5"), 
             aes(name, delta_freq_top10, group=realmname, fill=name)) +
  geom_linerange(aes(ymin=delta_freq_top10, ymax=delta_freq_else, color=freq_sign), 
                 position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(size=biomass_top10), shape=21, stroke=1.0, 
             position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(y=delta_freq_else, size=biomass_else), shape=21, stroke=1.0, 
             position=position_dodge2(width = 0.9, preserve = "single"),
             alpha=0.5, color="gray") +
  coord_flip() +
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
  ggtitle("RCP 8.5 mid-century projections") +
  theme(text = element_text(size=16)) +
  guides(alpha="none", color="none", fill="none") +
  theme(axis.title.y = element_blank()) 

s1 <- ggplot(filter(ddf_wide, scenario=="rcp 2.6"), 
             aes(name, delta_sev_top10, group=realmname, fill=name)) +
  geom_linerange(aes(ymin=delta_sev_top10, ymax=delta_sev_else, color=sev_sign), 
                 position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(size=biomass_top10), shape=21, stroke=1.0, 
             position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(y=delta_sev_else, size=biomass_else), shape=21, stroke=1.0, 
             position=position_dodge2(width = 0.9, preserve = "single"),
             alpha=0.5, color="gray") +
  coord_flip() +
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
  theme(text = element_text(size=16)) +
  guides(alpha="none", color="none", fill="none") +
  theme(axis.title.y = element_blank()) 

s2 <- ggplot(filter(ddf_wide, scenario=="rcp 8.5"), 
             aes(name, delta_sev_top10, group=realmname, fill=name)) +
  geom_linerange(aes(ymin=delta_sev_top10, ymax=delta_sev_else, color=sev_sign), 
                 position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(size=biomass_top10), shape=21, stroke=1.0, 
             position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_point(aes(y=delta_sev_else, size=biomass_else), shape=21, stroke=1.0, 
             position=position_dodge2(width = 0.9, preserve = "single"),
             alpha=0.5, color="gray") +
  coord_flip() +
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
  ggtitle("RCP 8.5 mid-century projections") +
  theme(text = element_text(size=16)) +
  guides(alpha="none", color="none", fill="none") +
  theme(axis.title.y = element_blank())

f1b <- f1 +
  geom_text(aes(name, delta_freq_top10, label=realmname, group=realmname),
            position=position_dodge2(width = 0.9, preserve = "single"), vjust=0.5, hjust=-0.3)
f2b <- f2 +
  geom_text(aes(name, delta_freq_top10, label=realmname, group=realmname),
            position=position_dodge2(width = 0.9, preserve = "single"), vjust=0.5, hjust=-0.3)

s1b <- s1 +
  geom_text(aes(name, delta_sev_top10, label=realmname, group=realmname),
            position=position_dodge2(width = 0.9, preserve = "single"), vjust=0.5, hjust=-0.3)
s2b <- s2 +
  geom_text(aes(name, delta_sev_top10, label=realmname, group=realmname),
            position=position_dodge2(width = 0.9, preserve = "single"), vjust=0.5, hjust=-0.3)

pdf("output/bubbleplot.pdf")
print(list(f1,f1b,f2,f2b,s1,s1b,s2,s2b))
dev.off()


scen.labs <- c("RCP 2.6", "RCP 8.5")
names(scen.labs) <- c("rcp 2.6", "rcp 8.5")
fplot <- function (dat){
  plot <- ggplot(dat, 
                 aes(name, delta_freq_top10, group=realmname, fill=name)) +
    geom_linerange(aes(ymin=delta_freq_top10, ymax=delta_freq_else, color=freq_sign), 
                   position = position_dodge2(width = 0.9, preserve = "single")) +
    geom_point(aes(size=biomass_top10), shape=21, stroke=1.0, 
               position = position_dodge2(width = 0.9, preserve = "single")) +
    geom_point(aes(y=delta_freq_else, size=biomass_else), shape=21, stroke=1.0, 
               position=position_dodge2(width = 0.9, preserve = "single"),
               alpha=0.5, color="gray") +
    facet_wrap(~scenario, scales="free_x", ncol=2, 
               labeller = labeller(scenario=scen.labs)) +
    coord_flip() +
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
    theme(
      strip.background = element_blank(),
      strip.text = element_text(hjust=0, size=12),
      axis.title = element_text(size=12),
      plot.title = element_text(size=12),
      axis.text = element_text(size=11),
      legend.text = element_text(size=11),
      legend.title = element_text(size=12)
      
    ) +
    ylab("Projected change in drought events per decade") +
    theme(text = element_text(size=16)) +
    guides(alpha="none", color="none", fill="none") +
    theme(axis.title.y = element_blank()) + 
    scale_y_continuous(expand = c(0.2, 0)) +
    scale_x_discrete(expand = c(0.12, 0))
  
  
}

splot <- function(dat){
  plot <- ggplot(dat, 
                 aes(name, delta_sev_top10, group=realmname, fill=name)) +
    geom_linerange(aes(ymin=delta_sev_top10, ymax=delta_sev_else, color=sev_sign), 
                   position = position_dodge2(width = 0.9, preserve = "single")) +
    geom_point(aes(size=biomass_top10), shape=21, stroke=1.0, 
               position = position_dodge2(width = 0.9, preserve = "single")) +
    geom_point(aes(y=delta_sev_else, size=biomass_else), shape=21, stroke=1.0, 
               position=position_dodge2(width = 0.9, preserve = "single"),
               alpha=0.5, color="gray") +
    facet_wrap(~scenario, scales="free_x", ncol=2, 
               labeller = labeller(scenario=scen.labs)) +
    coord_flip() +
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
    theme(
      strip.background = element_blank(),
      strip.text = element_text(hjust=0, size=12),
      axis.title = element_text(size=12),
      plot.title = element_text(size=12),
      axis.text = element_text(size=11),
      legend.text = element_text(size=11),
      legend.title = element_text(size=12)
      
    ) +
    ylab("Projected change in drought severity") +
    theme(text = element_text(size=16)) +
    guides(alpha="none", color="none", fill="none") +
    theme(axis.title.y = element_blank())  +
    scale_y_continuous(expand = c(0.2, 0)) +
    scale_x_discrete(expand = c(0.12, 0))
}

fmid <- fplot(ddf_wide) +
  ggtitle("Mid-century projections") 
smid <- splot(ddf_wide) +
  ggtitle("Mid-century projections") 
flate <- fplot(ddf_wide_late) +
  ggtitle("Late-century projections") 
slate <- splot(ddf_wide_late) +
  ggtitle("Late-century projections") 


fmid2 <- fmid +
  geom_label(aes(name, delta_freq_top10, label=realmname, group=realmname),
            position=position_dodge2(width = 0.9, preserve = "single"), hjust=0.2, alpha=0.5, size = 9/.pt) 
flate2 <- flate +
  geom_label(aes(name, delta_freq_top10, label=realmname, group=realmname),
            position=position_dodge2(width = 0.9, preserve = "single"), hjust=0.2, alpha=0.5, size = 9/.pt) 
smid2 <- smid +
  geom_label(aes(name, delta_sev_top10, label=realmname, group=realmname),
            position=position_dodge2(width = 0.9, preserve = "single"), hjust=0.2, alpha=0.5, size = 9/.pt) 
slate2 <- slate +
  geom_label(aes(name, delta_sev_top10, label=realmname, group=realmname),
            position=position_dodge2(width = 0.9, preserve = "single"), hjust=0.2, alpha=0.5, size = 9/.pt) 

pdf("output/bubbleplot_facet.pdf", width = 10, height=6)
print(list(fmid, fmid2, flate, flate2, smid, smid2, slate, slate2))
dev.off()

p2 <- c("#276845", "#ddb340", "#22bf92", "#468bc3", "#b85a3d", "#44498b", "#c88ebe") 
biomelist <- filter(biomenames, name%in%unique(ddf_wide$name))

pdf("output/bubble_biome.pdf", width=10, height=2.5)
for(i in 1:7) {
  plot1 <- fplot(filter(ddf_wide, name==biomelist$name[i])) +
    scale_fill_manual(values=p2[i])  +
    scale_y_continuous(expand = c(0.25, 0)) +
    scale_x_discrete(expand = c(0.15, 0.5))
  plot1b <- plot1 +
    geom_label(aes(name, delta_freq_top10, label=realmname, group=realmname),
               position=position_dodge2(width = 0.9, preserve = "single"), hjust=0.2, alpha=0.5, size = 9/.pt) 
  print(list(plot1, plot1b))
}

dev.off()

pdf("output/bubble_biome_tall.pdf", width=6, height=5)
for(i in 1:7) {
  plot1 <- fplot(filter(ddf_wide, name==biomelist$name[i])) +
    facet_wrap(~scenario, scales="free_x", ncol=1, 
               labeller = labeller(scenario=scen.labs)) +
    scale_fill_manual(values=p2[i])  +
    scale_y_continuous(expand = c(0.25, 0)) +
    scale_x_discrete(expand = c(0.15, 0.5)) +
    ggtitle(biomelist$name[i])+
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(size=12)) 
  plot1b <- plot1 +
    geom_label(aes(name, delta_freq_top10, label=realmname, group=realmname),
               position=position_dodge2(width = 0.9, preserve = "single"), hjust=0, alpha=0.5, size = 9/.pt) 
  print(list(plot1, plot1b))
}

dev.off()