library(terra)
library(mbsi)
library(ggplot2)
rast <- rast("data/drought/resampled drought/drought_indices_AWI_CM_1_1_MR_SPEI_hist_12.tif")

timeseries <- terra::extract(rast, c( -122.679565, 45.512794))
timeseries2 <- sapply(timeseries, "[[", 2)

df <- expand_grid(year = c(1981:2014), month=c(1:12))
df <- df[1:397,]
df$SPEI <- timeseries2
df$counter<- c(1:397)
df$event <- find_flood_drought(df$SPEI, threshold = 2)
df$drought <- ifelse(df$event=="drought", "drought", NA)
df$droughtcount <- ifelse(df$event=="drought",1, 3)
df$droughtcount <- ifelse(df$droughtcount==1&df$year>1990, 2, df$droughtcount)

df <- df %>%
  mutate(date = make_date(year, month))


ggplot(df, aes(date, SPEI)) +
  geom_line() +
  geom_ribbon(aes(ymin = SPEI, ymax = 0, fill = as.factor(droughtcount), alpha=as.factor(droughtcount))) +
  scale_fill_manual(values=c("pink", "pink", "#000000")) +
  scale_alpha_manual(values=c(0.8, 0.8, 0)) +
  theme_test() +
  theme(axis.title.x=element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size=11)) +
  geom_hline(yintercept=0)+
  geom_hline(yintercept=-2,linetype='dashed')

pdf("output/example_timeseries.pdf", height=4, width=10)
print(
  ggplot(df, aes(date, SPEI)) +
    geom_line() +
    geom_ribbon(aes(ymin = SPEI, ymax = 0, fill = as.factor(droughtcount), alpha=as.factor(droughtcount))) +
    scale_fill_manual(values=c("pink", "pink", "#000000")) +
    scale_alpha_manual(values=c(0.8, 0.8, 0)) +
    theme_test() +
    theme(axis.title.x=element_blank(),
          axis.title = element_text(size=12),
          axis.text = element_text(size=11)) +
    geom_hline(yintercept=0)+
    geom_hline(yintercept=-2,linetype='dashed')+
    theme(legend.position = 'none')
)
dev.off()


