## Script to Derive drought metrics from raw Drought files (From Thymios)

library(terra)

## set your path to wherever files come from and will go
mainpath <-"/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees"

path1 <- paste0(mainpath, "/data/drought/raw drought/")
path2 <- paste0(mainpath, "/data/drought/")
file_list <- list.files(path1)


### drought time (months below threshold/decade)
dr_time <- function(x,y,nlayers){length(which(x<y))/(nlayers/120)}

### drought frequency (events)
dr_freq <- function(x,y,nlayers){rl <- rle(x<y); return(length(which(rl$values==TRUE))/(nlayers/120))}

### max duration at theshold y
max_dur <- function(x,y,nlayers){rl <- rle(x<y); druns <-which(rl$values==TRUE);return(max(rl$lengths[druns]))}
### median duration at theshold y
med_dur <- function(x,y,nlayers){rl <- rle(x<y); druns <-which(rl$values==TRUE);return(median(rl$lengths[druns]))}

### max severity at theshold y
max_severe <- function(x,y,nlayers){rl <- rle(x<y); 
  sevs <- c()
  druns <-which(rl$values==TRUE);
  if (length(druns)>0){
    runs.lengths.cumsum = cumsum(rl$lengths);
    ends = runs.lengths.cumsum[druns];
    newindex = ifelse(druns>1, druns-1, 0);
    starts = runs.lengths.cumsum[newindex] + 1;
    if (0 %in% newindex) starts = c(1,starts);
    for (i in 1:length(starts)){;
      sevs[i] <- sum(x[starts[i]:ends[i]],na.rm=TRUE)};
  } else {
    sevs <- 0}
  return(min(sevs))}

med_severe <- function(x,y,nlayers){rl <- rle(x<y); 
  sevs <- c()
  druns <-which(rl$values==TRUE);
  if (length(druns)>0){
    runs.lengths.cumsum = cumsum(rl$lengths);
    ends = runs.lengths.cumsum[druns];
    newindex = ifelse(druns>1, druns-1, 0);
    starts = runs.lengths.cumsum[newindex] + 1;
    if (0 %in% newindex) starts = c(1,starts);
    for (i in 1:length(starts)){;
      sevs[i] <- sum(x[starts[i]:ends[i]],na.rm=TRUE)};
  } else {
    sevs <- 0}
  return(median(sevs))}
# 
# 
# dmid <- d[[1:420]]
# dlate <- d[[601:1020]]
# 
# fr_hist <- app(dhist,fun=dr_freq,y=-1,nlayers=nlyr(dhist))
# fr_late <- app(dlate,fun=dr_freq,y=-1,nlayers=nlyr(dlate))
# fr_hist2 <- app(dhist,fun=dr_freq2,y=-1,nlayers=nlyr(dhist))
# fr_late2 <- app(dlate,fun=dr_freq2,y=-1,nlayers=nlyr(dlate))

thresh <- -2


## Loop through all the functions and all the models to create rasters of all the outputs
for(f in c("dr_time", "dr_freq", "max_dur", "med_dur", "max_severe", "med_severe")) {
  h_patt <- paste0(".*", "hist")
  print(h_patt)
  h_file_sub <- file_list[grepl(pattern = h_patt, x=file_list, fixed=F)==T]
  print(h_file_sub)
  for(n in c("rcp26", "rcp85")){
    patt <- paste0(".*", n)
    print(patt)
    file_sub <- file_list[grepl(pattern = patt, x=file_list, fixed=F)==T]
    print(file_sub)
    
    for(i in 1:9) {
      coarse <- rast(paste0(path1, h_file_sub[6]))
      dhist <- rast(paste0(path1, h_file_sub[i]))
      dhist <- resample(dhist, coarse)
      print(dhist)
      d <- rast(paste0(path1, file_sub[i]))
      d <- resample(d, coarse)
      print(d)
      
      dmid <- d[[1:420]]
      dlate <- d[[601:1020]]
      
      hist <- app(dhist, fun=f, y=thresh, nlayers=nlyr(dhist))
      hist[is.infinite(hist)] <- 0
      mid <- app(dmid, fun=f, y=thresh, nlayers=nlyr(dmid))
      mid[is.infinite(mid)] <- 0
      late <- app(dlate, fun=f, y=thresh, nlayers=nlyr(dlate))
      late[is.infinite(late)] <- 0
      
      
      if(i==1){
        hist_stack <- hist
        mid_stack <- mid
        late_stack <- late
        
      } else{
        hist_stack <- c(hist_stack, hist)
        mid_stack <- c(mid_stack, mid)
        late_stack <- c(late_stack, late)}

    } 
    
    delta_mid <- mid_stack-hist_stack
    delta_late <- late_stack-hist_stack
    
    
    writeRaster(hist_stack, paste0(path2, f, "_hist.tif"), overwrite=T)
    writeRaster(mid_stack, paste0(path2, f,"_",n,"_mid.tif"), overwrite=T)
    writeRaster(late_stack, paste0(path2, f,"_",n,"_late.tif"), overwrite=T)
    writeRaster(delta_mid, paste0(path2, f,"_",n,"_mid_delta.tif"), overwrite=T)
    writeRaster(delta_late, paste0(path2, f,"_",n,"_late_delta.tif"), overwrite=T)

  }

}


## Loop through all the scenarios and metrics to create summary (max, min, median) layers of all the metrics
for(n in c("rcp26", "rcp85")){
  
  for(f in c("dr_time", "dr_freq", "max_dur", "med_dur", "max_severe", "med_severe")) {
    hist_stack <- rast(paste0(path2, f, "_hist.tif"))
    mid_stack <- rast(paste0(path2, f,"_",n,"_mid.tif"))
    late_stack <- rast(paste0(path2, f,"_",n,"_late.tif"))
    delta_mid <- rast(paste0(path2, f,"_",n,"_mid_delta.tif"))
    delta_late <- rast(paste0(path2, f,"_",n,"_late_delta.tif"))
    
    hist <- c(min(hist_stack, na.rm = T), median(hist_stack, na.rm = T), max(hist_stack, na.rm = T))
    names(hist) <- paste0(f, "_", names(hist))
    
    mid <- c(min(mid_stack, na.rm = T), median(mid_stack, na.rm = T), max(mid_stack, na.rm = T))
    names(mid) <- paste0(f, "_", names(mid))
    
    late <- c(min(late_stack, na.rm = T), median(late_stack, na.rm = T), max(late_stack, na.rm = T))
    names(late) <- paste0(f, "_", names(late))
    
    delta_mid <- c(min(delta_mid, na.rm = T), median(delta_mid, na.rm = T), max(delta_mid, na.rm = T))
    names(delta_mid) <- paste0("delta_",f, "_", names(delta_mid))
    
    delta_late <- c(min(delta_late, na.rm = T), median(delta_late, na.rm = T), max(delta_late, na.rm = T))
    names(delta_late) <- paste0("delta_",f, "_", names(delta_late))
    
    if(f=="dr_time"){
      hist_all <- hist
      mid_all <- mid
      late_all <- late
      delta_mid_all <- delta_mid
      delta_late_all <- delta_late
    } else{
        hist_all <- c(hist_all, hist)
        mid_all <- c(mid_all, mid)
        late_all <- c(late_all, late)
        delta_mid_all <- c(delta_mid_all, delta_mid)
        delta_late_all <- c(delta_late_all, delta_late)
    }
  }
  
  writeRaster(hist_all, paste0(path2, "allmetrics_summary_hist.tif"), overwrite=T)
  writeRaster(mid_all, paste0(path2, "allmetrics_summary_",n,"_mid.tif"), overwrite=T)
  writeRaster(late_all, paste0(path2, "allmetrics_summary_",n,"_late.tif"), overwrite=T)
  writeRaster(delta_mid_all, paste0(path2, "allmetrics_summary_",n,"_mid_delta.tif"), overwrite=T)
  writeRaster(delta_late_all, paste0(path2, "allmetrics_summary_",n,"_late_delta.tif"), overwrite=T)

}

