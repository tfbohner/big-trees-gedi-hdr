library(terra)

path1 <- "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/drought/raw drought/"
file_list <- list.files(path1)

# rcp26 <- file_list[grepl(pattern = ".*rcp26", x=file_list, fixed=F)==T]

dr_freq <- function(x,y,nlayers){length(which(x<y))/(nlayers/120)}
dr_months <- function(x,y,nlayers){length(which(x<y))}


# fr_mid <- list()
# 
# res_list <- lapply(fr_mid, res)
# 
# which(sapply(res_list,"[[",1)==max(sapply(res_list,"[[",1)))
# which(sapply(res_list,"[[",2)==max(sapply(res_list,"[[",2)))
# 
# list2 <- lapply(fr_mid, function(x) resample(x, fr_mid[6]))
# 
# test <- resample(fr_mid[[1]], fr_mid[[6]])

for(n in c("hist", "rcp26", "rcp85")){
  patt <- paste0(".*", n)
  print(patt)
  file_sub <- file_list[grepl(pattern = patt, x=file_list, fixed=F)==T]
  print(file_sub)
  
  for(i in 1:9) {
    if(n=="hist") {
      coarse <- rast(paste0(path1, file_sub[6]))
      d <- rast(paste0(path1, file_sub[i]))
      d <- resample(d, coarse)
      print(d)

      fr_hist <- app(d,fun=dr_freq,y=-2,nlayers=nlyr(d))

      
      if(i==1){
        hist_stack <- fr_hist
      } else{
        hist_stack <- c(hist_stack, fr_hist)
      }
      
    } else{
      coarse <- rast(paste0(path1, file_sub[6]))
      d <- rast(paste0(path1, file_sub[i]))
      d <- resample(d, coarse)
      print(d)
      
      dmid <- d[[1:420]]
      dlate <- d[[601:1020]]
      fr_mid <- app(dmid,fun=dr_freq,y=-2,nlayers=nlyr(dmid))
      fr_late <- app(dlate,fun=dr_freq,y=-2,nlayers=nlyr(dlate))
      
      if(i==1){
        mid_stack <- fr_mid
        late_stack <- fr_late
      } else{
        mid_stack <- c(mid_stack, fr_mid)
        late_stack <- c(late_stack, fr_late)
      }
    }

  }
  
  if(n=="hist") {
    assign("hist_stack", hist_stack)
  } else{
    assign(paste0("mid_stack_", n), mid_stack)
    assign(paste0("late_stack_", n), late_stack)
  }
  
}

writeRaster(hist_stack, "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/drought/months_neg2_drought_hist.tif", overwrite=T)
writeRaster(mid_stack_rcp26, "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/drought/months_neg2_drought_mid_rcp26.tif", overwrite=T)
writeRaster(late_stack_rcp26, "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/drought/months_neg2_drought_late_rcp26.tif", overwrite=T)
writeRaster(mid_stack_rcp85, "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/drought/months_neg2_drought_mid_rcp85.tif", overwrite=T)
writeRaster(late_stack_rcp85, "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/drought/months_neg2_drought_late_rcp85.tif", overwrite=T)





mid_avg <- app(mid_stack_rcp26, fun=median)
plot(mid_avg)


pmid_sd <- app(mid_stack_rcp26, fun=sd)
plot(mid_sd)


### drought projection file
dhist <- rast("/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/drought/raw drought/drought_indices_AWI_CM_1_1_MR_SPEI_hist_12.tif")
d <- rast("/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/drought/raw drought/drought_indices_AWI_CM_1_1_MR_SPEI_rcp85_12.tif")
dmid <- d[[1:420]]
dlate <- d[[601:1020]]

### drought frequency (months/decade)
dr_freq <- function(x,y,nlayers){length(which(x<y))/(nlayers/120)}
dr_months <- function(x,y,nlayers){length(which(x<y))}
fr_hist <- app(dhist,fun=dr_freq,y=-2,nlayers=nlyr(dhist))
fr_mid <- app(dmid,fun=dr_freq,y=-2,nlayers=nlyr(dmid))
fr_late <- app(dlate,fun=dr_freq,y=-2,nlayers=nlyr(dlate))

# delta_fr_mid <- fr_mid - fr_hist
# delta_fr_late <- fr_late - fr_hist

writeRaster(fr_hist, "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/drought/months_neg2_drought_spei_12.tif", overwrite=T)
writeRaster(fr_mid, "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/drought/months_neg2_drought_mid_rcp85_spei_12.tif", overwrite=T)
writeRaster(fr_late, "/Users/teresabohner/Google Drive/My Drive/Teresa Lab Dir/HDR big trees/data/drought/months_neg2_drought_late_rcp85_spei_12.tif", overwrite=T)

