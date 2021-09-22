library(plyr)
library(rowr)
library(xtable)

#### FLEVO

flevo_res <- read.csv('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Random_search_BBCH_Rafa/Results-all-images.csv', stringsAsFactors = F)

resultss_adam <- flevo_res[flevo_res$optimizer == 'Adam',]
paste0(resultss_adam$LR, sep = ',', collapse = '')
paste0(resultss_adam$momentum, sep = ',', collapse = '')

r71 <- read.csv('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Random_search_BBCH_Rafa/Results-all-images_py/71/cnn_output_data_check.csv', stringsAsFactors = F)
r99 <- read.csv('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Random_search_BBCH_Rafa/Results-all-images_py/99/cnn_output_data_check.csv', stringsAsFactors = F)

table(r71$basename == r99$basename)
table(r71$cnn_labels__RAW == r99$cnn_labels__RAW)
table(r71$cnn_labels == r99$cnn_labels)

#BSO/no-BSO comparison
colnames(flevo_res)[grep('test',colnames(flevo_res))]
accTypes <- c("test_bbch_pict", "test_bbch_parcel","test_crop_parcel","test_crop_pict")

#accTypes_compare
accTypes_dfComp <- data.frame(matrix(NA, nrow = length(accTypes), ncol = 3))
colnames(accTypes_dfComp) <- c('accType', 'T', 'F')

#False comparisons - BSO accuracy is higher than noBSO
runsWithHighBSO <- data.frame()
cbind.fill<-function(...){
  nm <- list(...) 
  nm<-lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

for(i in 1:length(accTypes)){
  accTypes_dfComp$accType[i] <- accTypes[i]
  
  flevo_res_int <- flevo_res[c(1,grep(accTypes[i], colnames(flevo_res)))]
  
  t_comp <- data.frame(table(flevo_res_int[,grep('noBSO', colnames(flevo_res_int))] > flevo_res_int[,grep('_BSO', colnames(flevo_res_int))]))
  
  accTypes_dfComp$T[i] <- t_comp$Freq[grep('TRUE',t_comp$Var1)]
  accTypes_dfComp$F[i] <- t_comp$Freq[grep('FALSE',t_comp$Var1)]
  
  higherBSO <- flevo_res_int[!flevo_res_int[,grep('noBSO', colnames(flevo_res_int))] > flevo_res_int[,grep('_BSO', colnames(flevo_res_int))],]
  
  if(i == 1){
    runsWithHighBSO <- higherBSO
  }else{
    runsWithHighBSO <- merge(runsWithHighBSO, higherBSO, by = 'X', all = T)
  }

}
write.csv(runsWithHighBSO,'/data/work/Ispra/Flevoland/Flevo_v5_presi/runsWithHighBSO.csv', row.names = F)

#STUPID MOMO WAY  
for(i in 1:nrow(runsWithHighBSO)){
  runsWithHighBSO[i,][runsWithHighBSO[i,] > 0.8]
}


#how many pics per parcel
flevo_gt <- '/data/work/Ispra/Flevoland/Flevo_v5/inputs/gt_BBCH.csv'
working_dir <- '/data/work/Ispra/Flevoland/Flevo_v5/outputs/Random_search_BBCH_Rafa/Results-all-images_py'
i <- 0

df_flevo <- read.csv(flevo_gt, stringsAsFactors = F)
df.bbch <- read.csv(file.path(working_dir,i,'cnn_output_data_check.csv'), stringsAsFactors = F)

#remove __RAW vectors
df.bbch <- df.bbch[,-c(grep('__RAW', colnames(df.bbch)))]

#preprocess
colnames(df.bbch) <- c('basename', 'code_bbch_surveyed', 'code_bbch_max', 'prob_max')
df.bbch$filename <- paste0('/mnt/mars/Projects/2016_LANDSENSE/Data', df.bbch$basename)

#remove trash, empty rows and bso
df.bbch <- df.bbch[df.bbch$code_bbch_max != 'tsh0',]
df.bbch <- df.bbch[df.bbch$code_bbch_surveyed != '',]
df.bbch <- df.bbch[!grepl('BSO',df.bbch$code_bbch_surveyed),]

df.bbch <- df.bbch[df.bbch$code_bbch_max != 'bso0',]
df.bbch <- df.bbch[df.bbch$code_bbch_max != 'bso1',]
df.bbch <- df.bbch[df.bbch$code_bbch_max != 'bso2',]

#conver to uppercase
df.bbch$code_max <- toupper(substr(df.bbch$code_bbch_max, 1, 3) )
df.bbch$code_surveyed <- toupper(substr(df.bbch$code_bbch_surveyed, 1, 3) )
df.bbch$code_bbch_max <- toupper(df.bbch$code_bbch_max) 

#try by removing code_bbch_surveyed and code_surveyed from the test_set and using the one from the gt like Lauri
df.bbch$code_bbch_surveyed <- NULL
df.bbch$code_surveyed <- NULL

#merge with original data tensorflow_flevoland_up_noDUP
#df_flevo$code_bbch_surveyed <- NULL

df.bbch <- merge(df.bbch, df_flevo, by = 'basename')

#check this crop larcel acc diff
nrow(df.bbch)
table(df.bbch$code_surveyed)
table(df.bbch$code_max)

# #should be 0
# if(grep('noDUP', flevo_gt) > 0){
#   #NoDUP
#   if(all(df.bbch$code_surveyed.x == df.bbch$code_surveyed.y)){
#     df.bbch$code_surveyed.y <- NULL
#     colnames(df.bbch)[7] <- "code_surveyed"
#   }else{
#     stop(cat("ERROR at merge code_surveyed"))
#   }
# }else{
#   #DUP
#   df.bbch$code_surveyed.y <- NULL
#   colnames(df.bbch)[7] <- "code_surveyed"
#   #nrow(df.bbch[is.na(df.bbch$code_bbch_surveyed),])
# }

df.crop <- df.bbch
##correct parcels with more than one Ground Truth in terms of Crop
parcelsWithGT2 <- data.frame(matrix(NA, nrow = length(unique(df.crop$objectid_survey)), ncol = 2))
colnames(parcelsWithGT2) <- c('objectid_survey', 'code_surveyed')
for(parcell in 1:length(unique(df.crop$objectid_survey))){
  dfInt <- df.crop[df.crop$objectid_survey == unique(df.crop$objectid_survey)[parcell],]
  #dfInt <- df.crop[df.crop$objectid_survey == 686829,]
  parcelsWithGT2$objectid_survey[parcell] <- unique(dfInt$objectid_survey)
  parcelsWithGT2$code_surveyed[parcell] <- paste0(unique(dfInt$code_surveyed), collapse = ',')
}

parcelsWithGT2 <- parcelsWithGT2[order(parcelsWithGT2$objectid_survey),]
parcelsWithGT2 <- parcelsWithGT2[nchar(parcelsWithGT2$code_surveyed) > 3,]

for(ii in 1:nrow(parcelsWithGT2)){
  parcelID <- parcelsWithGT2$objectid_survey[[ii]]
  
  #print(paste0('working on parcelID ', parcelID))
  
  uniqueCode_survey <- parcelsWithGT2$code_surveyed[[ii]]
  uniqueCode_survey <- strsplit(uniqueCode_survey, ',')[[1]]
  for(iii in 1:length(uniqueCode_survey)){
    #print(paste(parcelID, uniqueCode_survey[iii]))
    
    #print(paste0(as.double(parcelID+(iii/100)), ' is the new parcelID'))
    df.crop$objectid_survey[which(df.crop$objectid_survey == parcelID & df.crop$code_surveyed == uniqueCode_survey[iii])] <- as.double(parcelID+(iii/10))
    
    #print('New row in df.crop')
    #print(which(df.crop$objectid_survey == (parcelID + (iii/100)) & df.crop$code_surveyed == uniqueCode_survey[iii]))
  }
}


#correct parcels with more than one BBCH
parcelsWithBBCH2 <- data.frame(matrix(NA, nrow = length(unique(df.bbch$objectid_survey)), ncol = 2))
colnames(parcelsWithBBCH2) <- c('objectid_survey', 'code_bbch_surveyed')
for(parcell in 1:length(unique(df.bbch$objectid_survey))){
  dfInt <- df.bbch[df.bbch$objectid_survey == unique(df.bbch$objectid_survey)[parcell],]
  #dfInt <- df.bbch[df.bbch$objectid_survey == 686829,]
  parcelsWithBBCH2$objectid_survey[parcell] <- unique(dfInt$objectid_survey)
  parcelsWithBBCH2$code_bbch_surveyed[parcell] <- paste0(unique(dfInt$code_bbch_surveyed), collapse = ',')
}

parcelsWithBBCH2 <- parcelsWithBBCH2[order(parcelsWithBBCH2$objectid_survey),]
parcelsWithBBCH2 <- parcelsWithBBCH2[nchar(parcelsWithBBCH2$code_bbch_surveyed) > 5,]

for(jjj in 1:nrow(parcelsWithBBCH2)){
  parcelID <- parcelsWithBBCH2$objectid_survey[[jjj]]
  #print(parcelID)
  uniqueCodeBBCH_survey <- parcelsWithBBCH2$code_bbch_surveyed[[jjj]]
  uniqueCodeBBCH_survey <- strsplit(uniqueCodeBBCH_survey, ',')[[1]]
  for(jjjj in 1:length(uniqueCodeBBCH_survey)){
    #print(uniqueCodeBBCH_survey[jjjj])
    
    df.bbch$objectid_survey[df.bbch$objectid_survey == parcelID & df.bbch$code_bbch_surveyed == uniqueCodeBBCH_survey[jjjj]] <- as.double(parcelID+(jjjj/10))
    
    #print(df.bbch[df.bbch$objectid_survey == parcelID+(jjjj/10) & df.bbch$code_bbch_surveyed == uniqueCodeBBCH_survey[jjjj],])
  }
}


all(sort(unique(df.bbch$code_bbch_max)) == sort(unique(df.bbch$code_bbch_surveyed)))
all(sort(unique(df.bbch$code_max)) == sort(unique(df.bbch$code_surveyed)))

#bbch level
df.bbch_objid_tab <-  as.data.frame(table(df.bbch$objectid_survey))
hist(df.bbch_objid_tab$Freq)

#crop level
table(df.crop$objectid_survey)

###LUCAS


r64 <- read.csv('/data/work/Ispra/LUCAS/CROP/LUCAS_C_vision/v2/outputs/Random_search_LUCAS/Results-all-images_py/64/cnn_output_data_check.csv', stringsAsFactors = F)
r20 <- read.csv('/data/work/Ispra/LUCAS/CROP/LUCAS_C_vision/v2/outputs/Random_search_LUCAS/Results-all-images_py/20/cnn_output_data_check.csv', stringsAsFactors = F)

table(r64$pointidyear == r20$pointidyear)
table(r64$cnn_labels__RAW == r20$cnn_labels__RAW)
table(r64$cnn_labels == r20$cnn_labels)
  
#test set
flevo5_tstset <- read.csv('/data/work/Ispra/Flevoland/Flevo_v5/inputs/flevo5_test_set_scr3000.csv', stringsAsFactors = F)
head(flevo5_tstset)
table(flevo5_tstset$cropbbch)

#test set RAW
flevo5_tstset_RAW <- read.csv('/data/work/Ispra/Flevoland/Flevo_v5/inputs/flevo5_test_set_scr.csv', stringsAsFactors = F)

#test set spatial distribution
f5_extrBname <- function(df){
  pathcolIdx <- grep('path', colnames(df))
  
  for(i in 1:nrow(df)){
    df$basename[i] <- strsplit(df[i, pathcolIdx], 'NL')[[1]][2]
    df$basename[i] <- paste0('NL', df$basename[i])
  }
  
  return(df)
}

flevo5_tstset <- f5_extrBname(flevo5_tstset)
flevo5_tstset_RAW <- f5_extrBname(flevo5_tstset_RAW)

#merge with flevo table to extract loc info
##should be all T
table(flevo5_tstset$basename %in% df_flevo$basename)
table(flevo5_tstset_RAW$basename %in% df_flevo$basename)

flevo5_tstset_merge <- merge(flevo5_tstset, df_flevo, by = 'basename')
flevo5_tstset_RAW_merge <- merge(flevo5_tstset_RAW, df_flevo, by = 'basename')
head(flevo5_tstset_merge)

#should be all T
table(flevo5_tstset_merge$cropbbch == flevo5_tstset_merge$code_bbch_surveyed)

library(rgdal)
# points from scratch
coords = cbind(flevo5_tstset_merge$longitude, flevo5_tstset_merge$latitude)
sp = SpatialPoints(coords)
# make spatial data frame
spdf = SpatialPointsDataFrame(coords, flevo5_tstset_merge)
spdf = SpatialPointsDataFrame(sp, flevo5_tstset_merge)

##__RAW
coords = cbind(flevo5_tstset_RAW_merge$longitude, flevo5_tstset_RAW_merge$latitude)
sp = SpatialPoints(coords)
# make spatial data frame
spdf_RAW = SpatialPointsDataFrame(coords, flevo5_tstset_RAW_merge)
spdf_RAW = SpatialPointsDataFrame(sp, flevo5_tstset_RAW_merge)

## BSO0, GRA1, SBT39 were the only crops that got randomly sampled
### Has there been any kind of spatial fuckery as a result?

cappedClasses <- c('BSO0', 'GRA1', 'SBT39')
cappedClasses_df <- data.frame(matrix(NA, nrow = length(cappedClasses), ncol = 5))
colnames(cappedClasses_df) <- c('cappedClass', 'nrowCC', 'nrowRAW', 'numParCC', 'numParRAW')

for(i in 1:length(cappedClasses)){
  cappedClasses_df$cappedClass[i] <- cappedClasses[i]
  
  #CC
  spdf_int <- spdf[spdf$cropbbch == cappedClasses[i],] 
  proj4string(spdf_int) <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ')
  #bbox(spdf_int)
  cappedClasses_df$nrowCC[i] <- nrow(spdf_int)
  #plot(spdf_int, add= T, col='green')
  
  spdf_int_objid_t <- as.data.frame(table(spdf_int$objectid_survey))
  cappedClasses_df$numParCC[i] <- nrow(spdf_int_objid_t)
  
  #RAW
  spdf_RAW_int <- spdf_RAW[spdf_RAW$code_bbch_surveyed == cappedClasses[i],] 
  proj4string(spdf_RAW_int) <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ')
  #bbox(spdf_RAW_int)
  cappedClasses_df$nrowRAW[i] <- nrow(spdf_RAW_int)
  #plot(spdf_RAW_int, add= T, col='red')
  
  spdf_RAW_int_objid_t <- as.data.frame(table(spdf_RAW_int$objectid_survey))
  cappedClasses_df$numParRAW[i] <-  nrow(spdf_RAW_int_objid_t)
  
}


#####
table_with_all_bbch <- read.csv('/data/work/Ispra/Flevoland/bbchstagesofallcrops_explained_fig8.csv', stringsAsFactors = F)
rownames(table_with_all_bbch) <- NULL
xtable(table_with_all_bbch)


#check how much removing BSO0 increases results
resultsAll <- read.csv('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Random_search_BBCH_Rafa/Results-all-images.csv', stringsAsFactors = F)

#bbchpic
resultsAll_bbchpic <- resultsAll[,c(1,2,3,4,5,6,7,8,9,10,11, 12, 13)]
resultsAll_bbchpic$withoutMoreThanWith <- ifelse(resultsAll_bbchpic$test_bbch_pict_noBSO > resultsAll_bbchpic$test_bbch_pict_BSO, T, F)
resultsAll_bbchpic_T <- resultsAll_bbchpic[resultsAll_bbchpic$withoutMoreThanWith == T,]
resultsAll_bbchpic_T$diff <- resultsAll_bbchpic_T$test_bbch_pict_noBSO - resultsAll_bbchpic_T$test_bbch_pict_BSO
mean(resultsAll_bbchpic_T$diff)

#bbchpar
resultsAll_bbchpar <- resultsAll[,c(1,2,3,4,5,6,7,8,9,10,11, 14, 15)]
resultsAll_bbchpar$withoutMoreThanWith <- ifelse(resultsAll_bbchpar$test_bbch_parcel_noBSO > resultsAll_bbchpar$test_bbch_parcel_BSO, T, F)
resultsAll_bbchpar_T <- resultsAll_bbchpar[resultsAll_bbchpar$withoutMoreThanWith == T,]
resultsAll_bbchpar_T$diff <- resultsAll_bbchpar_T$test_bbch_parcel_noBSO - resultsAll_bbchpar_T$test_bbch_parcel_BSO
mean(resultsAll_bbchpar_T$diff)

#croppic
resultsAll_croppic <- resultsAll[,c(1,2,3,4,5,6,7,8,9,10,11, 18, 19)]
resultsAll_croppic$withoutMoreThanWith <- ifelse(resultsAll_croppic$test_crop_pict_noBSO > resultsAll_croppic$test_crop_pict_BSO, T, F)
resultsAll_croppic_T <- resultsAll_croppic[resultsAll_croppic$withoutMoreThanWith == T,]
resultsAll_croppic_T$diff <- resultsAll_croppic_T$test_crop_pict_noBSO - resultsAll_croppic_T$test_crop_pict_BSO
mean(resultsAll_croppic_T$diff)

#croppar
resultsAll_croppar <- resultsAll[,c(1,2,3,4,5,6,7,8,9,10,11, 16, 17)]
resultsAll_croppar$withoutMoreThanWith <- ifelse(resultsAll_croppar$test_crop_parcel_noBSO > resultsAll_croppar$test_crop_parcel_BSO, T, F)
resultsAll_croppar_T <- resultsAll_croppar[resultsAll_croppar$withoutMoreThanWith == T,]
resultsAll_croppar_T$diff <- resultsAll_croppar_T$test_crop_parcel_noBSO - resultsAll_croppar_T$test_crop_parcel_BSO
mean(resultsAll_croppar_T$diff)
