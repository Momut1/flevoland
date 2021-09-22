rm(list=ls())
library(stringi)
library(strex)

#declare global vars
working_dir <- '/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/Results-all-images_py/'
flevo_gt <- '/data/work/Ispra/Flevoland/Flevo_v5/inputs/gt_BBCH.csv'
df_flevo <- read.csv(flevo_gt, stringsAsFactors = F)
df.bbch_exclude <- read.csv('/data/work/Ispra/Flevoland/Flevo_v5/inputs/dfExcludeAll_noDup_best.csv', stringsAsFactors = F)

i <- '44'

save.dir <- file.path('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/figures_Exclude_f1_tshB', i)
if(!dir.exists(save.dir)){
  dir.create(save.dir, recursive = T)
}

pathtocnnoutcsv_cur <- file.path(working_dir,i,'cnn_output_data_check.csv')

remove_cnnvalues_of_BSO0_TSH0 <- function(df){
  for(ii in 1:nrow(df)){
    
    #extract index of classes to be removed (BSO0, TSH0)
    label_rowInt <- as.vector(stri_extract_all_words(df$cnn_labels__RAW[ii], simplify = TRUE))
    idx_BSO0 <- grep('bso0',label_rowInt)
    idx_TSH0 <- grep('tsh0',label_rowInt)
    
    #remove BSO0 and TSH0
    label_rowInt_minusExlClass <- label_rowInt[-c(idx_BSO0, idx_TSH0)]
    
    #extract decimal numbers
    value_rowInt <- str_extract_numbers(df$cnn_values__RAW[ii], decimal = T, sci = T)
    
    #remove BSO0 and TSH0
    value_rowInt_minusExlClass <- value_rowInt[[1]][-c(idx_BSO0, idx_TSH0)]
    
    #assign to new rows in df
    df$cnn_labels__RAW_new[ii] <- paste(label_rowInt_minusExlClass, sep = ',', collapse = ', ')
    df$cnn_values__RAW_new[ii] <- paste(value_rowInt_minusExlClass, sep = ',', collapse = ', ')
  }
  
  return(df)
}
order_cnnvalues_4_binerizedClasses <- function(df.bbch){
  df.bbch_new <- data.frame()
  binerizedClasses <- sort(unique(df.bbch$code_bbch_surveyed))
  for(iii in 1:length(binerizedClasses)){
    print(iii)
    df.bbch_cropbbch <- df.bbch[df.bbch$code_bbch_surveyed == binerizedClasses[iii],]
    
    #   for(j in 1:nrow(df.bbch_cropbbch)){
    #     
    #     print(j)
    #     #extract index of classes in binerizedClasses[iii]
    #     label_rowInt <- as.vector(stri_extract_all_words(df.bbch_cropbbch$cnn_labels__RAW_new[j], simplify = TRUE))
    #     idx_binerizedClasses_iii <- grep(tolower(binerizedClasses[iii]),label_rowInt)
    #     
    #     #extract index of probability of classes in binerizedClasses[iii]
    #     #extract decimal numbers
    #     value_rowInt <- str_extract_numbers(df.bbch_cropbbch$cnn_values__RAW_new[j], decimal = T, sci = T)[[1]]
    #     prob_binerizedClasses_iii <- value_rowInt[idx_binerizedClasses_iii]
    #     
    #     new_sortedCorrect <- rep(0, length(binerizedClasses))
    #     new_sortedCorrect[iii] <- prob_binerizedClasses_iii
    #     df.bbch_cropbbch$cnn_values__RAW_new_sortedCorrect[j] <- paste(new_sortedCorrect, sep=', ', collapse = ', ')
    #   }
    #   
    #   if(iii == 1){
    #     df.bbch_new <- df.bbch_cropbbch
    #   }else{
    #     df.bbch_new <- rbind(df.bbch_new, df.bbch_cropbbch)
    #   }
    # }
    
    for(j in 1:nrow(df.bbch_cropbbch)){
      
      print(j)
      #extract index of each classes
      label_rowInt <- as.vector(stri_extract_all_words(df.bbch_cropbbch$cnn_labels__RAW_new[j], simplify = TRUE))
      idx_CAR1 <- grep(tolower('CAR1'),label_rowInt)
      idx_CAR4 <- grep(tolower('CAR4'),label_rowInt)
      idx_GMA2 <- grep(tolower('GMA2'),label_rowInt)
      idx_GRA1 <- grep(tolower('GRA1'),label_rowInt)
      idx_MAI3 <- grep(tolower('MAI3'),label_rowInt)
      idx_MAI7 <- grep(tolower('MAI7'),label_rowInt)
      idx_ONI1 <- grep(tolower('ONI1'),label_rowInt)
      idx_ONI48 <- grep(tolower('ONI48'),label_rowInt)
      idx_POT1 <- grep(tolower('POT1'),label_rowInt)
      idx_POT6 <- grep(tolower('POT6'),label_rowInt)
      idx_POT8 <- grep(tolower('POT8'),label_rowInt)
      idx_POT9 <- grep(tolower('POT9'),label_rowInt)
      idx_SBT14 <- grep(tolower('SBT14'),label_rowInt)
      idx_SBT39 <- grep(tolower('SBT39'),label_rowInt)
      idx_SCR2 <- grep(tolower('SCR2'),label_rowInt)
      idx_VEG1 <- grep(tolower('VEG1'),label_rowInt)
      idx_WWH2 <- grep(tolower('WWH2'),label_rowInt)
      idx_WWH3 <- grep(tolower('WWH3'),label_rowInt)
      idx_WWH7 <- grep(tolower('WWH7'),label_rowInt)
      
      #extract index of probability of each class
      #extract decimal numbers
      value_rowInt <- str_extract_numbers(df.bbch_cropbbch$cnn_values__RAW_new[j], decimal = T, sci = T)[[1]]
      prob_CAR1 <- value_rowInt[idx_CAR1]
      prob_CAR4 <- value_rowInt[idx_CAR4]
      prob_GMA2 <- value_rowInt[idx_GMA2]
      prob_GRA1 <- value_rowInt[idx_GRA1]
      prob_MAI3 <- value_rowInt[idx_MAI3]
      prob_MAI7 <- value_rowInt[idx_MAI7]
      prob_ONI1 <- value_rowInt[idx_ONI1]
      prob_ONI48 <- value_rowInt[idx_ONI48]
      prob_POT1 <- value_rowInt[idx_POT1]
      prob_POT6 <- value_rowInt[idx_POT6]
      prob_POT8 <- value_rowInt[idx_POT8]
      prob_POT9 <- value_rowInt[idx_POT9]
      prob_SBT14 <- value_rowInt[idx_SBT14]
      prob_SBT39 <- value_rowInt[idx_SBT39]
      prob_SCR2 <- value_rowInt[idx_SCR2]
      prob_VEG1 <- value_rowInt[idx_VEG1]
      prob_WWH2 <- value_rowInt[idx_WWH2]
      prob_WWH3 <- value_rowInt[idx_WWH3]
      prob_WWH7 <- value_rowInt[idx_WWH7]
      
      new_sortedCorrect <- c(prob_CAR1, prob_CAR4, prob_GMA2, prob_GRA1, prob_MAI3, prob_MAI7, prob_ONI1, prob_ONI48, prob_POT1, prob_POT6, prob_POT8, prob_POT9, prob_SBT14, prob_SBT39, prob_SCR2, prob_VEG1, prob_WWH2, prob_WWH3, prob_WWH7)
      df.bbch_cropbbch$cnn_values__RAW_new_sortedCorrect[j] <- paste(new_sortedCorrect, sep=', ', collapse = ', ')
    }
    
    if(iii == 1){
      df.bbch_new <- df.bbch_cropbbch
    }else{
      df.bbch_new <- rbind(df.bbch_new, df.bbch_cropbbch)
    }
  }
  return(df.bbch_new)
}
load_and_preprocess_leaveRAW <- function(pathtocnnoutcsv, run){
  #load cnn output
  df.bbch <- read.csv(pathtocnnoutcsv, stringsAsFactors = F)
  
  #remove all images from the exclude list
  df.bbch <- df.bbch[!df.bbch$basename %in% df.bbch_exclude$basename,]
  
  #remove all BSO0
  df.bbch <- df.bbch[!grepl('BSO',df.bbch$code_bbch_surveyed),]
  
  #remove from cnn_values__RAW the probas of BSO0 and THS0
  df.bbch <- remove_cnnvalues_of_BSO0_TSH0(df.bbch)
  
  #order the proba_max in terms of the order of the binerized output (alphabetical)
  df.bbch_new <- order_cnnvalues_4_binerizedClasses(df.bbch)
  
  #preprocess
  colnames(df.bbch_new)[c(grep('^cnn_labels$', colnames(df.bbch_new)))] <- 'code_bbch_max'
  colnames(df.bbch_new)[c(grep('^cnn_values$', colnames(df.bbch_new)))] <- 'prob_max'
  
  # print(nrow(df.bbch[df.bbch$code_bbch_max == 'tsh0',]))
  # print(nrow(df.bbch[df.bbch$code_bbch_max == 'bso0',]))
  # print(nrow(df.bbch[grepl('BSO',df.bbch$code_bbch_surveyed),]))
  
  #conver to uppercase
  df.bbch_new$code_max <- toupper(substr(df.bbch_new$code_bbch_max, 1, 3) )
  df.bbch_new$code_surveyed <- toupper(substr(df.bbch_new$code_bbch_surveyed, 1, 3) )
  df.bbch_new$code_bbch_max <- toupper(df.bbch_new$code_bbch_max)
  
  #try by removing code_bbch_surveyed and code_surveyed from the test_set and using the one from the gt like Lauri
  df.bbch_new$code_bbch_surveyed <- NULL
  df.bbch_new$code_surveyed <- NULL
  
  #merge with original data tensorflow_flevoland_up_noDUP
  df.bbch_new <- merge(df.bbch_new, df_flevo, by = 'basename')
  
  #check this crop larcel acc diff
  # print(nrow(df.bbch))
  # table(df.bbch$code_surveyed)
  # table(df.bbch$code_max)
  
  return(df.bbch_new)
}

correct_parcelID_crop <- function(df.bbch){
  #make a copy of  df.bbch for the crop level performance evaluation
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
  
  return(df.crop)
}
correct_parcelID_bbch <- function(df.bbch){
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
  
  return(df.bbch)
}

sumProbalities <- function(vectorr){
  #convert to numeric
  vectorr_num <- as.numeric(vectorr)
  
  vectorr_num_sum <- sum(vectorr_num)
  
  return(vectorr_num_sum)
}
summedCNNvalues_forCropLevel <- function(df.crop){
  
  bbchOccuranceDF <- unique(df.crop[c("code_surveyed","bbch")])
  bbchOccuranceDF_count <- as.data.frame(bbchOccuranceDF %>% count(code_surveyed))
  bbchOccuranceDF_count$endIdx <- cumsum(bbchOccuranceDF_count$n)
  bbchOccuranceDF_count$startIdx <- c(1,3,4,5,7,9,13,15,16,17)
  
  
  for(ii in 1:nrow(df.crop)){
    #extract bbch vector of probabilities
    cnn_values__RAW_new_sortedCorrect_int <- strsplit(df.crop$cnn_values__RAW_new_sortedCorrect[ii], ', ')[[1]]
    
    bbchOccuranceDF_count_int <- bbchOccuranceDF_count
    bbchOccuranceDF_count_int$newSummedProbas <- 0
    for(jj in 1:nrow(bbchOccuranceDF_count_int)){
      newWSummedProba_int <- sumProbalities(cnn_values__RAW_new_sortedCorrect_int[bbchOccuranceDF_count_int$startIdx[jj]:bbchOccuranceDF_count_int$endIdx[jj]])
      bbchOccuranceDF_count_int$newSummedProbas[jj] <- newWSummedProba_int
    }
    
    df.crop$cnn_values__RAW_new_sortedCorrect_SUMCROP[ii] <- paste(bbchOccuranceDF_count_int$newSummedProbas, sep = ',', collapse = ', ')
  }
  
  return(df.crop)
}

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
range01 <- function(x){
  x / sum(x)}

preprocess_parcels_bbch <- function(df.bbch){
  df.bbch <- df.bbch[!is.na(df.bbch$objectid_survey),]
  
  #majority vote on sum of max probability
  aaggreg.bbch.classif<-aggregate(code_bbch_max ~ objectid_survey, df.bbch, Modes)
  shittyParcels <- c()
  for(iiii in 1:nrow(aaggreg.bbch.classif)){
    if(length(aaggreg.bbch.classif$code_bbch_max[[iiii]]) > 1){
      dfInt <- df.bbch[df.bbch$objectid_survey == aaggreg.bbch.classif$objectid_survey[[iiii]],]
      DfIntAgg <- aggregate(prob_max ~ code_bbch_max, dfInt, sum)
      # DfIntAggCount <- as.data.frame(t(t(table(dfInt$code_bbch_max))))
      # DfIntAggCount$Var2 <- NULL
      # colnames(DfIntAggCount) <- c('code_bbch_max', 'count')
      # DfIntAggFinal <- merge(DfIntAgg, DfIntAggCount, by = 'code_bbch_max')
      # DfIntAggFinal$prob_max_weighted <- DfIntAggFinal$prob_max / DfIntAggFinal$count
      maxCodeOnMaxProb <- DfIntAgg$code_bbch_max[which.max(DfIntAgg$prob_max)]
      aaggreg.bbch.classif$code_bbch_max[[iiii]] <- maxCodeOnMaxProb
      shittyParcels <- c(shittyParcels, aaggreg.bbch.classif$objectid_survey[[iiii]])
    }
  }
  
  shittyParcelsDF <- aaggreg.bbch.classif[aaggreg.bbch.classif$objectid_survey %in% shittyParcels,]
  
  
  
  #aaggreg.bbch.classif<-aggregate(code_bbch_max ~ objectid_survey, df.bbch, Modes)
  aaggreg.bbch.classif$code_bbch_max <- toupper(aaggreg.bbch.classif$code_bbch_max)
  aggreg.bbch.survey<-aggregate(code_bbch_surveyed ~ objectid_survey, df.bbch, Modes)
  field.bbch<-merge(aaggreg.bbch.classif,aggreg.bbch.survey, by = 'objectid_survey')
  field.bbch$TF <- field.bbch$code_bbch_max == field.bbch$code_bbch_surveyed
  
  table(field.bbch$code_bbch_surveyed)
  
  #assign prob_max for each parcel based on the mean of prob_maxs for all the images belonging to this parcel
  for(iii in 1:nrow(field.bbch)){
    df.bbch_intToParcel <- df.bbch[df.bbch$objectid_survey == field.bbch$objectid_survey[iii],]
    field.bbch$prob_max_mean[iii] <- mean(df.bbch_intToParcel$prob_max)
  }
  
  return(field.bbch)
}
create_mPRC_inputdf_bbchPar <- function(parcel.bbch, df.bbch){
  #dataframe looks like - 1 col/class, 1 col/class proba. Each class col must be either binerized (1/0) with only 1 class per row being 1 - the true class for this example;
  ## the probability cols must be named accordingly as well
  multiPRCInput <- data.frame(matrix(0, nrow=nrow(parcel.bbch), ncol=2))
  colnames(multiPRCInput) <- c('code_bbch_surveyed', 'prob_arranged')
  allclasses <- sort(unique(parcel.bbch$code_bbch_surveyed))
  
  
  for(ii in 1:nrow(parcel.bbch)){
    
    #assign TRUE class
    multiPRCInput$code_bbch_surveyed[ii] <- parcel.bbch$code_bbch_surveyed[ii]
    
    #populate probability columns
    #want to take the sum of probabilities for each of the classes from each of the images belonging to the parcel 
    #fetch parcel ID
    parcelID_int <- parcel.bbch$objectid_survey[ii]
    #fetch all images that belong to this parcel with the matched label
    df.bbch_parcelID_int <- df.bbch[df.bbch$objectid_survey == parcelID_int,]
    df.bbch_parcelID_int_aggregatedProbas <- aggregate(prob_max ~ code_bbch_max, df.bbch_parcelID_int, FUN = sum)
    #and scale them between 0 and 1
    df.bbch_parcelID_int_aggregatedProbas_scaled <- as.data.frame(range01(df.bbch_parcelID_int_aggregatedProbas$prob_max))
    #format df
    colnames(df.bbch_parcelID_int_aggregatedProbas_scaled) <- c('prob_max_scaled')
    df.bbch_parcelID_int_aggregatedProbas_scaled$code_bbch_max <- df.bbch_parcelID_int_aggregatedProbas$code_bbch_max
    #fill in 0s for the rest of the classes
    dfClasses_int <- data.frame(matrix(0, nrow = length(allclasses), ncol = 2))
    colnames(dfClasses_int) <- c('code_bbch_max', 'prob_max_scaled')
    dfClasses_int$code_bbch_max <- allclasses
    
    for(iii in 1:nrow(dfClasses_int)){
      if(dfClasses_int$code_bbch_max[iii] %in% df.bbch_parcelID_int_aggregatedProbas_scaled$code_bbch_max){
        dfClasses_int$prob_max_scaled[iii] <- df.bbch_parcelID_int_aggregatedProbas_scaled$prob_max_scaled[df.bbch_parcelID_int_aggregatedProbas_scaled$code_bbch_max == dfClasses_int$code_bbch_max[iii]]
      }
    }
    
    multiPRCInput$prob_arranged[ii] <- paste(dfClasses_int$prob_max_scaled, sep=', ', collapse = ', ')
  }
  
  return(multiPRCInput)
}

preprocess_parcels_crop <- function(df.crop){
  
  aaggreg.crop.classif<-aggregate(code_max ~ objectid_survey, df.crop, Modes)
  shittyParcels <- c()
  for(iiiiii in 1:nrow(aaggreg.crop.classif)){
    if(length(aaggreg.crop.classif$code_max[[iiiiii]]) > 1){
      dfInt <- df.crop[df.crop$objectid_survey == aaggreg.crop.classif$objectid_survey[[iiiiii]],]
      DfIntAgg <- aggregate(prob_max ~ code_max, dfInt, sum)
      # DfIntAggCount <- as.data.frame(t(t(table(dfInt$code_bbch_max))))
      # DfIntAggCount$Var2 <- NULL
      # colnames(DfIntAggCount) <- c('code_bbch_max', 'count')
      # DfIntAggFinal <- merge(DfIntAgg, DfIntAggCount, by = 'code_bbch_max')
      # DfIntAggFinal$prob_max_weighted <- DfIntAggFinal$prob_max / DfIntAggFinal$count
      maxCodeOnMaxProb <- DfIntAgg$code_max[which.max(DfIntAgg$prob_max)]
      aaggreg.crop.classif$code_max[[iiiiii]] <- maxCodeOnMaxProb
      shittyParcels <- c(shittyParcels, aaggreg.crop.classif$objectid_survey[[iiiiii]])
    }
  }
  
  shittyParcelsDF <- aaggreg.crop.classif[aaggreg.crop.classif$objectid_survey %in% shittyParcels,]
  
  
  aaggreg.crop.classif$code_max <- toupper(aaggreg.crop.classif$code_max)
  aggreg.crop.survey<-aggregate(code_surveyed ~ objectid_survey, df.crop, Modes)
  field.crop<-merge(aaggreg.crop.classif,aggreg.crop.survey, by = 'objectid_survey')
  unique(field.crop$code_surveyed)
  
  return(field.crop)
}
create_mPRC_inputdf_cropPar <- function(parcel.crop, df.crop){
  #dataframe looks like - 1 col/class, 1 col/class proba. Each class col must be either binerized (1/0) with only 1 class per row being 1 - the true class for this example;
  ## the probability cols must be named accordingly as well
  multiPRCInput <- data.frame(matrix(0, nrow=nrow(parcel.crop), ncol=2))
  colnames(multiPRCInput) <- c('code_surveyed', 'prob_arranged')
  allclasses <- sort(unique(parcel.crop$code_surveyed))
  
  
  for(ii in 1:nrow(parcel.crop)){
    
    #assign TRUE class
    multiPRCInput$code_surveyed[ii] <- parcel.crop$code_surveyed[ii]
    
    #populate probability columns
    #want to take the sum of probabilities for each of the classes from each of the images belonging to the parcel 
    #fetch parcel ID
    parcelID_int <- parcel.crop$objectid_survey[ii]
    #fetch all images that belong to this parcel with the matched label
    df.crop_parcelID_int <- df.crop[df.crop$objectid_survey == parcelID_int,]
    df.crop_parcelID_int_aggregatedProbas <- aggregate(prob_max ~ code_max, df.crop_parcelID_int, FUN = sum)
    #and scale them between 0 and 1
    df.crop_parcelID_int_aggregatedProbas_scaled <- as.data.frame(range01(df.crop_parcelID_int_aggregatedProbas$prob_max))
    #format df
    colnames(df.crop_parcelID_int_aggregatedProbas_scaled) <- c('prob_max_scaled')
    df.crop_parcelID_int_aggregatedProbas_scaled$code_max <- df.crop_parcelID_int_aggregatedProbas$code_max
    #fill in 0s for the rest of the classes
    dfClasses_int <- data.frame(matrix(0, nrow = length(allclasses), ncol = 2))
    colnames(dfClasses_int) <- c('code_max', 'prob_max_scaled')
    dfClasses_int$code_max <- allclasses
    
    for(iii in 1:nrow(dfClasses_int)){
      if(dfClasses_int$code_max[iii] %in% df.crop_parcelID_int_aggregatedProbas_scaled$code_max){
        dfClasses_int$prob_max_scaled[iii] <- df.crop_parcelID_int_aggregatedProbas_scaled$prob_max_scaled[df.crop_parcelID_int_aggregatedProbas_scaled$code_max == dfClasses_int$code_max[iii]]
      }
    }
    
    multiPRCInput$prob_arranged[ii] <- paste(dfClasses_int$prob_max_scaled, sep=', ', collapse = ', ')
  }
  
  return(multiPRCInput)
}

#work it
df.bbch <- load_and_preprocess_leaveRAW(pathtocnnoutcsv_cur, 44)

df.crop <- correct_parcelID_crop(df.bbch)
df.crop <- summedCNNvalues_forCropLevel(df.crop)

df.bbch <- correct_parcelID_bbch(df.bbch)

#checks
all(sort(unique(df.bbch$code_bbch_max)) == sort(unique(df.bbch$code_bbch_surveyed)))
all(sort(unique(df.bbch$code_max)) == sort(unique(df.bbch$code_surveyed)))


#check some stuff for why crop parcel acc is different
table(df.bbch$code_surveyed)
table(df.bbch$code_max)
print(nrow(df.crop))

#export to check if we get the same results with this dfbbch44
#write.csv(df.bbch, file.path(save.dir, 'df.bbch_prcintput_TESTIFRESULTSARETHESAME.csv'))
##same CM

#explort dfs for spyder3 prc visualization
write.csv(as.data.frame(cbind(df.bbch$code_bbch_surveyed, df.bbch$cnn_labels__RAW_new, df.bbch$cnn_values__RAW_new_sortedCorrect ,df.bbch$prob_max)), file.path(save.dir, 'df.bbch_prcintput.csv'), row.names = F)
#df.bbch <- read.csv(file.path(save.dir, 'df.bbch_prcintput.csv'))

#crop picture level
write.csv(as.data.frame(cbind(df.crop$code_surveyed, df.crop$cnn_labels__RAW_new ,df.crop$cnn_values__RAW_new_sortedCorrect_SUMCROP)), file.path(save.dir, 'df.crop_prcintput.csv'))


#bbch parcel level
parcel.bbch <- preprocess_parcels_bbch(df.bbch)
multiPRCInput_bbch_par <-  create_mPRC_inputdf_bbchPar(parcel.bbch, df.bbch)

write.csv(multiPRCInput_bbch_par, file.path(save.dir, 'df.bbch_par_prcintput.csv'))

#crop parcel level
parcel.crop <- preprocess_parcels_crop(df.crop)
multiPRCInput_crop_par <-  create_mPRC_inputdf_cropPar(parcel.crop, df.crop)

write.csv(multiPRCInput_crop_par, file.path(save.dir, 'df.crop_par_prcintput.csv'))

