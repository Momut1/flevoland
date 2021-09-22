rm(list=ls())
library(ggplot2)
library(dplyr)
library(scales)
library(caret)
library(reshape2)
library(gridExtra)
library(tidyr)
library(DescTools)

options(digits = 10)
options(scipen = 999)

#define global variables
working_dir <- '/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/Results-all-images_py/'
df_flevo <- read.csv('/data/work/Ispra/Flevoland/Flevo_v5/inputs/gt_BBCH.csv', stringsAsFactors = F)
df.bbch_exclude <- read.csv('/data/work/Ispra/Flevoland/Flevo_v5/inputs/dfExcludeAll_noDup_best.csv', stringsAsFactors = F)

#define table for run comparison
dfCompareAll <- data.frame()
colnames(dfCompareAll) <- c('runn', 'level', 'acc', 'f1', 'f1_2')

#define functions
load_and_preprocess <- function(pathtocnnoutcsv, run){
  #load cnn output
  df.bbch <- read.csv(pathtocnnoutcsv, stringsAsFactors = F)
  
  #remove all images from the exclude list
  df.bbch <- df.bbch[!df.bbch$basename %in% df.bbch_exclude$basename,]
  
  #remove __RAW vectors
  df.bbch <- df.bbch[,-c(grep('__RAW', colnames(df.bbch)))]
  
  #preprocess
  colnames(df.bbch)[c(grep('cnn_labels', colnames(df.bbch)))] <- 'code_bbch_max'
  colnames(df.bbch)[c(grep('cnn_values', colnames(df.bbch)))] <- 'prob_max'
  
  df.bbch <- df.bbch[!grepl('BSO',df.bbch$code_bbch_surveyed),]
  
  # print(nrow(df.bbch[df.bbch$code_bbch_max == 'tsh0',]))
  # print(nrow(df.bbch[df.bbch$code_bbch_max == 'bso0',]))
  # print(nrow(df.bbch[grepl('BSO',df.bbch$code_bbch_surveyed),]))
  
  #conver to uppercase
  df.bbch$code_max <- toupper(substr(df.bbch$code_bbch_max, 1, 3) )
  df.bbch$code_surveyed <- toupper(substr(df.bbch$code_bbch_surveyed, 1, 3) )
  df.bbch$code_bbch_max <- toupper(df.bbch$code_bbch_max)
  
  #try by removing code_bbch_surveyed and code_surveyed from the test_set and using the one from the gt like Lauri
  df.bbch$code_bbch_surveyed <- NULL
  df.bbch$code_surveyed <- NULL
  
  #merge with original data tensorflow_flevoland_up_noDUP
  df.bbch <- merge(df.bbch, df_flevo, by = 'basename')
  
  #check this crop larcel acc diff
  # print(nrow(df.bbch))
  # table(df.bbch$code_surveyed)
  # table(df.bbch$code_max)
  
  return(df.bbch)
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

confusionMatrix_calculate_PA_UA_F1_BBCH <- function(df){
  u = sort(union(df$code_bbch_surveyed,df$code_bbch_max))
  cm = table(factor(df$code_bbch_surveyed, u), factor(df$code_bbch_max, u),useNA = "ifany")
  # CONFUSION MATRIX WITH MARGINS ####
  cm.with.sum<- cbind(cm,TOTAL=margin.table(cm,margin=1))
  cm.with.sums.1<- rbind(cm.with.sum,TOTAL=margin.table(cm.with.sum,margin=2))
  
  # UA, PA; FSCORE ####
  cm.index<-confusionMatrix(cm,mode='prec_recall')
  cm.ByClass<-round(data.frame(cm.index$byClass),digit=4)
  table.byClass.1<-data.frame(UA=cm.ByClass$Recall,PA=cm.ByClass$Precision,F_score=cm.ByClass$F1)
  rownames(table.byClass.1)<-rownames(cm)
  
  return(table.byClass.1)
} 
confusionMatrix_tableForm_BBCH <- function(df, table.byClass.1){
  u = sort(union(df$code_bbch_surveyed,df$code_bbch_max))
  cm = table(factor(df$code_bbch_surveyed, u), factor(df$code_bbch_max, u),useNA = "ifany")
  # CONFUSION MATRIX WITH MARGINS ####
  cm.with.sum<- cbind(cm,TOTAL=margin.table(cm,margin=1))
  cm.with.sums.1<- rbind(cm.with.sum,TOTAL=margin.table(cm.with.sum,margin=2))
  
  #Add Precision and Recall to confusion matrix
  cm.with.sums.1 <- rbind(cm.with.sums.1, UA = c(round((table.byClass.1$UA * 100), 1), NA))
  cm.with.sums.1 <- cbind(cm.with.sums.1, PA = c(round((table.byClass.1$PA * 100), 1), NA, NA))
  
  return(cm.with.sums.1)
}
confusionMatrix_produceGrobTable_BBCH <- function(df, table.byClass.1){
  u = sort(union(df$code_bbch_surveyed,df$code_bbch_max))
  cm = table(factor(df$code_bbch_surveyed, u), factor(df$code_bbch_max, u),useNA = "ifany")
  # CONFUSION MATRIX WITH MARGINS ####
  cm.with.sum<- cbind(cm,TOTAL=margin.table(cm,margin=1))
  cm.with.sums.1<- rbind(cm.with.sum,TOTAL=margin.table(cm.with.sum,margin=2))
  
  # UA, PA; FSCORE ####
  cm.index<-confusionMatrix(cm,mode='prec_recall')
  
  #Accuracy grob table to append to confusion matrix graph
  cm.bbch.index.overal.table <- as.data.frame(format(round(cm.index$overall, 3), nsmall = 3))
  cm.bbch.index.overal.table$Metric <- c('Acc', 'Kappa', 'AccLow', 'AccUp', 'AccNull', 'AccPv', 'McnPv')
  rownames(cm.bbch.index.overal.table) <- NULL
  cm.bbch.index.overal.table <- cm.bbch.index.overal.table[,c(2,1)]
  colnames(cm.bbch.index.overal.table) <- c('Metric','Value')
  rownames(cm.bbch.index.overal.table) <- NULL
  cm.bbch.index.overal.table$Value <- as.numeric(as.character(cm.bbch.index.overal.table$Value))
  cm.bbch.index.overal.table <- cm.bbch.index.overal.table[!cm.bbch.index.overal.table$Metric %in% c('AccLow', 'AccUp', 'AccNull', 'AccPv'),]
  cm.bbch.index.overal.table[nrow(cm.bbch.index.overal.table)+1, 1] <- 'M-F1'
  cm.bbch.index.overal.table$Value[cm.bbch.index.overal.table$Metric == 'M-F1'] <- round(mean(table.byClass.1$F_score), 3)
  
  return(cm.bbch.index.overal.table)
}

confusionMatrix_calculate_PA_UA_F1_CROP <- function(df){
  u = sort(union(df$code_surveyed,df$code_max))
  cm = table(factor(df$code_surveyed, u), factor(df$code_max, u),useNA = "ifany")
  # CONFUSION MATRIX WITH MARGINS ####
  cm.with.sum<- cbind(cm,TOTAL=margin.table(cm,margin=1))
  cm.with.sums.1<- rbind(cm.with.sum,TOTAL=margin.table(cm.with.sum,margin=2))
  
  # UA, PA; FSCORE ####
  cm.index<-confusionMatrix(cm,mode='prec_recall')
  cm.ByClass<-round(data.frame(cm.index$byClass),digit=4)
  table.byClass.1<-data.frame(UA=cm.ByClass$Recall,PA=cm.ByClass$Precision,F_score=cm.ByClass$F1)
  rownames(table.byClass.1)<-rownames(cm)
  
  return(table.byClass.1)
}
confusionMatrix_tableForm_CROP <- function(df, table.byClass.1){
  u = sort(union(df$code_surveyed,df$code_max))
  cm = table(factor(df$code_surveyed, u), factor(df$code_max, u),useNA = "ifany")
  # CONFUSION MATRIX WITH MARGINS ####
  cm.with.sum<- cbind(cm,TOTAL=margin.table(cm,margin=1))
  cm.with.sums.1<- rbind(cm.with.sum,TOTAL=margin.table(cm.with.sum,margin=2))
  
  #Add Precision and Recall to confusion matrix
  cm.with.sums.1 <- rbind(cm.with.sums.1, UA = c(round((table.byClass.1$UA * 100), 1), NA))
  cm.with.sums.1 <- cbind(cm.with.sums.1, PA = c(round((table.byClass.1$PA * 100), 1), NA, NA))
  
  return(cm.with.sums.1)
}
confusionMatrix_produceGrobTable_CROP <- function(df, table.byClass.1){
  u = sort(union(df$code_surveyed,df$code_max))
  cm = table(factor(df$code_surveyed, u), factor(df$code_max, u),useNA = "ifany")
  # CONFUSION MATRIX WITH MARGINS ####
  cm.with.sum<- cbind(cm,TOTAL=margin.table(cm,margin=1))
  cm.with.sums.1<- rbind(cm.with.sum,TOTAL=margin.table(cm.with.sum,margin=2))
  
  # UA, PA; FSCORE ####
  cm.index<-confusionMatrix(cm,mode='prec_recall')
  
  #Accuracy grob table to append to confusion matrix graph
  cm.crop.index.overal.table <- as.data.frame(format(round(cm.index$overall, 3), nsmall = 3))
  cm.crop.index.overal.table$Metric <- c('Acc', 'Kappa', 'AccLow', 'AccUp', 'AccNull', 'AccPv', 'McnPv')
  rownames(cm.crop.index.overal.table) <- NULL
  cm.crop.index.overal.table <- cm.crop.index.overal.table[,c(2,1)]
  colnames(cm.crop.index.overal.table) <- c('Metric','Value')
  rownames(cm.crop.index.overal.table) <- NULL
  cm.crop.index.overal.table$Value <- as.numeric(as.character(cm.crop.index.overal.table$Value))
  cm.crop.index.overal.table <- cm.crop.index.overal.table[!cm.crop.index.overal.table$Metric %in% c('AccLow', 'AccUp', 'AccNull', 'AccPv'),]
  cm.crop.index.overal.table[nrow(cm.crop.index.overal.table)+1, 1] <- 'M-F1'
  cm.crop.index.overal.table$Value[cm.crop.index.overal.table$Metric == 'M-F1'] <- round(mean(table.byClass.1$F_score), 3)
  
  return(cm.crop.index.overal.table)
}

create_intCompareTable <- function(cm.index.overal.table, table.byClass.1, levelo, idx){
  #for the loop df_compare
  t_int <- data.frame(matrix(NA, nrow = 1, ncol = 5))
  colnames(t_int) <- c('runn', 'level', 'acc', 'f1', 'f1_2')
  t_int$level <- levelo
  t_int$runn <- idx
  t_int$acc <- cm.index.overal.table$Value[1]
  t_int$f1 <- mean(table.byClass.1$F_score)
  #t_int$f1_2 <- 2 * (mean(table.bbch.byClass.1$UA) * mean(table.bbch.byClass.1$PA)) / (mean(table.bbch.byClass.1$UA) + mean(table.bbch.byClass.1$PA))
  
  return(t_int)
}
PA_UA_F1_scatterPlot_input <- function(cm.with.sums.1, table.byClass.1, levelo, idx){
  
  #control for crop/bbch size of dataframe
  if(grepl('bbch', levelo)){
    dfToplot <- cm.with.sums.1[,ncol(cm.with.sums.1) - 1][1:19]
  }else if(grepl('crop', levelo)){
    dfToplot <- cm.with.sums.1[,ncol(cm.with.sums.1) - 1][1:10]
  }

  
  df_scplot <- data.frame(cbind(dfToplot, table.byClass.1$F_score, table.byClass.1$UA, table.byClass.1$PA))
  df_scplot$cropbbch <- rownames(df_scplot)
  df_scplot$level <- levelo
  df_scplot$runn <- idx
  colnames(df_scplot) <- c('total', 'f1', 'ua', 'pa', 'cropbbch', 'level')
  rownames(df_scplot) <- NULL
  
  return(df_scplot)
}
melt_and_order_df <- function(cm.with.sums.1){
  
  # melt
  df.bbch.melt <- melt(cm.with.sums.1)
  
  #order to have confusion matrix margins at bottom and right
  df.bbch.melt <- df.bbch.melt %>%
    mutate(Var1 = factor(Var1), # alphabetical order by default
           Var2 = factor(Var2, levels = rev(unique(Var2))))
  
  return(df.bbch.melt)
}
plot_confusionMatrix_BBCH <- function(df.melt, df.sums, cm.index.overal.table, save.name){
  
  ggplot(df.melt, aes(Var1, Var2)) +
    geom_tile(aes(fill = value), colour = "white") +
    #scale_fill_gradientn(limits = c(0,300),colours=c("navyblue", "darkmagenta", "darkorange1"),breaks=b, labels=format(b))
    scale_fill_distiller(limits = c(1,max(sort(df.sums$value[df.sums$Var1 != 'TOTAL' | df.sums$Var2 != 'TOTAL' ]))),type = 'div', palette = "RdYlGn",name="Number of\n Images (raw)", direction = 1)+
    #scale_fill_gradientn(colours = c("white", "red", "green"), limits = c(1,max(df.bbch.sums$value[df.bbch.sums$Var1 != 'TOTAL' | df.bbch.sums$Var2 != 'TOTAL'])),name="Number of\n Images (raw)") +
    geom_text(aes(label = ifelse(value > 0 ,as.character(value),'')), color = 'gray32')+
    geom_tile(data=df.sums, fill = 'gray')+
    geom_text(data=df.sums, aes(label = as.character(value)))+
    theme(axis.text.x = element_text(angle = -90))+
    geom_vline(xintercept = 2.5, linetype="dashed", color = "black", size=1.5)+#CAR
    geom_vline(xintercept = 3.5, linetype="dashed", color = "black", size=1.5)+#GMA
    geom_vline(xintercept = 4.5, linetype="dashed", color = "black", size=1.5)+#GRA
    geom_vline(xintercept = 6.5, linetype="dashed", color = "black", size=1.5)+#MAI
    geom_vline(xintercept = 8.5, linetype="dashed", color = "black", size=1.5)+#ONI
    geom_vline(xintercept = 12.5, linetype="dashed", color = "black", size=1.5)+#POT
    geom_vline(xintercept = 14.5, linetype="dashed", color = "black", size=1.5)+#SBT
    geom_vline(xintercept = 15.5, linetype="dashed", color = "black", size=1.5)+#SCR
    geom_vline(xintercept = 16.5, linetype="dashed", color = "black", size=1.5)+#VEG
    #geom_vline(xintercept = 20.5, linetype="dashed", color = "black", size=1.5)+#WWH
    #geom_vline(xintercept = 23.5, linetype="dashed", color = "black", size=1.5)+
    labs(x = 'Label')+
    labs(y = 'Prediction')+
    scale_x_discrete(position = "top")+
    theme(legend.justification =  'top') +
    annotation_custom(tableGrob(cm.index.overal.table, rows = NULL), xmin=0, xmax=46.2, ymin=0, ymax=10)
  ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')
}
plot_confusionMatrix_CROP <- function(df.melt, df.sums, cm.index.overal.table, save.name){
  
  ggplot(df.melt, aes(Var1, Var2)) +
    geom_tile(aes(fill = value), colour = "white") +
    #scale_fill_gradientn(limits = c(0,300),colours=c("navyblue", "darkmagenta", "darkorange1"),breaks=b, labels=format(b))
    scale_fill_distiller(limits = c(1,max(sort(df.sums$value[df.sums$Var1 != 'TOTAL' | df.sums$Var2 != 'TOTAL']))),type = 'div', palette = "RdYlGn",name="Number of\n Images (raw)", direction = 1)+
    geom_text(aes(label = ifelse(value > 0 ,as.character(value),'')), color = 'gray32')+
    geom_tile(data=df.sums, fill = 'gray')+
    geom_text(data=df.sums, aes(label = as.character(value)))+
    theme(axis.text.x = element_text(angle = -90))+
    labs(x = 'Label')+
    labs(y = 'Prediction')+
    scale_x_discrete(position = "top")+
    theme(legend.justification =  'top') +
    annotation_custom(tableGrob(cm.index.overal.table, rows = NULL), xmin=9.9, xmax=17, ymin=4, ymax=5)
  ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')
}

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
majorityVoting_BBCH <- function(df){
  
  #get modes for parcels in terms of cnn output
  aaggreg.bbch.classif<-aggregate(code_bbch_max ~ objectid_survey, df, Modes)
  shittyParcels <- c()
  for(iiii in 1:nrow(aaggreg.bbch.classif)){
    if(length(aaggreg.bbch.classif$code_bbch_max[[iiii]]) > 1){
      dfInt <- df[df$objectid_survey == aaggreg.bbch.classif$objectid_survey[[iiii]],]
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
  
  #shittyParcelsDF <- aaggreg.bbch.classif[aaggreg.bbch.classif$objectid_survey %in% shittyParcels,]
  
  #cast to uppercase
  aaggreg.bbch.classif$code_bbch_max <- toupper(aaggreg.bbch.classif$code_bbch_max)
  
  #get modes for parcels in terms of groundtruth
  aggreg.bbch.survey<-aggregate(code_bbch_surveyed ~ objectid_survey, df.bbch, Modes)
  field.bbch<-merge(aaggreg.bbch.classif,aggreg.bbch.survey, by = 'objectid_survey')
  field.bbch$TF <- field.bbch$code_bbch_max == field.bbch$code_bbch_surveyed
  
  print(sort(unique(field.bbch$code_bbch_surveyed)))
  
  return(field.bbch)
}
majorityVoting_CROP <- function(df){
  
  aaggreg.crop.classif<-aggregate(code_max ~ objectid_survey, df, Modes)
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
  
  #shittyParcelsDF <- aaggreg.crop.classif[aaggreg.crop.classif$objectid_survey %in% shittyParcels,]
  
  #cast to uppercase
  aaggreg.crop.classif$code_max <- toupper(aaggreg.crop.classif$code_max)
  
  #get modes for parcels in terms of groundtruth
  aggreg.crop.survey<-aggregate(code_surveyed ~ objectid_survey, df.crop, Modes)
  field.crop<-merge(aaggreg.crop.classif,aggreg.crop.survey, by = 'objectid_survey')
  field.crop$TF <- field.crop$code_max == field.crop$code_surveyed
  
  print(unique(field.crop$code_surveyed))
  
  return(field.crop)
}

#BBCH
for(i in list.files(working_dir, recursive = F)){
  print(i)
  
  #load in data
  df.bbch <- load_and_preprocess(file.path(working_dir,i,'cnn_output_data_check.csv'), i)
  
  #assign save directory
  save.dir <- file.path('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/figures_Exclude_f1_tshB_func', i)
  
  if(!dir.exists(save.dir)){
    dir.create(save.dir, recursive = T)
  }
  
  #correct parcel IDs
  df.crop <- correct_parcelID_crop(df.bbch)
  df.bbch <- correct_parcelID_bbch(df.bbch)
  
  #do some checks
  if(! all(sort(unique(df.bbch$code_bbch_max)) == sort(unique(df.bbch$code_bbch_surveyed))) & all(sort(unique(df.bbch$code_max)) == sort(unique(df.bbch$code_surveyed)))){
    stop(cat('something wrong after parcel ID recoding'))
  }
  
  #check some stuff for why crop parcel acc is different
  #table(df.bbch$code_surveyed)
  #table(df.bbch$code_max)
  #print(nrow(df.crop))
  
  ################################ BBCH PICTURE LEVEL #################################
  
  #preprocess for confusion Matrix
  table.bbch.byClass.1 <- confusionMatrix_calculate_PA_UA_F1_BBCH(df.bbch)
  cm.bbch.with.sums.1 <- confusionMatrix_tableForm_BBCH(df.bbch, table.bbch.byClass.1)
  cm.bbch.index.overal.table <- confusionMatrix_produceGrobTable_BBCH(df.bbch, table.bbch.byClass.1)
  
  #create table for model comparison
  t_bbchpic <- create_intCompareTable(cm.bbch.index.overal.table, table.bbch.byClass.1, 'bbchpic', i)
  
  #create input for PA/UA/F1 scatter plot
  df_scplot_bbch <- PA_UA_F1_scatterPlot_input(cm.bbch.with.sums.1, table.bbch.byClass.1, 'bbchpic', i)
  
  #melt and order the confusion matrix
  df.bbch.melt <- melt_and_order_df(cm.bbch.with.sums.1)

  #collect rows and columns to exclude from coloring scheme and leave in neutral gray
  df.bbch.sums <- df.bbch.melt[df.bbch.melt$Var2 %in% c('TOTAL', 'PA', 'UA') | df.bbch.melt$Var1 %in% c('TOTAL', 'PA', 'UA') ,]
  
  #plot and save confusion matrix figure
  plot_confusionMatrix_BBCH(df.bbch.melt, df.bbch.sums, cm.bbch.index.overal.table, 'test_bbch_pict_noBSO.png')
  
  ################################ CROP PICTURE LEVEL #################################
  
  #preprocess for confusion Matrix
  table.crop.byClass.1 <- confusionMatrix_calculate_PA_UA_F1_CROP(df.crop)
  cm.crop.with.sums.1 <- confusionMatrix_tableForm_CROP(df.crop, table.crop.byClass.1)
  cm.crop.index.overal.table <- confusionMatrix_produceGrobTable_CROP(df.crop, table.crop.byClass.1)
  
  #create table for model comparison
  t_croppic <- create_intCompareTable(cm.crop.index.overal.table, table.crop.byClass.1, 'croppic', i)
  
  #create input for PA/UA/F1 scatter plot
  df_scplot_crop <- PA_UA_F1_scatterPlot_input(cm.crop.with.sums.1, table.crop.byClass.1, 'croppic', i)
  
  #melt and order the confusion matrix
  df.crop.melt <- melt_and_order_df(cm.crop.with.sums.1)
  
  #collect rows and columns to exclude from coloring scheme and leave in neutral gray
  df.crop.sums <- df.crop.melt[df.crop.melt$Var2 %in% c('TOTAL', 'PA', 'UA') | df.crop.melt$Var1 %in% c('TOTAL', 'PA', 'UA') ,]
  
  #plot and save confusion matrix figure
  plot_confusionMatrix_CROP(df.crop.melt, df.crop.sums, cm.crop.index.overal.table, 'test_crop_pict_noBSO.png')
  
  ################################### BBCH PARCEL LEVEL #########################
  
  df.bbch <- df.bbch[!is.na(df.bbch$objectid_survey),]

  #do majority voting and create df for input to confusion matrix functions
  df.bbch.parcel <- majorityVoting_BBCH(df.bbch)
  
  #preprocess for confusion Matrix
  table.bbch.byClass.1.parcel <- confusionMatrix_calculate_PA_UA_F1_BBCH(df.bbch.parcel)
  cm.bbch.with.sums.1.parcel <- confusionMatrix_tableForm_BBCH(df.bbch.parcel, table.bbch.byClass.1.parcel)
  cm.bbch.index.overal.table.parcel <- confusionMatrix_produceGrobTable_BBCH(df.bbch.parcel, table.bbch.byClass.1.parcel)

  #create table for model comparison
  t_bbchpar <- create_intCompareTable(cm.bbch.index.overal.table.parcel, table.bbch.byClass.1.parcel, 'bbchpar', i)
  
  #create input for PA/UA/F1 scatter plot
  df_scplot_bbch_parcel <- PA_UA_F1_scatterPlot_input(cm.bbch.with.sums.1.parcel, table.bbch.byClass.1.parcel, 'bbchpar', i)
  
  #melt and order the confusion matrix
  df.bbch.melt.parcel <- melt_and_order_df(cm.bbch.with.sums.1.parcel)
  
  #collect rows and columns to exclude from coloring scheme and leave in neutral gray
  df.bbch.sums.parcel <- df.bbch.melt.parcel[df.bbch.melt.parcel$Var2 %in% c('TOTAL', 'UA', 'PA') | df.bbch.melt.parcel$Var1 %in% c('TOTAL', 'UA', 'PA') ,]
  
  #plot and save confusion matrix figure
  plot_confusionMatrix_BBCH(df.bbch.melt.parcel, df.bbch.sums.parcel, cm.bbch.index.overal.table.parcel, 'test_bbch_parcel_noBSO.png')
  
  ################################### CROP PARCEL LEVEL #########################

  df.crop.parcel <- majorityVoting_CROP(df.crop)
  
  #preprocess for confusion Matrix
  table.crop.byClass.1.parcel <- confusionMatrix_calculate_PA_UA_F1_CROP(df.crop.parcel)
  cm.crop.with.sums.1.parcel <- confusionMatrix_tableForm_CROP(df.crop.parcel, table.crop.byClass.1.parcel)
  cm.crop.index.overal.table.parcel <- confusionMatrix_produceGrobTable_CROP(df.crop.parcel, table.crop.byClass.1.parcel)
  
  #create table for model comparison
  t_croppar <- create_intCompareTable(cm.crop.index.overal.table.parcel, table.crop.byClass.1.parcel, 'croppar', i)
  
  #create input for PA/UA/F1 scatter plot
  df_scplot_crop_parcel <- PA_UA_F1_scatterPlot_input(cm.crop.with.sums.1.parcel, table.crop.byClass.1.parcel, 'croppar', i)

  #melt and order the confusion matrix
  df.crop.melt.parcel <- melt_and_order_df(cm.crop.with.sums.1.parcel)
  
  #collect rows and columns to exclude from coloring scheme and leave in neutral gray
  df.crop.sums.parcel <- df.crop.melt.parcel[df.crop.melt.parcel$Var2 %in% c('TOTAL', 'UA', 'PA') | df.crop.melt.parcel$Var1 %in% c('TOTAL', 'UA', 'PA') ,]
  
  #plot and save confusion matrix figure
  plot_confusionMatrix_CROP(df.crop.melt.parcel, df.crop.sums.parcel, cm.crop.index.overal.table.parcel, 'test_crop_parcel_noBSO.png')
  
  ################################### ALL OTHER OUTPUT #########################
  
  #collect all results in one df
  dfCompareAll <- rbind(t_bbchpic, t_croppic, t_bbchpar, t_croppar)
  
  #collect all scatter plot resutls in oen df 
  df_scplot_all <- rbind(df_scplot_bbch, df_scplot_crop, df_scplot_bbch_parcel, df_scplot_crop_parcel)
  df_scplot_all$crop <- substr(df_scplot_all$cropbbch, 1, 3)
}

write.csv(dfCompareAll, '/data/work/Ispra/Flevoland/Flevo_v5/outputs/Random_search_BBCH_Rafa/Results-all-images_R_f1.csv', row.names = F)
write.csv(df_scplot_all, '/data/work/Ispra/Flevoland/Flevo_v5/outputs/Random_search_BBCH_Rafa/df_scplot_all.csv')

#parcelstuff momentary output for top run 44
write.csv(df.bbch.parcel, '/data/work/Ispra/Flevoland/Flevo_v5/outputs/Random_search_BBCH_Rafa/df_bbch_parcel_44.csv')
write.csv(df.crop.parcel, '/data/work/Ispra/Flevoland/Flevo_v5/outputs/Random_search_BBCH_Rafa/df_crop_parcel_44.csv')
