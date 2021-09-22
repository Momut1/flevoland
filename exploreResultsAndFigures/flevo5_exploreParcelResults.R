rm(list=ls())
#dbClearResult(dbListResults(con)[[1]])
library(ggplot2)
library(RPostgreSQL)
library(rpostgis)
library(gridExtra)

#define global variables
working_dir <- '/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/Results-all-images_py/'
df_flevo <- read.csv('/data/work/Ispra/Flevoland/Flevo_v5/inputs/gt_BBCH.csv', stringsAsFactors = F)
df.bbch_exclude <- read.csv('/data/work/Ispra/Flevoland/Flevo_v5/inputs/dfExcludeAll_noDup_best.csv', stringsAsFactors = F)
i <- '44'
pg_hostname<-'localhost'
pg_user<-'postgres'
pg_password<-'postgres'
pg_dbname<-'postgres'
pg_port<-'5432'


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

load_and_preprocess_parcel_BBCH <- function(pathtoparcelcsv, df.bbch){
  df_bbch_parcel_44 <- read.csv(pathtoparcelcsv, stringsAsFactors = F)
  
  #get all relevant info from main pre-processed df
  for(j in 1:nrow(df_bbch_parcel_44)){
    df.bbch_int <- df.bbch[df.bbch$objectid_survey == df_bbch_parcel_44$objectid_survey[j],]
    df.bbch_int$TF <- ifelse(df.bbch_int$code_bbch_max == df.bbch_int$code_bbch_surveyed, T, F)
    
    #assign number of correct/incorrect/total images to each parcel
    df_bbch_parcel_44$NumOfPicsClassCorrect[j] <- nrow(df.bbch_int[df.bbch_int$TF == T,])
    df_bbch_parcel_44$NumOfPicsClassIncorrect[j] <- nrow(df.bbch_int[df.bbch_int$TF == F,])
    df_bbch_parcel_44$NumOfPicsTotal[j] <- df_bbch_parcel_44$NumOfPicsClassCorrect[j] + df_bbch_parcel_44$NumOfPicsClassIncorrect[j]
  }
  
  dfNumOfParcels <- as.data.frame(table(df_bbch_parcel_44$code_bbch_surveyed, df_bbch_parcel_44$TF))
  
  for(k in 1:nrow(df_bbch_parcel_44)){
    df_bbch_parcel_44$numOfParcelsPerCrop[k] <- dfNumOfParcels$Freq[dfNumOfParcels$Var1 == df_bbch_parcel_44$code_bbch_surveyed[k] & dfNumOfParcels$Var2 == df_bbch_parcel_44$TF[k]]
  }
  
  return(df_bbch_parcel_44)
}
load_and_preprocess_parcel_CROP <- function(pathtoparcelcsv, df.crop){
  df_crop_parcel_44 <- read.csv(pathtoparcelcsv, stringsAsFactors = F)
  
  #get all relevant info from main pre-processed df
  for(j in 1:nrow(df_crop_parcel_44)){
    df.crop_int <- df.crop[df.crop$objectid_survey == df_crop_parcel_44$objectid_survey[j],]
    df.crop_int$TF <- ifelse(df.crop_int$code_bbch_max == df.crop_int$code_bbch_surveyed, T, F)
    
    #assign number of correct/incorrect/total images to each parcel
    df_crop_parcel_44$NumOfPicsClassCorrect[j] <- nrow(df.crop_int[df.crop_int$TF == T,])
    df_crop_parcel_44$NumOfPicsClassIncorrect[j] <- nrow(df.crop_int[df.crop_int$TF == F,])
    df_crop_parcel_44$NumOfPicsTotal[j] <- df_crop_parcel_44$NumOfPicsClassCorrect[j] + df_crop_parcel_44$NumOfPicsClassIncorrect[j]
  }
  
  # get the number of parcels belonging to each correct/incorrect classification per class
  dfNumOfParcels <- as.data.frame(table(df_crop_parcel_44$code_surveyed, df_crop_parcel_44$TF))
  for(k in 1:nrow(df_crop_parcel_44)){
    df_crop_parcel_44$numOfParcelsPerCrop[k] <- dfNumOfParcels$Freq[dfNumOfParcels$Var1 == df_crop_parcel_44$code_surveyed[k] & dfNumOfParcels$Var2 == df_crop_parcel_44$TF[k]]
  }
  
  return(df_crop_parcel_44)
}

plot_barplot <- function(df_parcel, save.name){
  ggplot(data=df_bbch_parcel_44, aes(x=as.character(objectid_survey), y=NumOfPicsTotal, fill = TF)) +
    geom_bar(stat="identity", width=0.9)+
    #scale_x_discrete(limits = df_bbch_parcel_44$TF)+
    theme(axis.text.x = element_text(angle = 90))#+geom_text(aes(ifelse(dfParTF$TF==F,label=dfParTF$TF, NA)), vjust=1.6, color="white", size=3.5)
  ggsave(file.path(save.dir, save.name))
}
GeomLineOnHisto <- function(df_parcel){
  
  df_probabilityForCorrectIncorrect <- data.frame(matrix(NA, nrow = length(sort(unique(df_bbch_parcel_44$NumOfPicsTotal))), ncol = 3))
  colnames(df_probabilityForCorrectIncorrect) <- c('numOfPicsTotal', 'True', 'False')
  for(i in 1:length(sort(unique(df_bbch_parcel_44$NumOfPicsTotal)))){
    numOfPicsTotal_int <- sort(unique(df_bbch_parcel_44$NumOfPicsTotal))[i]
    dfInt <- df_bbch_parcel_44[df_bbch_parcel_44$NumOfPicsTotal == numOfPicsTotal_int,]
    dfInt_table <- as.data.frame(table(dfInt$TF)/nrow(dfInt))
    dfInt_table$Var1 <- as.character(dfInt_table$Var1)
    
    df_probabilityForCorrectIncorrect$numOfPicsTotal[i] <- numOfPicsTotal_int
    
    #control for instances of a single numOfPicTotal
    if(nrow(dfInt_table) > 1){
      df_probabilityForCorrectIncorrect$False[i] <- dfInt_table$Freq[dfInt_table$Var1 == 'FALSE']
      df_probabilityForCorrectIncorrect$True[i] <- dfInt_table$Freq[dfInt_table$Var1 == 'TRUE']
    }else if(nrow(dfInt_table) == 1){
      #if we have only TRUE images
      if(grepl('TRUE',dfInt_table$Var1)){
        df_probabilityForCorrectIncorrect$False[i] <- NA
        df_probabilityForCorrectIncorrect$True[i] <- dfInt_table$Freq[dfInt_table$Var1 == 'TRUE']
      }else if(grepl('FALSE',dfInt_table$Var1)){
        df_probabilityForCorrectIncorrect$False[i] <- dfInt_table$Freq[dfInt_table$Var1 == 'FALSE']
        df_probabilityForCorrectIncorrect$True[i] <- NA
      }
    }
    
  }
  
  #put NA where the proportion is the lesser number
  df_probabilityForCorrectIncorrect_withNAs <- data.frame()
  for(i in 1:length(sort(unique(df_probabilityForCorrectIncorrect$numOfPicsTotal)))){
    numOfPicsTotal_int <- sort(unique(df_probabilityForCorrectIncorrect$numOfPicsTotal))[i]
    
    dfInt <- df_probabilityForCorrectIncorrect[df_probabilityForCorrectIncorrect$numOfPicsTotal == numOfPicsTotal_int,]
    
    dfInt[dfInt == min(c(dfInt$True, dfInt$False))] <- NA 
    
    if(i == 1){
      df_probabilityForCorrectIncorrect_withNAs <- dfInt
    }else{
      df_probabilityForCorrectIncorrect_withNAs <- rbind(df_probabilityForCorrectIncorrect_withNAs, dfInt)
    }
  }
  
  
  df_probabilityForCorrectIncorrect_melt <- melt(df_probabilityForCorrectIncorrect_withNAs, id.vars = 1)
  
 p <- ggplot(df_probabilityForCorrectIncorrect_melt, aes(x=numOfPicsTotal, y=value, color = variable)) +
   geom_point(show.legend = FALSE)+
   geom_line(show.legend = FALSE)+
   coord_flip()+
   scale_color_manual(values=c("#00BFC4", "#F8766D"))+
   labs(x ="Number of images/parcel", y = "Probability")+
   theme(legend.position='none')+
   theme_classic()

  
  return(p)
  
}
plot_histo <- function(df_parcel, save.name, savetf){
  
  
  p <- ggplot(df_parcel, aes(NumOfPicsTotal, fill = TF)) +
    geom_histogram(binwidth = 1)+ 
    #scale_y_continuous(breaks = seq(0, 30, by = 10))
    #coord_flip()+
    labs(fill = "True/False \n classification \n of parcels", x ="Number of images/parcel", y = "Number of Parecls")+
    theme_classic()+ 
    #theme(legend.position="bottom")+ 
    #guides(color = FALSE)
    theme(legend.position='none')
  
  if(savetf == T){
    ggsave(p, file.path(save.dir, save.name))
  }else if(savetf == F){
    return(p)
  }  
}
plot_barplot_misscalss_BBCH <- function(df_parcel, save.name){
  missclassifiedParcels <- df_parcel[df_parcel$TF == FALSE,]
  
  ggplot(data=missclassifiedParcels, aes(x=as.factor(objectid_survey), y=NumOfPicsTotal, fill = code_bbch_surveyed)) +
    geom_bar(stat="identity", width=0.5)+
    theme(axis.text.x = element_text(angle = 45))+
    geom_text(aes(label=NumOfPicsTotal), vjust=1.6, color="black", size=3.5)
  ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')
}
plot_barplot_misscalss_CROP <- function(df_parcel, save.name){
  missclassifiedParcels <- df_parcel[df_parcel$TF == FALSE,]
  
  ggplot(data=missclassifiedParcels, aes(x=as.factor(objectid_survey), y=NumOfPicsTotal, fill = code_surveyed)) +
    geom_bar(stat="identity", width=0.5)+
    theme(axis.text.x = element_text(angle = 45))+
    geom_text(aes(label=NumOfPicsTotal), vjust=1.6, color="black", size=3.5)
  ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')
}
plot_barplot_final_BBCH <- function(df_parcel, save.name, savetf){
  
  p <- ggplot(df_parcel, aes(x=code_bbch_surveyed, y=NumOfPicsTotal, fill = TF)) +
          geom_boxplot() +
          geom_text(data = df_parcel, aes(x = code_bbch_surveyed, y = ifelse(TF == F, 150, 130),colour=TF, label = numOfParcelsPerCrop), size = 5, position = position_dodge(width = 1), show.legend = FALSE) + 
          theme_classic()+
          theme(legend.justification=c(0,0), legend.position=c(0.85,0.7))+
          #guides(color = 'legend', label = 'none')+
          labs(fill = "True/False \n classification \n of parcels", x ="Crop-BBCH class", y = "Total number of images/parcel")
  
  if(savetf == T){
    ggsave(p, file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')
  }else if(savetf == F){
    return(p)
  }  
}
plot_barplot_final_CROP <- function(df_parcel, save.name){
  ggplot(df_parcel, aes(x=code_surveyed, y=NumOfPicsTotal, fill = TF)) +
    geom_boxplot() +
    geom_text(data = df_parcel, aes(x = code_surveyed, y = ifelse(TF == F, 170, 150),colour=TF, label = numOfParcelsPerCrop), size = 5, position = position_dodge(width = 1)) + 
    theme_classic()
  ggsave(file.path(save.dir, save.name), width = 11, height = 8, dpi = 150, units = "in", device='png')
}

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

Connect_to_db <- function(user, host, port, password, dbname){
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, user=user, host=host, port=port, password=password,dbname=dbname)
  return(con)
  
}
con <- Connect_to_db(pg_user, pg_hostname,pg_port,pg_password,pg_dbname)

print(i)

#load in data
df.bbch <- load_and_preprocess(file.path(working_dir,i,'cnn_output_data_check.csv'), i)

#assign save directory
save.dir <- file.path('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/figures_Exclude_f1_tshB', i)

if(!dir.exists(save.dir)){
  dir.create(save.dir, recursive = T)
}

#correct parcel IDs
df.crop <- correct_parcelID_crop(df.bbch)
df.bbch <- correct_parcelID_bbch(df.bbch)

#do some checks
if(! all(sort(unique(df.bbch$code_bbch_max)) == sort(unique(df.bbch$code_bbch_surveyed))) & all(sort(unique(df.bbch$code_max)) == sort(unique(df.bbch$code_surveyed)))){
  stop(cat('something wrong after parcel ID recoding'))
}else{
  print('tutto be')
}

#load in and preprocess parcel data
df_bbch_parcel_44 <- load_and_preprocess_parcel_BBCH('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Random_search_BBCH_Rafa/df_bbch_parcel_44.csv', df.bbch)
df_crop_parcel_44 <- load_and_preprocess_parcel_CROP('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Random_search_BBCH_Rafa/df_crop_parcel_44.csv', df.crop)

#plot barplot
plot_barplot(df_bbch_parcel_44,'barplot_allParcelsWithNumOfPixTF_BBCH.png')
#plot_barplot(df_crop_parcel_44,'barplot_allParcelsWithNumOfPixTF_CROP.png')

#plot histogram
plot_histo(df_bbch_parcel_44,'histo_NumOfPixHisto_BBCH.png')
plot_histo(df_crop_parcel_44,'histo_NumOfPixHisto_CROP.png')

#plot barplot missclassified parcels
plot_barplot_misscalss_BBCH(df_bbch_parcel_44,'barplot_missclassParcelsBarplot_BBCH.png')
plot_barplot_misscalss_CROP(df_crop_parcel_44,'barplot_missclassParcelsBarplot_CROP.png')

#plot final barplot
plot_barplot_final_BBCH(df_bbch_parcel_44, 'barplot_final_parcelsWithTotalNumberOfPicsTF_BBCH.png', T)
plot_barplot_final_CROP(df_crop_parcel_44, 'barplot_final_parcelsWithTotalNumberOfPicsTF_CROP.png', T)

#stitch the histogram to the barplot_final
barplot_fina_bbch <- plot_barplot_final_BBCH(df_bbch_parcel_44,'barplot_missclassParcelsBarplot_BBCH.png', F)
histo_bbch <- plot_histo(df_bbch_parcel_44,'histo_NumOfPixHisto_BBCH.png', F)
geomOnHisto <- GeomLineOnHisto(df_bbch_parcel_44)
empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(), 
        panel.background=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),           
        axis.title.x=element_blank(), axis.title.y=element_blank())

p_combined <- grid.arrange(histo_bbch, empty, barplot_fina_bbch, geomOnHisto, ncol=2, nrow=2, widths=c(5, 1), heights=c(2, 5))
ggsave(file.path(save.dir, 'barplotBBCH_hist_combined.png'), width = 11, height = 8, dpi = 150, units = "in", device='png')

#extract some histo metrics
mean(df_bbch_parcel_44$NumOfPicsTotal[df_bbch_parcel_44$TF == F])
median(df_bbch_parcel_44$NumOfPicsTotal[df_bbch_parcel_44$TF == F])
Modes(df_bbch_parcel_44$NumOfPicsTotal[df_bbch_parcel_44$TF == F])
min(df_bbch_parcel_44$NumOfPicsTotal[df_bbch_parcel_44$TF == F])

mean(df_bbch_parcel_44$NumOfPicsTotal[df_bbch_parcel_44$TF == T])
median(df_bbch_parcel_44$NumOfPicsTotal[df_bbch_parcel_44$TF == T])
Modes(df_bbch_parcel_44$NumOfPicsTotal[df_bbch_parcel_44$TF == T])
min(df_bbch_parcel_44$NumOfPicsTotal[df_bbch_parcel_44$TF == T])

df_bbch_parcel_44_less50 <- df_bbch_parcel_44[df_bbch_parcel_44$NumOfPicsTotal <= 50,]
table(df_bbch_parcel_44_less50$TF)/nrow(df_bbch_parcel_44_less50)

df_bbch_parcel_44_less50_more100 <- df_bbch_parcel_44[df_bbch_parcel_44$NumOfPicsTotal > 50 & df_bbch_parcel_44$NumOfPicsTotal <= 100,]
table(df_bbch_parcel_44_less50_more100$TF)/nrow(df_bbch_parcel_44_less50_more100)

#what is the factor difference between T/F parcels
crop_bbch <- sort(unique(df_bbch_parcel_44$code_bbch_surveyed))
correctPar <- c(29,29, 55, 55, 23, 16, 22, 21, 27, 14, 23, 19, 22, 25, 12, 8, 10, 15, 15)
incorrectPar <- c(2, 1, 11, 7, 2, 7, 2, 5, 0, 6, 2, 2, 5, 7, 3, 1, 4, 4, 3)
dfParcelsCorrIncorr <- data.frame(crop_bbch, correctPar, incorrectPar)
dfParcelsCorrIncorr$factorrOf <- round(dfParcelsCorrIncorr$correctPar / dfParcelsCorrIncorr$incorrectPar, 1)


#get interesting parcels out
df_bbch_parcel_44_intParcelsF <- df_bbch_parcel_44[df_bbch_parcel_44$code_bbch_surveyed %in% c('CAR1', 'CAR4', 'MAI3', 'SBT14', 'WWH2') & df_bbch_parcel_44$TF == F,]
df_bbch_parcel_44_intParcelsF <- df_bbch_parcel_44_intParcelsF[order(df_bbch_parcel_44_intParcelsF$code_bbch_surveyed),]
df_bbch_parcel_44_intParcelsF[order(df_bbch_parcel_44_intParcelsF$objectid_survey),]

#fish out all images from these parcels
df.bbch_intParcelsF <- df.bbch[df.bbch$objectid_survey %in% df_bbch_parcel_44_intParcelsF$objectid_survey,]

#check if num of images match - both should be T
length(table(df.bbch_intParcelsF$objectid_survey)) == length(df_bbch_parcel_44_intParcelsF$NumOfPicsTotal)
table(df.bbch_intParcelsF$objectid_survey) %in% df_bbch_parcel_44_intParcelsF$NumOfPicsTotal

#make filepath on eos drive
df.bbch_intParcelsF$filepatheos <- file.path('/eos/jeodpp/data/projects/REFOCUS/data/COP4AGRI/', gsub('_','/',df.bbch_intParcelsF$basename))

write.csv(df.bbch_intParcelsF, '/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/df_bbch_intParcelsF_44.csv')

### get all miss-classified parcels and check them up
df_bbch_parcel_44_missClass <- df_bbch_parcel_44[df_bbch_parcel_44$TF == F,]
df_bbch_parcel_44_missClass$code_bbch_max <- tolower(df_bbch_parcel_44_missClass$code_bbch_max)
df_bbch_parcel_44_missClass$code_bbch_surveyed <- tolower(df_bbch_parcel_44_missClass$code_bbch_surveyed)

df_bbch_parcel_44_missClass <- df_bbch_parcel_44_missClass[order(df_bbch_parcel_44_missClass$code_bbch_surveyed),]

write.csv(df_bbch_parcel_44_missClass, '/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/df_bbch_missclassParcels_44.csv')

#distance to centroid stuff
df.bbch$distance_ratioAboveThr <- ifelse(df.bbch$distance_ratio > 0.95, 'above', 'below')
ggplot(df.bbch, aes(distance_ratio, fill = distance_ratioAboveThr)) +
  geom_histogram(binwidth = 0.01)+ 
  labs(fill = "Is distance to centroid \n above or below 0.95", x ="Distance to centroid", y = "Number of Images")+
  theme_classic()+
  theme(legend.position = c(0.25, 0.8), axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), legend.key.size = unit(1.5, 'cm'), legend.title = element_text(size=20), legend.text = element_text(size=20))+
  scale_x_continuous(breaks=c(0,0.25,0.50,0.75,1))
ggsave(file.path(save.dir, 'dfbbch_distance_ratioAboveThr.png'), width = 11, height = 8, dpi = 150, units = "in", device='png')

df.bbch_missClassParcels <- df.bbch[df.bbch$objectid_survey %in% df_bbch_parcel_44_missClass$objectid_survey,]
df.bbch_missClassParcels$distance_ratioAboveThr <- ifelse(df.bbch_missClassParcels$distance_ratio > 0.95, 'above', 'below')
ggplot(df.bbch_missClassParcels, aes(distance_ratio, fill =distance_ratioAboveThr)) +
  geom_histogram(binwidth = 0.01)+ 
  theme_classic()+
  labs(x ="Distance to centroid", y = "Number of Images")+
  theme(legend.position = 'none', axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  scale_x_continuous(breaks=c(0,0.25,0.50,0.75,1))
ggsave(file.path(save.dir, 'dfbbch_distance_ratioAboveThr_missclass.png'), width = 11, height = 8, dpi = 150, units = "in", device='png')







#database stuff
q <- dbSendQuery(con, 'select * from flevo.brp2019')
brp <- fetch(q, n=Inf)

for(top10 in top10ofallAccLevels){
  if(top10 %in% c(11, 14, 55, 63, 65)){
    df_flevo <- read.csv('/data/work/Ispra/Flevoland/tensorflow_flevoland_up.csv', stringsAsFactors = F)
    
    df.bbch <- read.csv(file.path(working_dir,top10,'cnn_output_data_check.csv'), stringsAsFactors = F)
    colnames(df.bbch) <- c('code_bbch_max', 'prob_max', 'basename', 'code_bbch_surveyed')
    
    df.bbch$filename <- paste0('/mnt/mars/Projects/2016_LANDSENSE/Data', df.bbch$basename)
    
    # sort(unique(df.bbch$code_bbch_max))
    # sort(unique(df.bbch$code_bbch_surveyed))
    # nrow(df.bbch[df.bbch$code_bbch_surveyed == '',])
    
    df.bbch <- df.bbch[df.bbch$code_bbch_max != 'tsh0',]
    df.bbch <- df.bbch[df.bbch$code_bbch_max != 'bso0',]
    df.bbch <- df.bbch[df.bbch$code_bbch_max != 'bso2',]
    df.bbch <- df.bbch[df.bbch$code_bbch_surveyed != '',]
    df.bbch <- df.bbch[!grepl('BSO',df.bbch$code_bbch_surveyed),]
    #nrow(df.bbch[df.bbch$code_bbch_surveyed == '',])
    df.bbch$code_max <- toupper(substr(df.bbch$code_bbch_max, 1, 3) )
    
    
    #sort(unique(df.bbch$code_bbch_max))
    #sort(unique(df.bbch$code_bbch_surveyed))
    
    
    df_flevo$code_bbch_surveyed <- NULL
    #df.bbch <- df.bbch[df.bbch$code_bbch_surveyed != 'ONI4',]
    #df.bbch <- df.bbch[df.bbch$code_bbch_max != 'ONI4',]
    
    df.bbch <- merge(df.bbch, df_flevo, by.x = 'filename', by.y = 'name')
    
    Modes <- function(x) {
      ux <- unique(x)
      tab <- tabulate(match(x, ux))
      ux[tab == max(tab)]
    }
    
    parcelsWithGT2<-aggregate(code_surveyed ~ objectid_survey, df.bbch, Modes)
    for(ii in 1:nrow(parcelsWithGT2)){
      if(length(parcelsWithGT2$code_surveyed[[ii]]) > 1){
        parcelID <- parcelsWithGT2$objectid_survey[[ii]]
        uniqueCode_survey <- parcelsWithGT2$code_surveyed[[ii]][2]
        df.bbch$objectid_survey[df.bbch$objectid_survey == parcelID & df.bbch$code_surveyed == uniqueCode_survey] <- parcelID+0.5
      }
    }
    
    parcelsWithBBCHpl2<-aggregate(code_bbch_surveyed ~ objectid_survey, df.bbch, Modes)
    for(iii in 1:nrow(parcelsWithBBCHpl2)){
      if(length(parcelsWithBBCHpl2$code_bbch_surveyed[[iii]]) > 1){
        parcelID <- parcelsWithBBCHpl2$objectid_survey[[iii]]
        #parcelsWithBBCHpl2check$parcelID[iii] <- parcelID
        #print(parcelID)
        
        #classOld <- c()
        #classNew <- c()
        for(j in 1:length(parcelsWithBBCHpl2$code_bbch_surveyed[[iii]])){
          #print(j)
          uniqueCode_survey <- parcelsWithBBCHpl2$code_bbch_surveyed[[iii]][j]
          #classOld <- c(classOld, uniqueCode_survey)
          #print(uniqueCode_survey)
          df.bbch$objectid_survey[df.bbch$objectid_survey == parcelID & df.bbch$code_bbch_surveyed == uniqueCode_survey] <- parcelID+(j/10)
          #df.bbch[df.bbch$objectid_survey == parcelID,]
          # parcelsWithBBCHpl2Int <-aggregate(code_bbch_surveyed ~ objectid_survey, df.bbch, Modes)
          # print(paste(parcelsWithBBCHpl2Int$objectid_survey[parcelsWithBBCHpl2Int$objectid_survey == parcelID],
          #             parcelsWithBBCHpl2Int$code_bbch_surveyed[parcelsWithBBCHpl2Int$code_bbch_surveyed == uniqueCode_survey]))
          # #print(parcelID+(j/10))
        }
        df.bbch<-df.bbch[!(df.bbch$objectid_survey==parcelID),]
        #parcelsWithBBCHpl2check$ClassOld[iii] <- paste(classOld, collapse=", ")
      }
    }
    
    #sort(unique(df.bbch$code_bbch_surveyed))
    df.bbch$code_bbch_max <- toupper(df.bbch$code_bbch_max) 
    df.bbch$TF <- ifelse(df.bbch$code_bbch_max == df.bbch$code_bbch_surveyed, TRUE, FALSE)
    
    for(k in 1:nrow(df.bbch)){
      df.bbch$localname[k] <- file.path('/data/work/Ispra/Flevoland/testSetBBCH', strsplit(df.bbch$basename, '/')[[k]][7])
    }
    
    df.bbch <- df.bbch[df.bbch$code_bbch_surveyed != 'BSO0',]
    df.bbch <- df.bbch[df.bbch$code_bbch_surveyed != 'BSO1',]
    df.bbch <- df.bbch[df.bbch$code_bbch_surveyed != 'BSO2',]
    df.bbch <- df.bbch[df.bbch$code_surveyed != 'BSO',]
    
    
    if(all(sort(unique(df.bbch$code_bbch_max)) == sort(unique(df.bbch$code_bbch_surveyed))) 
       & all(sort(unique(df.bbch$code_max)) == sort(unique(df.bbch$code_surveyed)))){
      write.csv(df.bbch, file.path('/data/work/Ispra/Flevoland/bestModelsResults/bbch/allruns/Results',top10,'dfbbch.csv'))
      
      setwd(file.path('/data/work/Ispra/Flevoland/bestModelsResults/bbch/allruns/Results',top10))
      
      system(paste0('ogr2ogr -f "PostgreSQL" PG:"host=localhost user=postgres dbname=postgres password=postgres" dfbbch.csv -nln "flevo.dfbbch',top10,'"'))
      
      
      # 0/ Connect to PG DB and set up directories (input, output, mappings) ----
      con <- Connect_to_db(pg_user, pg_hostname,pg_port,pg_password,pg_dbname) # Connect to the PG db where you want to upload all LUCAS points
      
      dbSendQuery(con, paste0('alter table flevo.dfbbch',top10,' add column geom geometry(Point,4326)'))
      dbSendQuery(con, paste0('update flevo.dfbbch',top10,' set geom = ST_SetSRID(ST_GeomFromText(ST_AsText(wkb_geometry)), 4326)'))
      
      #PARCELS
      Modes <- function(x) {
        ux <- unique(x)
        tab <- tabulate(match(x, ux))
        ux[tab == max(tab)]
      }
      
      aaggreg.bbch.classif<-aggregate(code_bbch_max ~ objectid_survey, df.bbch, Modes)
      shittyParcels <- c()
      for(i in 1:nrow(aaggreg.bbch.classif)){
        if(length(aaggreg.bbch.classif$code_bbch_max[[i]]) > 1){
          dfInt <- df.bbch[df.bbch$objectid_survey == aaggreg.bbch.classif$objectid_survey[[i]],]
          DfIntAgg <- aggregate(prob_max ~ code_bbch_max, dfInt, sum)
          
          maxCodeOnMaxProb <- DfIntAgg$code_bbch_max[which.max(DfIntAgg$prob_max)]
          aaggreg.bbch.classif$code_bbch_max[[i]] <- maxCodeOnMaxProb
          shittyParcels <- c(shittyParcels, aaggreg.bbch.classif$objectid_survey[[i]])
        }
      }
      
      shittyParcelsDF <- aaggreg.bbch.classif[aaggreg.bbch.classif$objectid_survey %in% shittyParcels,]
      
      
      #aaggreg.bbch.classif<-aggregate(code_bbch_max ~ objectid_survey, df.bbch, Mode)
      aaggreg.bbch.classif$code_bbch_max <- toupper(aaggreg.bbch.classif$code_bbch_max) 
      aggreg.bbch.survey<-aggregate(code_bbch_surveyed ~ objectid_survey, df.bbch, Modes)
      field.bbch<-merge(aaggreg.bbch.classif,aggreg.bbch.survey, by = 'objectid_survey')
      field.bbch$objectid_surveyInt <- as.integer(field.bbch$objectid_survey)
      field.bbch$TF <- ifelse(field.bbch$code_bbch_max == field.bbch$code_bbch_surveyed, TRUE, FALSE)
      #unique(field.bbch$code_bbch_surveyed)
      
      dfbbchParcel <- merge(brp, field.bbch, by.x = 'ogc_fid', by.y = 'objectid_surveyInt')
      write.csv(dfbbch63Parcel, file.path('/data/work/Ispra/Flevoland/bestModelsResults/bbch/allruns/Results',top10,'dfbbchParcel.csv'))
      
      setwd(file.path('/data/work/Ispra/Flevoland/bestModelsResults/bbch/allruns/Results',top10))
      system(paste0('ogr2ogr -f "PostgreSQL" PG:"host=localhost user=postgres dbname=postgres password=postgres" dfbbchParcel.csv -nln "flevo.dfbbchParcel',top10,'"'))
      
      
      # 0/ Connect to PG DB and set up directories (input, output, mappings) ----
      con <- Connect_to_db(pg_user, pg_hostname,pg_port,pg_password,pg_dbname) # Connect to the PG db where you want to upload all LUCAS points
      
      dbSendQuery(con, paste0('alter table flevo.dfbbchParcel',top10,' add column geom geometry(MULTIPOLYGON,4326)'))
      dbSendQuery(con, paste0('update flevo.dfbbchParcel',top10,' set geom = ST_SetSRID(ST_GeomFromText(ST_AsText(wkb_geometry)), 4326)'))
      
    }
  }
}

#min number of images for correct classification
