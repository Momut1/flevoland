library(ggplot2)
library(dplyr)
library(scales)
library(caret)
library(reshape2)
library(magick)
library(stringr)

working_dir <- '/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/figures/'
if(!dir.exists(file.path(working_dir))){
  dir.create(file.path(working_dir), recursive = T)
}

###### USER - PRODUCER GRAPH
bestModels <- c(44,107,127)
for(modell in bestModels){
  
  if(!dir.exists(file.path(working_dir, modell))){
    dir.create(file.path(working_dir, modell), recursive = T)
  }
  
  df.bbch <- read.csv(paste0('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/Results-all-images_py/',modell,'/cnn_output_data_check.csv'), stringsAsFactors = F)
  head(df.bbch)
  
  #remove unneccessary columns
  df.bbch <- df.bbch[,-c(grep('RAW', colnames(df.bbch)))]
  colnames(df.bbch)[grep('cnn_labels', colnames(df.bbch))] <- 'code_bbch_max'
  colnames(df.bbch)[grep('cnn_values', colnames(df.bbch))] <- 'prob_max'
  head(df.bbch)
  
  sort(unique(df.bbch$code_bbch_max))
  sort(unique(df.bbch$code_bbch_surveyed))
  nrow(df.bbch[df.bbch$code_bbch_surveyed == '',])
  
  
  df.bbch <- df.bbch[df.bbch$code_bbch_max != 'tsh0',]
  df.bbch <- df.bbch[df.bbch$code_bbch_max != 'bso0',]
  df.bbch <- df.bbch[df.bbch$code_bbch_max != 'bso2',]
  df.bbch <- df.bbch[df.bbch$code_bbch_surveyed != '',]
  df.bbch <- df.bbch[!grepl('BSO',df.bbch$code_bbch_surveyed),]
  nrow(df.bbch[df.bbch$code_bbch_surveyed == '',])
  df.bbch$code_max <- toupper(substr(df.bbch$code_bbch_max, 1, 3) )
  df.bbch$code_bbch_max <- toupper(df.bbch$code_bbch_max)
  
  u.bbch = sort(union(df.bbch$code_bbch_surv,df.bbch$code_bbch_max))
  cm.bbch = table(factor(df.bbch$code_bbch_surv, u.bbch), factor(df.bbch$code_bbch_max, u.bbch),useNA = "ifany")
  # CONFUSION MATRIX WITH MARGINS ####
  cm.bbch.with.sum<- cbind(cm.bbch,TOTAL=margin.table(cm.bbch,margin=1))
  cm.bbch.with.sums.1<- rbind(cm.bbch.with.sum,TOTAL=margin.table(cm.bbch.with.sum,margin=2))
  
  # UA, PA; FSCORE ####
  cm.bbch.index<-confusionMatrix(cm.bbch,mode='prec_recall')
  cm.bbch.ByClass<-round(data.frame(cm.bbch.index$byClass),digit=4)
  table.bbch.byClass.1<-data.frame(UA=cm.bbch.ByClass$Recall,PA=cm.bbch.ByClass$Precision,F_score=cm.bbch.ByClass$F1)
  rownames(table.bbch.byClass.1)<-rownames(cm.bbch)
  
  
  # melt
  df.bbch.melt <- melt(cm.bbch.with.sums.1)
  
  #calcualte accuracies
  df.bbch.procPA <- data.frame()
  df.bbch.procUA <- data.frame()
  for(i in unique(df.bbch.melt$Var2)){
    #i <- 'BSO0'
    # PRODUCER ACCUCARY
    df.int <- df.bbch.melt[df.bbch.melt$Var2 == i,]
    df.int$PA <- round((df.int$value / df.int$value[df.int$Var1 == 'TOTAL'] *100),3)
    
    # USER ACCURACY
    df.int2 <- df.bbch.melt[df.bbch.melt$Var1 == i,]
    df.int2$UA <- round(df.int2$value / df.int2$value[df.int2$Var2 == 'TOTAL'] *100,3)
    
    # MERGERS
    df.bbch.procPA <- rbind(df.bbch.procPA, df.int)
    df.bbch.procUA <- rbind(df.bbch.procUA, df.int2)
  }
  
  #PA == RECALL
  df.bbch.procPA <- df.bbch.procPA[!is.na(df.bbch.procPA$Var1),]
  df.bbch.procPA$PA <- as.numeric(df.bbch.procPA$PA)
  df.bbch.procPA$nameid <- paste0(df.bbch.procPA$Var1, df.bbch.procPA$Var2)
  df.bbch.procPA <- df.bbch.procPA[df.bbch.procPA$Var1 != 'TSH0',]
  df.bbch.procPA <- df.bbch.procPA[df.bbch.procPA$Var2 != 'TSH0',]
  
  #UA == PRECISION
  df.bbch.procUA <- df.bbch.procUA[!is.na(df.bbch.procUA$Var1),]
  df.bbch.procUA$UA <- as.numeric(df.bbch.procUA$UA)
  df.bbch.procUA$nameid <- paste0(df.bbch.procUA$Var1, df.bbch.procUA$Var2)
  df.bbch.procUA <- df.bbch.procUA[df.bbch.procUA$Var1 != 'TSH',]
  df.bbch.procUA <- df.bbch.procUA[df.bbch.procUA$Var2 != 'TSH',]
  
  # F1
  df.bbch.procF1 <- merge(df.bbch.procPA, df.bbch.procUA, by = 'nameid')
  df.bbch.procF1 <- df.bbch.procF1[,-c(6,7,8)]
  colnames(df.bbch.procF1) <- c('nameid', 'Var1', 'Var2', 'value', 'PA', 'UA')
  df.bbch.procF1$F1 <- 2 * ((df.bbch.procF1$UA * df.bbch.procF1$PA) / (df.bbch.procF1$UA + df.bbch.procF1$PA))
  df.bbch.procF1$F1[is.na(df.bbch.procF1$F1)] <- 0
  df.bbch.procF1 <- df.bbch.procF1[df.bbch.procF1$Var1 != 'TSH',]
  df.bbch.procF1 <- df.bbch.procF1[df.bbch.procF1$Var2 != 'TSH',]
  
  #####  accuracy signatures
  df.bbch.procPA$crop <- (str_extract(df.bbch.procPA$Var2, "[aA-zZ]+"))
  df.bbch.procPA$bbch <- as.numeric(str_extract(df.bbch.procPA$Var2, "[0-9]+"))
  
  # PA
  df.PA.all.compare <- data.frame()
  for(i in unique(df.bbch.procPA$crop)){
    df.bbch.procPA.int <- df.bbch.procPA[df.bbch.procPA$Var1 == df.bbch.procPA$Var2,]
    df.bbch.procPA.int2 <- df.bbch.procPA.int[df.bbch.procPA.int$crop == i,]
    df.PA.all.compare <- rbind(df.PA.all.compare, df.bbch.procPA.int2)
  }
  
  # UA
  df.bbch.procUA$crop <- (str_extract(df.bbch.procUA$Var2, "[aA-zZ]+"))
  df.bbch.procUA$bbch <- as.numeric(str_extract(df.bbch.procUA$Var2, "[0-9]+"))
  
  df.UA.all.compare <- data.frame()
  for(i in unique(df.bbch.procUA$crop)){
    df.bbch.procUA.int <- df.bbch.procUA[df.bbch.procUA$Var1 == df.bbch.procUA$Var2,]
    df.bbch.procUA.int2 <- df.bbch.procUA.int[df.bbch.procUA.int$crop == i,]
    df.UA.all.compare <- rbind(df.UA.all.compare, df.bbch.procUA.int2)
  }
  
  # F1
  df.bbch.procF1$crop <- (str_extract(df.bbch.procF1$Var2, "[aA-zZ]+"))
  df.bbch.procF1$bbch <- as.numeric(str_extract(df.bbch.procF1$Var2, "[0-9]+"))
  
  df.F1.all.compare <- data.frame()
  for(i in unique(df.bbch.procF1$crop)){
    df.bbch.procF1.int <- df.bbch.procF1[df.bbch.procF1$Var1 == df.bbch.procF1$Var2,]
    df.bbch.procF1.int2 <- df.bbch.procF1.int[df.bbch.procF1.int$crop == i,]
    df.F1.all.compare <- rbind(df.F1.all.compare, df.bbch.procF1.int2)
  }
  
  df.ALL.bbch.proc.compare <- merge(df.PA.all.compare, df.UA.all.compare, by = 'nameid') 
  df.ALL.bbch.proc.compare <- merge(df.ALL.bbch.proc.compare, df.F1.all.compare, by = 'nameid') 
  df.ALL.bbch.proc.compare <- df.ALL.bbch.proc.compare[, -c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
  colnames(df.ALL.bbch.proc.compare) <- c('PA', 'UA', 'F1', 'crop', 'bbch')
  
  for(cropp in sort(unique(df.bbch$code_max))){
    df.crop_int <- df.ALL.bbch.proc.compare[df.ALL.bbch.proc.compare$crop == cropp,]
    df.crop_int <- melt(df.crop_int, c('crop', 'bbch'))
    colnames(df.crop_int) <- c('Crop', 'BBCH', 'Metric', 'Score')

    # Conditional statement to be used in plot
    df.crop_int_MAXF1 <- df.crop_int[df.crop_int$Score == max(df.crop_int$Score[df.crop_int$Metric == 'F1']),]
    blue.bold.italic.16.text <- element_text(face = "bold.italic", color = "red", size = 35)
    con <- ifelse(df.crop_int$BBCH == df.crop_int_MAXF1$BBCH , 'red', 'black')
    
    if(length(unique(df.crop_int$BBCH)) > 1){
      p2<-ggplot(data=df.crop_int, aes(x=factor(BBCH), y=Score, group = Metric)) +
        geom_point(aes(color = Metric), size = 5) +
        geom_point(shape = 1,size = 7,colour = "black")+
        geom_line(aes(color = Metric),  size = 3)+
        scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
        ylim(0, 100)+
        ggtitle(cropp)+
        labs(x = 'BBCH stage')+
        labs(y = 'Score')+ 
        theme(plot.title = element_text(color = "black", size = 50, face = "bold"))+
        theme(legend.position = 'none')+ 
        theme(axis.text.x = element_text(face = "bold", color = "black",size = 30),axis.text.y = element_text(face = "bold", color = "black",size = 30))+
        theme(axis.text=element_text(size=30),axis.title=element_text(size=30,face="bold"))+ 
        theme(axis.text.x = element_text(angle = 0, hjust = 0, colour = con))
        #theme(axis.text = ifelse(df.crop_int$BBCH == df.crop_int_MAXF1$BBCH,blue.bold.italic.16.text,NA)) 
        #theme(axis.text = blue.bold.italic.16.text)
      ggsave(file.path(working_dir, modell, paste0('metric',cropp,'.png')))
    }
  }
  
  
  ##### IMAGE MAGIC STITCH
  allFiles <- list.files(file.path(working_dir, '44'))
  metricFiles <- allFiles[grep('metric', allFiles)]
  metricFiles <- file.path(working_dir, '44',metricFiles)
  
  legend <- file.path('/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/metricLegend.PNG')
  
  image1<-image_read(as.character(metricFiles[1]))
  
  image2<-image_read(as.character(metricFiles[2]))
  
  image3<-image_read(as.character(metricFiles[3]))
  
  image4<-image_read(as.character(metricFiles[4]))
  
  image5<-image_read(as.character(metricFiles[5]))
  
  image6<-image_read(as.character(metricFiles[6]))
  
  # image7<-image_read(as.character(metricFiles[7]))
  # 
  # image8<-image_read(as.character(metricFiles[8]))
  # 
  # image9<-image_read(as.character(metricFiles[9]))
  # 
  # image10<-image_read(as.character(metricFiles[10]))
  # 
  # image11<-image_read(as.character(metricFiles[11]))
  # 
  # image12<-image_read(as.character(metricFiles[12]))
  # 
  legend <- image_read(as.character(legend))
  
  # img.row1<-image_append(c(image_border(image_background(image1, "hotpink"), "#000080", "20x10"), 
  #                          image_border(image_background(image2, "hotpink"), "#000080", "20x10"),
  #                          image_border(image_background(image3, "hotpink"), "#000080", "20x10")))
  # img.row1.appended<-image_append(image_scale(img.row1, "x600"))
  # 
  # img.row2 <- image_append(c(image_border(image_background(image4, "hotpink"), "#000080", "20x10"), 
  #                            image_border(image_background(image5, "hotpink"), "#000080", "20x10"),
  #                            image_border(image_background(image6, "hotpink"), "#000080", "20x10")))
  # img.row2.appended<-image_append(image_scale(img.row2, "x600"))
  # 
  # img.row3 <- image_append(c(image_border(image_background(image7, "hotpink"), "#000080", "20x10"), 
  #                            image_border(image_background(image8, "hotpink"), "#000080", "20x10"),
  #                            image_border(image_background(image9, "hotpink"), "#000080", "20x10")))
  # img.row3.appended<-image_append(image_scale(img.row3, "x600"))
  # 
  # img.row4 <- image_append(c(image_border(image_background(image10, "hotpink"), "#000080", "20x10"), 
  #                            image_border(image_background(image11, "hotpink"), "#000080", "20x10"),
  #                            image_border(image_background(image12, "hotpink"), "#000080", "20x10")))
  # img.row4.appended<-image_append(image_scale(img.row4, "x600"))
  # 
  # mosaic.4by4<-image_append(image_scale(c(img.row1.appended,img.row2.appended, img.row3.appended, img.row4.appended),"x600"), stack = TRUE)
  # mosaic.4by4.legend <- image_append(image_scale(c(mosaic.4by4,legend),"x800"))
  # ########################################
  # img.row1<-image_append(c(image1, image2, image3))
  # img.row1.appended<-image_append(image_scale(img.row1, "x600"))
  # 
  # img.row2 <- image_append(c(image4, image5, image6))
  # img.row2.appended<-image_append(image_scale(img.row2, "x600"))
  # 
  # img.row3 <- image_append(c(image7, image8, image9))
  # img.row3.appended<-image_append(image_scale(img.row3, "x600"))
  # 
  # img.row4 <- image_append(c(image10,image11,image12))
  # img.row4.appended<-image_append(image_scale(img.row4, "x600"))
  # 
  # mosaic.4by4<-image_append(image_scale(c(img.row1.appended,img.row2.appended, img.row3.appended, img.row4.appended),"x600"), stack = TRUE)
  # mosaic.4by4.legend <- image_append(image_scale(c(mosaic.4by4,legend),"x800"))
  # ######################
  
  img.row1<-image_append(c(image1, image2, image3))
  img.row1.appended<-image_append(image_scale(img.row1, "x600"))
  
  img.row2 <- image_append(c(image4, image5, image6))
  img.row2.appended<-image_append(image_scale(img.row2, "x600"))
  
  # img.row3 <- image_append(c(image7, legend))#, image8, image10))
  # img.row3.appended<-image_append(image_scale(img.row3, "x600"))
  
  #img.row4 <- image_append(c(image11))
  #img.row4.appended<-image_append(image_scale(img.row4, "x600"))
  
  mosaic.4by4<-image_append(image_scale(c(img.row1.appended,img.row2.appended),"x600"), stack = TRUE)#, img.row4.appended),"x600"), stack = TRUE)
  mosaic.4by4.legend <- image_append(image_scale(c(mosaic.4by4,image_scale(legend, "3200")),"x400"))
  image_write(mosaic.4by4.legend, path = file.path(working_dir, modell, "final.png"), format = "png")
}
