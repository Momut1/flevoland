working_dir <- '/data/work/Ispra/Flevoland/Flevo_v5/outputs/Random_search_BBCH_Rafa/Results-all-images_py/'
dfExcludeAll <- data.frame()

for(i in list.files(working_dir, recursive = F)){
  print(i)
  
  #load in data
  df.bbch <- read.csv(file.path(working_dir,i,'cnn_output_data_check.csv'), stringsAsFactors = F)
  
  #extract bso
  df.bbch_bso_lab <- df.bbch[df.bbch$cnn_labels == 'bso0',]
  df.bbch_bso_tru <- df.bbch[df.bbch$code_bbch_surveyed == 'BSO0',]
  
  #extract tsh
  df.bbch_tsh_lab <- df.bbch[df.bbch$cnn_labels == 'tsh0',]
  df.bbch_bso_tru <- df.bbch[df.bbch$code_bbch_surveyed == 'TSH0',]
  
  dfExcludeAll <- rbind(dfExcludeAll, df.bbch_bso_lab, df.bbch_bso_tru, df.bbch_tsh_lab, df.bbch_bso_tru)
}

dfExcludeAll_noDup <- dfExcludeAll[!duplicated(dfExcludeAll$basename),]
nrow(dfExcludeAll_noDup)

table(dfExcludeAll_noDup$cnn_labels)



###check 
dfExclude_check <- data.frame(matrix(NA, nrow = length(list.files(working_dir, recursive = F)), ncol = 5))
colnames(dfExclude_check) <- c('runn', 'n_pic', 'n_pic_af', 'n_pic_bso_af', 'n_pic_tsh_af')

for(i in 1:length(list.files(working_dir, recursive = F))){
  print(list.files(working_dir, recursive = F)[i])
  
  #assign run
  dfExclude_check$runn[i] <- list.files(working_dir, recursive = F)[i]
  
  #load in data
  df.bbch <- read.csv(file.path(working_dir,i,'cnn_output_data_check.csv'), stringsAsFactors = F)
  
  #assign pre exclusion all stats
  dfExclude_check$n_pic[i] <- nrow(df.bbch)
  
  df.bbch_a <- df.bbch[!df.bbch$basename %in% dfExcludeAll_noDup$basename,]
  
  dfExclude_check$n_pic_af[i] <- nrow(df.bbch_a)
  
  df.bbch_a_bso <- df.bbch_a[grep('bso', df.bbch_a$cnn_labels),]
  dfExclude_check$n_pic_bso_af[i] <- nrow(df.bbch_a_bso)
  
  df.bbch_a_tsh <- df.bbch_a[grep('tsh', df.bbch_a$cnn_labels),]
  dfExclude_check$n_pic_tsh_af[i] <- nrow(df.bbch_a_tsh)
}

write.csv(dfExcludeAll_noDup, '/data/work/Ispra/Flevoland/Flevo_v5/inputs/dfExcludeAll_noDup.csv')



###include best model into the EXCLUDE set
working_dir_best <-  '/data/work/Ispra/Flevoland/Flevo_v5/outputs/Best_model_BBCH/Results-all-images_py/'
dfExcludeAll_best <- data.frame()
for(i in list.files(working_dir_best, recursive = F)){
  print(i)
  
  #load in data
  df.bbch <- read.csv(file.path(working_dir_best,i,'cnn_output_data_check.csv'), stringsAsFactors = F)
  
  #extract bso
  df.bbch_bso_lab <- df.bbch[df.bbch$cnn_labels == 'bso0',]
  df.bbch_bso_tru <- df.bbch[df.bbch$code_bbch_surveyed == 'BSO0',]
  
  #extract tsh
  df.bbch_tsh_lab <- df.bbch[df.bbch$cnn_labels == 'tsh0',]
  df.bbch_bso_tru <- df.bbch[df.bbch$code_bbch_surveyed == 'TSH0',]
  
  dfExcludeAll_best <- rbind(dfExcludeAll_best, df.bbch_bso_lab, df.bbch_bso_tru, df.bbch_tsh_lab, df.bbch_bso_tru)
}

dfExcludeAll_best_noDup <- dfExcludeAll_best[!duplicated(dfExcludeAll_best$basename),]
nrow(dfExcludeAll_best_noDup)

table(dfExcludeAll_best_noDup$cnn_labels)

write.csv(dfExcludeAll_best_noDup, '/data/work/Ispra/Flevoland/Flevo_v5/inputs/dfExcludeAll_best_noDup.csv')


  