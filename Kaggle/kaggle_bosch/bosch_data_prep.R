library(data.table)
library(Matrix)
library(h2o)
library(caret)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

#function to compute mcc eval metric
compute_mcc <- function(pred_values,true_values,threshold=0.99){
  
  tp <- as.numeric(sum(pred_values == 1 & true_values == 1))
  tn <- as.numeric(sum(pred_values == 0 & true_values == 0))
  fp <- as.numeric(sum(pred_values==1 & true_values==0))
  fn <- as.numeric(sum(pred_values == 0 & true_values==1))
  
  up <- (tp*tn) - (fp*fn)
  den <- ((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))^0.5
  return(up/den)
}

#function to identify empty elements
is.empty <- function(x){
  x %in% ""
}

#function to compute the proportion of missing values
is_useful_feature <- function(x,threshold=0.9){
  return( (sum(is.na(x))+sum(is.empty(x)))/length(x) < threshold )
}

#exploratory analysis of levels and stations
compute_level_station_maps <- function(df_numeric) {

  numeric_feats <- setdiff(colnames(df_numeric),c("Id","Response"))
  #create a map of the lines and their respective stations, stations and their respective features
  station_feats_map <- list()
  line_station_map <- list()
  
  for(feat in numeric_feats){
    station_id <- str_split(feat,"_")[[1]][2]
    line_id <- str_split(feat,"_")[[1]][1]
    if(line_id %in% names(line_station_map)){
      line_station_map[[line_id]] <- unique(c(station_id,line_station_map[[line_id]]))
    }
    else{
      line_station_map[[line_id]] <- c(station_id) 
    }
    if(station_id %in% names(station_feats_map)){
      station_feats_map[[station_id]] <- c(feat,station_feats_map[[station_id]])  
    }
    else{
      station_feats_map[[station_id]] <- c(feat)
    }
    
  }
  return(station_feats_map)
}

#prepare training data with original feature variables
process_data_original <- function(file_path="Input/train_numeric.csv", max_lines=200000,save_fname="feats_train.RData"){
  
  df_numeric <- read_csv(file_path,col_types = cols(.default=col_number()),
                         n_max=max_lines)
  #remove near zero variance variables / having lots of missing values
  features_miss_threshold <- sapply(df_numeric,is_useful_feature,threshold=0.95)
  #retain the S32, S33 and S34 variables
  s31_32_feats <- df_numeric %>% select(matches("S31|S32")) %>% colnames()
  features_s31_s32 <- colnames(df_numeric) %in% s31_32_feats
  
  df_numeric <- df_numeric[,features_miss_threshold|features_s31_s32]
  
  df_numeric[is.na(df_numeric)] <- -1
  df_numeric$Response <- factor(df_numeric$Response)
  df_feats <- df_numeric
  
  return(df_feats)
  
}
#prepare training data with summary statistics at sensor level
process_data_summary_station_level <- function(file_path="Input/train_numeric.csv", max_lines=20000,save_fname="feats_train.RData"){
  
  df_numeric <- read_csv(file_path,col_types = cols(.default=col_number()),
                         n_max=max_lines)
  
  #df_cat <- read_csv("Input/train_categorical.csv",n_max = 200000)
  
  
  #convert the type of target variable
  df_numeric$Response <- factor(df_numeric$Response)
  
  #remove near zero variance variables / having lots of missing values
  #df_numeric <- df_numeric[,!sapply(df_numeric,is_useless_feature,threshold=0.95)]
  #df_cat <- df_cat[,!sapply(df_cat,is_useless_feature,threshold=0.95)]
  
  #using tidy-data principles to pivot on station feature id
  df_numeric_tidy <-
    df_numeric %>% gather("station_feat","feat_value",-Id,-Response) 
  #save the r session space
  rm(df_numeric)
  gc()
  
  df_numeric_tidy <- df_numeric_tidy %>% separate(station_feat,into=c("line_id","station_id","feature_id"),sep="_") %>% select(-feature_id)
    
  #create summary stats for each station id 
  ## number of missing features
  ## number of missing sensors 
  df_numeric_stats$missing_sensors <- apply(df_numeric_stats,1,function(x) sum(is.na(x))/3)
  
  df_numeric_stats[is.na(df_numeric_stats)] <- -1
  df_numeric_stats$Response <- factor(df_numeric_stats$Response)
  

  #use just numeric features for first-phase
  df_feats <- df_numeric_stats
  #df_feats <- inner_join(df_numeric,df_cat,by="Id")
  save(df_feats,file = "df_numeric_feats_200k.RData")
  
  return(df_feats)
}


generate_model <- function(df_feats,save_fname="h2o_model_dir"){  
  
  local_h2o <- h2o.init(max_mem_size = "8G",nthreads = -1)
  df_feats.hex <- as.h2o(df_feats,destination_frame = "df_feats.hex")
  df_feats_split <- h2o.splitFrame(df_feats.hex,ratios = 0.75)
  df_feats_train <- df_feats_split[[1]]
  df_feats_test <- df_feats_split[[2]]
  
  ignore_vars <- c("Id")
  resp_var <- "Response"
  pred_vars <- setdiff(colnames(df_feats),ignore_vars)
  
  rf_feats <- h2o.randomForest(pred_vars,resp_var,df_feats_train,
                               ignore_const_cols=TRUE,min_rows = 20,ntrees=50,nbins_cats = 10,nbins=20)
  
  gbm_feats <- h2o.gbm(pred_vars,resp_var,df_feats_train,
                               ignore_const_cols=TRUE,min_rows = 20,ntrees=50,nbins_cats = 10,nbins=20, max_depth = 10)
  
  h2o.saveModel(rf_feats,path = "rf_feats_v1")
  
  ## validate performance on test test
  rf_preds <- h2o.predict(rf_feats,df_feats_test)
  gbm_preds <- h2o.predict(gbm_feats,df_feats_test)
  
  test_actual_preds <- as.data.frame(df_feats_test$Response)
  rf_local_preds <- as.data.frame(rf_preds)
  gbm_local_preds <- as.data.frame(gbm_preds)
  
  #computing the correct threshold for optimal MCC
  thresholds <- as.data.frame(list(thres=seq(0.985,0.999,by=0.001)))
  thresholds$rf_perf <- sapply(thresholds$thres,function(x) compute_mcc(as.numeric(rf_local_preds$p1 > quantile(rf_local_preds$p1,x))
                                                                     ,test_actual_preds$Response))
  
  thresholds$gbm_perf <- sapply(thresholds$thres,function(x) compute_mcc(as.numeric(gbm_local_preds$p1 > quantile(gbm_local_preds$p1,x))
                                                                        ,test_actual_preds$Response))
  
}









