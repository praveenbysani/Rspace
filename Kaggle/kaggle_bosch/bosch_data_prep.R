library(data.table)
library(Matrix)
library(h2o)
library(caret)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)


h2o_impvars <- c('L1_S24_F1846','L1_S24_F1672','L3_S29_F3327','L1_S24_F1723','L3_S29_F3455','L3_S33_F3859','L3_S33_F3863','L3_S29_F3427','L3_S29_F3382','L3_S33_F3861','L3_S30_F3754','L3_S29_F3412','L1_S24_F1844','L1_S24_F1581','L3_S30_F3759','L3_S29_F3348','L3_S33_F3867','L1_S24_F1842','L3_S30_F3559','L3_S30_F3494','L3_S33_F3865','L3_S30_F3499','L3_S33_F3857','L3_S29_F3436','L3_S33_F3855','L3_S29_F3421','L3_S30_F3554','L3_S30_F3744','L3_S30_F3519','L3_S30_F3534','L3_S34_F3882','L2_S26_F3062','L3_S33_F3869','L3_S29_F3357','L3_S29_F3330','L3_S30_F3749','L3_S33_F3871','L1_S24_F1758','L3_S29_F3339','L1_S24_F1695','L3_S32_F3850','L3_S29_F3433','L3_S29_F3351','L1_S24_F1632','L3_S30_F3804','L3_S29_F3379','L3_S29_F3342','L3_S34_F3876','L3_S29_F3354','L3_S29_F3404','L3_S30_F3504','L3_S29_F3324','L3_S29_F3321','L3_S29_F3376','L3_S29_F3373','L3_S29_F3318','L3_S29_F3395','L3_S29_F3367','L3_S29_F3485','L0_S0_F22','L3_S29_F3345','L3_S30_F3734','L2_S26_F3073','L3_S33_F3873','L3_S29_F3439','L3_S30_F3794','L3_S29_F3452','L2_S26_F3106','L3_S29_F3491','L3_S29_F3401','L3_S29_F3458','L3_S34_F3878','L3_S29_F3449','L0_S0_F20','L3_S30_F3514','L3_S29_F3315','L2_S26_F3051','L3_S30_F3724','L2_S26_F3040','L3_S36_F3920','L3_S30_F3704','L2_S26_F3121','L3_S30_F3669','L3_S30_F3509','L3_S30_F3769','L3_S30_F3574','L3_S29_F3370','L3_S29_F3388','L2_S26_F3036','L3_S36_F3924','L3_S29_F3336','L3_S29_F3385','L3_S35_F3896','L0_S0_F10','L3_S29_F3333','L0_S0_F8','L0_S1_F24','L3_S30_F3539','L3_S34_F3880','L3_S30_F3679')

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
read_numeric_data <- function(file_path="Input/train_numeric.csv", max_lines=200000,save_fname="feats_train.RData"){
  
  df_numeric <- read_csv(file_path,col_types = cols(.default=col_number()),
                         n_max=max_lines)
  #remove near zero variance variables / having lots of missing values
  features_miss_threshold <- sapply(df_numeric,is_useful_feature,threshold=0.9)
  #retain the S32, S33 and S34 variables
  station_feats <- df_numeric %>% select(matches("S26|S28|S31|S32")) %>% colnames()
  features_station <- colnames(df_numeric) %in% station_feats
  #TODO create a feature for s31, s32, s33 combination
  df_numeric <- df_numeric[,features_miss_threshold|features_station]
  
  df_numeric[is.na(df_numeric)] <- -1
  df_numeric$Response <- factor(df_numeric$Response)
  df_feats <- df_numeric
  
  return(df_feats)
  
}
#prepare training data with summary statistics at sensor level
process_numeric_station_level <- function(file_path="Input/train_numeric.csv", max_lines=20000,save_fname="feats_train.RData"){
  
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
  save(df_feats,file = save_fname)
  
  return(df_feats)
}

read_numeric_dtable <- function(fname="Input/train_numeric.csv",data_sel_cols){
  #data_sel_cols <- c("Id",h2o_impvars,"Response")
  df_feats <- fread(fname,select=data_sel_cols)
  return(df_feats)
}

find_valid_model <- function(df_feats){
  local_h2o <- h2o.init(max_mem_size = "8G",nthreads = -1)
  df_feats.hex <- as.h2o(df_feats,destination_frame = "df_feats.hex")
  df_feats_split <- h2o.splitFrame(df_feats.hex,ratios = 0.75,seed=999)
  df_feats_train <- df_feats_split[[1]]
  df_feats_test <- df_feats_split[[2]]
  
  ignore_vars <- c("Id")
  resp_var <- "Response"
  pred_vars <- setdiff(colnames(df_feats),ignore_vars)
  
  rf_feats_train <- h2o.randomForest(pred_vars,resp_var,df_feats_train,
                               ignore_const_cols=TRUE,min_rows = 20,ntrees=50,nbins_cats = 10)
  ## validate performance on test test
  rf_preds <- h2o.predict(rf_feats_train,df_feats_test)
  
  test_actual_preds <- as.data.frame(df_feats_test$Response)
  rf_local_preds <- as.data.frame(rf_preds)
  
  
  #computing the correct threshold for optimal MCC
  thresholds <- as.data.frame(list(thres=seq(0.71,0.99,by=0.01)))
  thresholds$rf_perf <- sapply(thresholds$thres,function(x) compute_mcc(as.numeric(rf_local_preds$p1 > quantile(rf_local_preds$p1,x))
                                                                     ,test_actual_preds$Response))

  #rf_feats_impvars <- h2o.randomForest(h2o_impvars,resp_var,df_feats.hex,
  #                                  ignore_const_cols=TRUE,min_rows = 10,ntrees=200,nbins=50,max_depth = 10)
  
  
  #rf_param_search <- h2o.grid("randomForest","rf_grid1",x=h2o_impvars,y=resp_var,training_frame=df_feats.hex,
  #            hyper_params = list(ntrees=c(100,200),nbins_cats=c(10,50),min_rows=c(10,50,100),
  #                                max_depth=c(5,10,40)), search_criteria = list(strategy = "RandomDiscrete", max_models = 42))
  
  return(list(thrshold=thresholds,model=rf_feats_train))
  
}

#build the final model on full data for kaggle test data
generate_kaggle_model <- function(df_feats,save_fname="h2o_model_dir"){  


  local_h2o <- h2o.init(max_mem_size = "8G",nthreads = -1)
  df_feats.hex <- as.h2o(df_feats,destination_frame = "df_feats.hex")
  
  ignore_vars <- c("Id")
  resp_var <- "Response"
  pred_vars <- setdiff(colnames(df_feats),ignore_vars)
  
  rf_feats_full <- h2o.randomForest(pred_vars,resp_var,df_feats.hex,
                                     ignore_const_cols=TRUE,min_rows = 10,ntrees=200,nbins=50,max_depth = 10)

  
  #gbm_feats <- h2o.gbm(pred_vars,resp_var,df_feats_train,
  #                             ignore_const_cols=TRUE,min_rows = 20,ntrees=50,nbins_cats = 10,nbins=20, max_depth = 10)
  #gbm_preds <- h2o.predict(gbm_feats,df_feats_test)
  #gbm_local_preds <- as.data.frame(gbm_preds)
  
  #thresholds$gbm_perf <- sapply(thresholds$thres,function(x) compute_mcc(as.numeric(gbm_local_preds$p1 > quantile(gbm_local_preds$p1,x))
  #                                                                      ,test_actual_preds$Response))
  return(rf_feats_full)
}









