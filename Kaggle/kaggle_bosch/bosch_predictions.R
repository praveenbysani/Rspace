source("bosch_data_prep.R")

#generate predictions



#df_feats <- process_data_original(max_lines=500000,save_fname="feats_train_numeric_500k.RData")

sel_train_cols <-c("Id",h2o_impvars,"Response")
sel_test_cols <- c("Id",h2o_impvars)

dt_numeric_train <- read_numeric_dtable("Input/train_numeric.csv",sel_train_cols)
dt_numeric_train$Response <- factor(dt_numeric_train$Response)
dt_numeric_train[is.na(dt_numeric_train)] <- -1

rf_feats_full <- generate_model(dt_numeric_train,"rf_full_data_impVars")

dt_numeric_test <- read_numeric_dtable("Input/test_numeric.csv",sel_test_cols)
dt_numeric_test[is.na(dt_numeric_test)] <- -1

df_numeric_test.hex <- as.h2o(dt_numeric_test,destination_frame = "df_numeric_test.hex")

h2o_rf_numeric_preds <- h2o.predict(rf_feats_full,df_numeric_test.hex)
kgl_rf_preds <- as.data.frame(h2o_rf_numeric_preds)
kgl_preds <- as.data.frame(list(Response=as.numeric(kgl_rf_preds$p1 > quantile(kgl_rf_preds$p1,0.995))))
kgl_preds$Id <- dt_numeric_test$Id
kgl_preds <- kgl_preds[,c("Id","Response")]


cat_classes <- c("integer",rep("factor",2140))
dt_cat_train <- fread("Input/train_categorical.csv",nrows=50000,colClasses = cat_classes,na.strings = "",
                      stringsAsFactors = TRUE)
dt_cat_train$Response <- dt_numeric_train[1:50000,]$Response

cat_train_results <- find_valid_model(dt_cat_train)



