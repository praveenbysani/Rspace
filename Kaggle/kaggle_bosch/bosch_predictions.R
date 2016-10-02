source("bosch_eda.R")

#generate predictions


sel_feats <- colnames(df_feats)

dt_numeric_test <- fread("Input/test_numeric.csv")
train_feats <- colnames(dt_numeric_test)[colnames(dt_numeric_test) %in% sel_feats]
df_numeric_test <- subset(dt_numeric_test,select=train_feats)
df_numeric_test[is.na(df_numeric_test)] <- -1

rm(dt_numeric_test)
gc()

df_numeric_test.hex <- as.h2o(df_numeric_test,destination_frame = "df_numeric_test.hex")

h2o_rf_numeric_preds <- h2o.predict(rf_feats,df_numeric_test.hex)
kgl_rf_preds <- as.data.frame(h2o_rf_numeric_preds)
kgl_preds <- as.data.frame(list(Response=as.numeric(kgl_rf_preds$p1 > quantile(kgl_rf_preds$p1,0.995))))
kgl_preds$Id <- df_numeric_test$Id
kgl_preds <- kgl_preds[,c("Id","Response")]




