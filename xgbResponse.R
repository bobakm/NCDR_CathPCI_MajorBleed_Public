MODE <- 'clinical'#'site'#'clinical'# 'all'
load(file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/five_fold_data_V8_7JUN2016.RData'))
load(file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/full_var_imp_',MODE,'_five_fold_data_V8_7JUN2016.RData', sep=''))
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/XGB_AUC_clinical_five_fold_data_V8_7JUN2016.RData")
folds <- c(1,2,3,4,5)

for(f in folds) {
  if(f == 1) {
    train.data <- train1.data[,model.importance$Feature]
    train.labels <- train1.labels
    test.data <- test1.data[,model.importance$Feature]
    test.labels <- test1.labels
  } else if (f == 2) {
    train.data <- train2.data[,model.importance$Feature]
    train.labels <- train2.labels
    test.data <- test2.data[,model.importance$Feature]
    test.labels <- test2.labels
  } else if (f == 3) {
    train.data <- train3.data[,model.importance$Feature]
    train.labels <- train3.labels
    test.data <- test3.data[,model.importance$Feature]
    test.labels <- test3.labels
  } else if (f == 4) {
    train.data <- train4.data[,model.importance$Feature]
    train.labels <- train4.labels
    test.data <- test4.data[,model.importance$Feature]
    test.labels <- test4.labels
  } else {
    train.data <- train5.data[,model.importance$Feature]
    train.labels <- train5.labels
    test.data <- test5.data[,model.importance$Feature]
    test.labels <- test5.labels
  }
  
  save('model.importance', 'train.data', 'train.labels', 'test.data', 'test.labels', file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/xgbranking/fold_',f,'_data'))
  
}

# library(xgboost)
# load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/XGB_AUC_clinical_five_fold_data_V8_7JUN2016.RData")
# load('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/full_clinical_testdata_and_labels_five_fold_data_V8_7JUN2016.RData')
# load('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/full_var_imp_clinical_five_fold_data_V8_7JUN2016.RData')
# folds <- c(1,2,3,4,5)
# 
# for(f in folds) {
#   if(f == 1) {
#     train.data <- rbind(test2.data, test3.data, test4.data, test4.)
#   } else if(f == 2) {
#     
#   } else if(f == 3) {
#     
#   } else if(f == 4) {
#     
#   } else {
#     
#   } 
# }
# 
# 
# full.data <- rbind(test1.data, test2.data, test3.data, test4.data, test5.data)
# xHat <- colMeans(full.data)
