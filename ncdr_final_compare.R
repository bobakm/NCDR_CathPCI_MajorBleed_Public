#NCDR Final Comparisons
#5-fold cross-validation
library(glmnet)
library(pROC)
library(xgboost)
library(foreach)
library(doParallel)
registerDoParallel()
MODE <- 'all'#'site'#'clinical'# 'all'
load(file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/five_fold_data_V8_7JUN2016.RData')
#load(file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/var_imp_',MODE,'_five_fold_data_V7_11MAY2016.RData', sep=''))
source('./fMeasure.R')
#Existing Model in CV
folds <- c(1,2,3,4,5)

# [1] "PRPCI"      "CVD"        "PAD"        "CLD"        "PRCA"       "HDEF"       "THROM"      "STEMI"      "NVD23"      "FEMALE"     "LYTICS"     "AGE_LE70"   "AGE_GT70"  
# [14] "BMI_LE30"   "NEWDIAB1"   "NEWDIAB2"   "CKD1"       "CKD2"       "CKD4"       "SHOCKPCIS1" "SHOCKPCIS2" "SHOCKPCIS3" "SHOCKPCIS4" "SHOCKPCIS5" "NYHA123"    "NYHA4"     
# [27] "LESSCAI23"  "LESSCAI4"   "NEWSEQ2"    "NEWSEQ3"    "PRETIMINO"  "PREHGBLE13" "PREHGBGT13"

rao.vars <- c('PriorPCI', 'CVD', 'PAD', 'ChronicLungDisease', 'PriorMI', 'HDEF','THROM', 'STEMI','NVD23', 'FEMALE', 'LYTICS', 'AGELE70', 'AGEGT70',
              'BMILE30', 'NEWDIAB1', 'NEWDIAB2', 'CKD1', 'CKD2', 'CKD4', 'SHOCKPCIS1', 'SHOCKPCIS2', 'SHOCKPCIS3','SHOCKPCIS4', 'SHOCKPCIS5', 'NYHA123', 'NYHA4',
              'LESSCAI23', 'LESSCAI4', 'NEWSEQ2', 'NEWSEQ3','PRETIMINO', 'PREHGBLE13', 'PREHGBGT13')

#GLM on Rao+People
folds.auc <- vector()
folds.tp <- vector()
folds.fp <- vector()
folds.tn <- vector()
folds.fn <- vector()
list.crossval.results <- list()

for(f in c(1,2,3,4,5)) {
  cat(paste('fold: ', f, '\n'))
  list.fold <- list()
  if(f == 1) {
    train.data <- train1.data
    train.labels <- train1.labels
    test.data <- test1.data
    test.labels <- test1.labels
  } else if (f == 2) {
    train.data <- train2.data
    train.labels <- train2.labels
    test.data <- test2.data
    test.labels <- test2.labels
  } else if (f == 3) {
    train.data <- train3.data
    train.labels <- train3.labels
    test.data <- test3.data
    test.labels <- test3.labels
  } else if (f == 4) {
    train.data <- train4.data
    train.labels <- train4.labels
    test.data <- test4.data
    test.labels <- test4.labels
  } else {
    train.data <- train5.data
    train.labels <- train5.labels
    test.data <- test5.data
    test.labels <- test5.labels
  }
  
  train.data <- train.data[,which(colnames(train.data) %in% rao.vars)]
  test.data <- test.data[,which(colnames(test.data) %in% rao.vars)]
  #train.data <- train.data[,-which(grepl('HemoDynamicEquipment', colnames(train.data)))]
  #test.data <- test.data[,-which(grepl('HemoDynamicEquipment', colnames(test.data)))]
  cat(paste('f: ', f,' model building\n'))
  train.data$labels <- train.labels
  #model.bleed <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), nrounds=100, verbose=0)#cv.glmnet(x=as.matrix(train.data), y=as.factor(train.labels), family='binomial')
  #model.bleed <- cv.glmnet(as.matrix(train.data), as.factor(train.labels), family='binomial', type.measure='auc', parallel=TRUE)
  model.bleed <- glm(labels~., data=train.data, family=binomial)
  cat(paste('f: ', f,' predict\n'))
  predict.bleed <- predict(model.bleed, newdata=test.data, type='response')
  cat(paste('f: ', f,' roc building\n'))
  roc.bleed <- roc(as.numeric(test.labels),  as.numeric(predict.bleed))
  cat(paste('f: ', f,' auc: ', roc.bleed$auc,'\n'))
  #  cat(paste('f: ', f,' f building\n'))
  f.bleed <- allROC_par(predict.bleed, test.labels)
  
  folds.auc[f] <- roc.bleed$auc
  list.fold[[1]] <- model.bleed
  list.fold[[2]] <- predict.bleed
  list.fold[[3]] <- roc.bleed
  list.fold[[4]] <- f.bleed
  folds.tp[f] <- f.bleed[[1]]$tp[1]
  folds.tn[f] <- f.bleed[[1]]$tn[1]
  folds.fp[f] <- f.bleed[[1]]$fp[1]
  folds.fn[f] <- f.bleed[[1]]$fn[1]
  list.crossval.results[[f]] <- list.fold
  
}

save('folds.auc', 'list.crossval.results', file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/fulldata_raopeople_glm_five_fold_data_V8_7JUN2016.RData')


folds.auc <- vector()
folds.tp <- vector()
folds.fp <- vector()
folds.tn <- vector()
folds.fn <- vector()
list.crossval.results <- list()

for(f in c(1,2,3,4,5)) {
  cat(paste('fold: ', f, '\n'))
  list.fold <- list()
  if(f == 1) {
    train.data <- train1.data
    train.labels <- train1.labels
    test.data <- test1.data
    test.labels <- test1.labels
  } else if (f == 2) {
    train.data <- train2.data
    train.labels <- train2.labels
    test.data <- test2.data
    test.labels <- test2.labels
  } else if (f == 3) {
    train.data <- train3.data
    train.labels <- train3.labels
    test.data <- test3.data
    test.labels <- test3.labels
  } else if (f == 4) {
    train.data <- train4.data
    train.labels <- train4.labels
    test.data <- test4.data
    test.labels <- test4.labels
  } else {
    train.data <- train5.data
    train.labels <- train5.labels
    test.data <- test5.data
    test.labels <- test5.labels
  }
  
  #train.data <- train.data[,which(colnames(train.data) %in% rao.vars)]
  #test.data <- test.data[,which(colnames(test.data) %in% rao.vars)]
  train.data <- train.data[,-which(grepl('HemoDynamicEquipment', colnames(train.data)))]
  test.data <- test.data[,-which(grepl('HemoDynamicEquipment', colnames(test.data)))]
  
  cat(paste('f: ', f,' model building\n'))
  model.bleed <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), verbose=0,
                         nrounds=1000, eta=0.1, max.depth=6, objective='binary:logistic') 
  #model.bleed <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), nrounds=100, verbose=0)#cv.glmnet(x=as.matrix(train.data), y=as.factor(train.labels), family='binomial')
  #model.bleed <- cv.glmnet(as.matrix(train.data), as.factor(train.labels), family='binomial', type.measure='auc', parallel=TRUE)
  cat(paste('f: ', f,' predict\n'))
  predict.bleed <- predict(model.bleed, as.matrix(test.data))#, type='response')
  cat(paste('f: ', f,' roc building\n'))
  roc.bleed <- roc(as.numeric(test.labels),  as.numeric(predict.bleed))
  cat(paste('f: ', f,' auc: ', roc.bleed$auc,'\n'))
  #  cat(paste('f: ', f,' f building\n'))
  f.bleed <- allROC_par(predict.bleed, test.labels)
  
  folds.auc[f] <- roc.bleed$auc
  list.fold[[1]] <- model.bleed
  list.fold[[2]] <- predict.bleed
  list.fold[[3]] <- roc.bleed
  list.fold[[4]] <- f.bleed
  folds.tp[f] <- f.bleed[[1]]$tp[1]
  folds.tn[f] <- f.bleed[[1]]$tn[1]
  folds.fp[f] <- f.bleed[[1]]$fp[1]
  folds.fn[f] <- f.bleed[[1]]$fn[1]
  list.crossval.results[[f]] <- list.fold
  
}

save('folds.auc', 'list.crossval.results', file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/fulldata_raopeople_xgb_five_fold_data_V8_7JUN2016.RData')


#5-fold cross-validation
library(glmnet)
library(pROC)
library(xgboost)
library(foreach)
library(doParallel)
registerDoParallel()
MODE <- 'all'#'site'#'clinical'# 'all'
load(file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/five_fold_data_V8_7JUN2016.RData'))
#load(file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/var_imp_',MODE,'_five_fold_data_V7_11MAY2016.RData', sep=''))
source('./fMeasure.R')
#Existing Model in CV
folds <- c(1,2,3,4,5)

# [1] "PRPCI"      "CVD"        "PAD"        "CLD"        "PRCA"       "HDEF"       "THROM"      "STEMI"      "NVD23"      "FEMALE"     "LYTICS"     "AGE_LE70"   "AGE_GT70"  
# [14] "BMI_LE30"   "NEWDIAB1"   "NEWDIAB2"   "CKD1"       "CKD2"       "CKD4"       "SHOCKPCIS1" "SHOCKPCIS2" "SHOCKPCIS3" "SHOCKPCIS4" "SHOCKPCIS5" "NYHA123"    "NYHA4"     
# [27] "LESSCAI23"  "LESSCAI4"   "NEWSEQ2"    "NEWSEQ3"    "PRETIMINO"  "PREHGBLE13" "PREHGBGT13"

# rao.vars <- c('PriorPCI', 'CVD', 'PAD', 'ChronicLungDisease', 'PriorMI', 'HDEF','THROM', 'STEMI','NVD23', 'FEMALE', 'LYTICS', 'AGELE70', 'AGEGT70',
#               'BMILE30', 'NEWDIAB1', 'NEWDIAB2', 'CKD1', 'CKD2', 'CKD4', 'SHOCKPCIS1', 'SHOCKPCIS2', 'SHOCKPCIS3','SHOCKPCIS4', 'SHOCKPCIS5', 'NYHA123', 'NYHA4',
#               'LESSCAI23', 'LESSCAI4', 'NEWSEQ2', 'NEWSEQ3','PRETIMINO', 'PREHGBLE13', 'PREHGBGT13')

#GLM on Rao+People
folds.auc <- vector()
folds.tp <- vector()
folds.fp <- vector()
folds.tn <- vector()
folds.fn <- vector()
list.crossval.results <- list()

f <- 1

#for(f in c(2,3,4,5)) {
  cat(paste('fold: ', f, '\n'))
  list.fold <- list()
  if(f == 1) {
    train.data <- train1.data
    train.labels <- train1.labels
    test.data <- test1.data
    test.labels <- test1.labels
  } else if (f == 2) {
    train.data <- train2.data
    train.labels <- train2.labels
    test.data <- test2.data
    test.labels <- test2.labels
  } else if (f == 3) {
    train.data <- train3.data
    train.labels <- train3.labels
    test.data <- test3.data
    test.labels <- test3.labels
  } else if (f == 4) {
    train.data <- train4.data
    train.labels <- train4.labels
    test.data <- test4.data
    test.labels <- test4.labels
  } else {
    train.data <- train5.data
    train.labels <- train5.labels
    test.data <- test5.data
    test.labels <- test5.labels
  }
  
  #train.data <- train.data[,which(colnames(train.data) %in% rao.vars)]
  #test.data <- test.data[,which(colnames(test.data) %in% rao.vars)]
  
  cat(paste('f: ', f,' model building\n'))
  #model.bleed <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), nrounds=100, verbose=0)#cv.glmnet(x=as.matrix(train.data), y=as.factor(train.labels), family='binomial')
  model.bleed <- cv.glmnet(as.matrix(train.data), as.factor(train.labels), family='binomial', type.measure='auc', parallel=TRUE)
  cat(paste('f: ', f,' predict\n'))
  predict.bleed <- predict(model.bleed, as.matrix(test.data), type='response')
  cat(paste('f: ', f,' roc building\n'))
  roc.bleed <- roc(as.numeric(test.labels),  as.numeric(predict.bleed))
  cat(paste('f: ', f,' auc: ', roc.bleed$auc,'\n'))
  #  cat(paste('f: ', f,' f building\n'))
  f.bleed <- allROC_par(predict.bleed, test.labels)
  
  folds.auc[f] <- roc.bleed$auc
  list.fold[[1]] <- model.bleed
  list.fold[[2]] <- predict.bleed
  list.fold[[3]] <- roc.bleed
  list.fold[[4]] <- f.bleed
  folds.tp[f] <- f.bleed[[1]]$tp[1]
  folds.tn[f] <- f.bleed[[1]]$tn[1]
  folds.fp[f] <- f.bleed[[1]]$fp[1]
  folds.fn[f] <- f.bleed[[1]]$fn[1]
  list.crossval.results[[f]] <- list.fold
  
#}

save('folds.auc', 'folds.tp','folds.tn', 'folds.fp','folds.fn','list.crossval.results', file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/fulldata_glm_fold_',f,'_five_fold_data_V8_7JUN2016.RData',sep='') )
