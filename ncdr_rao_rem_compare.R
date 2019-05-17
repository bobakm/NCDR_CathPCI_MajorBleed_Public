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
source('./setup_crossval.R')
#Existing Model in CV
folds <- c(1,2,3,4,5)

#GLM on Rao+People
folds.auc <- vector()
list.crossval.results <- list()

rem.crossval <- crossval.list(ncdr.rem.var, ncdr.rem.var$Bleed, 5)
rao.vars <- c('PriorPCI', 'CVD', 'PAD', 'ChronicLungDisease', 'PriorMI', 'HDEF','THROM', 'STEMI','NVD23', 'FEMALE', 'LYTICS', 'AGELE70', 'AGEGT70',
              'BMILE30', 'NEWDIAB1', 'NEWDIAB2', 'CKD1', 'CKD2', 'CKD4', 'SHOCKPCIS1', 'SHOCKPCIS2', 'SHOCKPCIS3','SHOCKPCIS4', 'SHOCKPCIS5', 'NYHA123', 'NYHA4',
              'LESSCAI23', 'LESSCAI4', 'NEWSEQ2', 'NEWSEQ3','PRETIMINO', 'PREHGBLE13', 'PREHGBGT13')
for(f in c(2,3,4,5)) {
  cat(paste('fold: ', f, '\n'))
  list.fold <- list()
  train.data <- rem.crossval[[1]][[f]]
  test.data <- rem.crossval[[2]][[f]]
  train.labels <- rem.crossval[[3]][[f]]
  test.labels <- rem.crossval[[4]][[f]]
  
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
  
  folds.auc[f] <- roc.bleed$auc
  
}
