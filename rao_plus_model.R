#NCDR Final Comparisons
#5-fold cross-validation
library(glmnet)
library(pROC)
library(xgboost)
library(foreach)
library(doParallel)
#registerDoParallel()
MODE <- 'clinical'#'site'#'clinical'# 'all'
cat(paste('LOADING data\n'))
load(file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/five_fold_data_V8_7JUN2016.RData')
#load(file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/var_imp_',MODE,'_five_fold_data_V7_11MAY2016.RData', sep=''))
source('./fMeasure.R')
#Existing Model in CV
folds <- c(1,2,3,4,5)

# SHOCKPRIORANDPCI <- as.numeric(ncdr.data.full$DCARSHOCK == 1)
# ncdr.data.full$SHOCKPRIORANDPCI <- SHOCKPRIORANDPCI
# ncdr.rem.var$SHOCKPRIORANDPCI <- as.numeric(ncdr.rem.var$DCARSHOCK == 1)
# ncdr.rao.var$SHOCKPRIORANDPCI <- as.numeric(ncdr.rao.var$DCARSHOCK == 1)
# train1.data$SHOCKPRIORANDPCI <- as.numeric(train1.data$DCARSHOCK == 1)
# train2.data$SHOCKPRIORANDPCI <- as.numeric(train2.data$DCARSHOCK == 1)
# train3.data$SHOCKPRIORANDPCI <- as.numeric(train3.data$DCARSHOCK == 1)
# train4.data$SHOCKPRIORANDPCI <- as.numeric(train4.data$DCARSHOCK == 1)
# train5.data$SHOCKPRIORANDPCI <- as.numeric(train5.data$DCARSHOCK == 1)
# test1.data$SHOCKPRIORANDPCI <- as.numeric(test1.data$DCARSHOCK == 1)
# test2.data$SHOCKPRIORANDPCI <- as.numeric(test2.data$DCARSHOCK == 1)
# test3.data$SHOCKPRIORANDPCI <- as.numeric(test3.data$DCARSHOCK == 1)
# test4.data$SHOCKPRIORANDPCI <- as.numeric(test4.data$DCARSHOCK == 1)
# test5.data$SHOCKPRIORANDPCI <- as.numeric(test5.data$DCARSHOCK == 1)
# 
# save.image(file='~/NCDR-ACC/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/five_fold_data_V8_7JUN2016.RData')

# [1] "PRPCI"      "CVD"        "PAD"        "CLD"        "PRCA"       "HDEF"       "THROM"      "STEMI"      "NVD23"      "FEMALE"     "LYTICS"     "AGE_LE70"   "AGE_GT70"  
# [14] "BMI_LE30"   "NEWDIAB1"   "NEWDIAB2"   "CKD1"       "CKD2"       "CKD4"       "SHOCKPCIS1" "SHOCKPCIS2" "SHOCKPCIS3" "SHOCKPCIS4" "SHOCKPCIS5" "NYHA123"    "NYHA4"     
# [27] "LESSCAI23"  "LESSCAI4"   "NEWSEQ2"    "NEWSEQ3"    "PRETIMINO"  "PREHGBLE13" "PREHGBGT13"

rao.vars <- c('PriorPCI', 'PriorCVD', 'PriorPAD', 'ChronicLungDisease', 'PriorCardiacArrest','THROM', 'STEMI','NVD23', 'FEMALE', 'LYTICS', 'AGELE70', 'AGEGT70',
              'BMILE30', 'NEWDIAB2', 'CKD1', 'CKD2', 'CKD4', 'SHOCKPCIS2', 'SHOCKPCIS3','SHOCKPCIS4', 'SHOCKPCIS5', 'NYHA123', 'NYHA4',
              'LESSCAI23', 'LESSCAI4', 'NEWSEQ2', 'NEWSEQ3','PRETIMINO', 'PREHGBLE13', 'PREHGBGT13', 'SHOCKPRIORANDPCI')#'SHOCKPCIS1' #'HDEF', 'NEWDIAB2'

rao.plus.vars <- c('Age','AGEGT70', 'AGELE70','BMI', 'BMILE30', 'ChronicLungDisease', 'CKD',
'CKD1', 'CKD2','CKD3', 'CKD4', 'FEMALE', 'GFR', 'LesonComplexty','StenosisPriorTreat', 'LESSCAI', 'LESSCAI23',
'LESSCAI4', 'LYTICS', 'NEWDIAB', 'NEWDIAB1', 'NEWDIAB2', 'NEWSEQ', 'NEWSEQ2', 'NEWSEQ3', 'NVD', 'NVD23',
'NYHA', 'NYHA123', 'NYHA4', 'PREHGBGT13', 'PREHGBLE13', 'PreProcHgb', 'PreProcTIMI', 'PRETIMINO',
'PriorCVD','PriorCardiacArrest','PriorPAD','PriorPCI','SHOCKPCIS','SHOCKPCIS1','SHOCKPCIS2','SHOCKPCIS3',
'SHOCKPCIS4','SHOCKPCIS5','SHOCKPCIS6','STEMI','CADPresentation','PrePCILVEF','PCIStatus','PriorCardioShock','PCICardioShock', 'CARSHOCK', 'DCARSHOCK',
'THROM','Diabetes', 'PreProcCreat', 'CurrentDialysis', 'SHOCKPRIORANDPCI')

rao.plus.vars <- c('Age','BMI','FEMALE', 'GFR', 'PreProcHgb','PriorCardiacArrest','CADPresentation',
                   'SHOCKPCIS', 'CARSHOCK', 'NYHA')


train1.data <- train1.data[, which(colnames(train1.data) %in% rao.plus.vars)]
train2.data <- train2.data[, which(colnames(train2.data) %in% rao.plus.vars)]
train3.data <- train3.data[, which(colnames(train3.data) %in% rao.plus.vars)]
train4.data <- train4.data[, which(colnames(train4.data) %in% rao.plus.vars)]
train5.data <- train5.data[, which(colnames(train5.data) %in% rao.plus.vars)]

test1.data <- test1.data[, which(colnames(test1.data) %in% rao.plus.vars)]
test2.data <- test2.data[, which(colnames(test2.data) %in% rao.plus.vars)]
test3.data <- test3.data[, which(colnames(test3.data) %in% rao.plus.vars)]
test4.data <- test4.data[, which(colnames(test4.data) %in% rao.plus.vars)]
test5.data <- test5.data[, which(colnames(test5.data) %in% rao.plus.vars)]

rm(ncdr.data.full)
rm(ncdr.rao.var)
rm(ncdr.rem.var)
rm(cv.fold1.idx.neg)
rm(cv.fold1.idx.pos)
rm(cv.fold2.idx.neg)
rm(cv.fold2.idx.pos)
rm(cv.fold3.idx.neg)
rm(cv.fold3.idx.pos)
rm(cv.fold4.idx.neg)
rm(cv.fold4.idx.pos)
gc()

#GLM on Rao+People
cat(paste('making clusters\n'))
c1 <- makePSOCKcluster(8)
registerDoParallel(c1)
cat(paste('running loop\n'))
list.crossval.results <- foreach(f=1:5) %dopar% {
  library(glmnet)
  library(pROC)
  library(xgboost)
  library(foreach)
  library(doParallel)
#list.crossval.results <- list()  
#for(f in c(4,5)) {
#  cat(paste('fold: ', f, '\n'))
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
  
#  train.rao.data <- train.data[,which(colnames(train.data) %in% rao.vars)]
#  test.rao.data <- test.data[,which(colnames(test.data) %in% rao.vars)]
  
  train.rao.plus.data <- train.data[,which(colnames(train.data) %in% rao.plus.vars)]
  test.rao.plus.data <- test.data[,which(colnames(test.data) %in% rao.plus.vars)]
  
  #cat(paste('f: ', f,' model building\n'))
 # train.data <- train.rao.data
#  train.data$labels <- train.labels
  #model.bleed <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), nrounds=100, verbose=0)#cv.glmnet(x=as.matrix(train.data), y=as.factor(train.labels), family='binomial')
  #model.bleed <- cv.glmnet(as.matrix(train.data), as.factor(train.labels), family='binomial', type.measure='auc', parallel=TRUE)
 # model.rao.bleed <- glm(labels~., data=train.data, family=binomial)
#  model.rao.xgb.bleed <- xgboost(data=as.matrix(train.rao.data), label=as.numeric(train.labels), verbose=0,
#                                 nrounds=1000, eta=0.1, max.depth=6, objective='binary:logistic', nthread=32)
#  model.rao.glmnet.bleed <- cv.glmnet(as.matrix(train.rao.data), as.factor(train.labels), family='binomial', type.measure='auc', parallel=TRUE)
  
  model.rao.plus.xgb.bleed <- xgboost(data=as.matrix(train.rao.plus.data), label=as.numeric(train.labels), verbose=0,
                                 nrounds=1000, eta=0.1, max.depth=6, objective='binary:logistic', nthread=32)
#  model.rao.plus.glmnet.bleed <- cv.glmnet(as.matrix(train.rao.plus.data), as.factor(train.labels), family='binomial', type.measure='auc', parallel=TRUE)
  
  
  #cat(paste('f: ', f,' predict\n'))
 # predict.rao.bleed <- predict(model.rao.bleed, newdata=test.rao.data, type='response')
#  predict.rao.glm.bleed <- predict(model.rao.glmnet.bleed, as.matrix(test.rao.data), type='response')
#  predict.rao.xgb.bleed <- predict(model.rao.xgb.bleed, as.matrix(test.rao.data))
#  predict.rao.plus.glm.bleed <- predict(model.rao.plus.glmnet.bleed, as.matrix(test.rao.plus.data), type='response')
  predict.rao.plus.xgb.bleed <- predict(model.rao.plus.xgb.bleed, as.matrix(test.rao.plus.data))
  
  #predict.bleed <- predict(model.bleed, newdata=test.data, type='response')
  #cat(paste('f: ', f,' roc building\n'))
 # roc.bleed <- roc(as.numeric(test.labels),  as.numeric(predict.rao.bleed))
#  roc.rao.glm.bleed <- roc(as.numeric(test.labels),  as.numeric(predict.rao.glm.bleed))
#  roc.rao.xgb.bleed <- roc(as.numeric(test.labels),  as.numeric(predict.rao.xgb.bleed))
#  roc.rao.plus.glm.bleed <- roc(as.numeric(test.labels),  as.numeric(predict.rao.plus.glm.bleed))
  roc.rao.plus.xgb.bleed <- roc(as.numeric(test.labels),  as.numeric(predict.rao.plus.xgb.bleed))
  
  #cat(paste('f: ', f,' auc: ', roc.bleed$auc,'\n'))
  #  cat(paste('f: ', f,' f building\n'))
 # f.bleed <- allROC_par(predict.rao.bleed, test.labels)
#  f.rao.glm.bleed <- allROC_par(predict.rao.glm.bleed, test.labels)
#  f.rao.xgb.bleed <- allROC_par(predict.rao.xgb.bleed, test.labels)
#  f.rao.plus.glm.bleed <- allROC_par(predict.rao.plus.glm.bleed, test.labels)
  f.rao.plus.xgb.bleed <- allROC_par(predict.rao.plus.xgb.bleed, test.labels)
  
#  list.fold[[1]] <- model.rao.bleed
#  list.fold[[2]] <- model.rao.glmnet.bleed
#  list.fold[[3]] <- model.rao.xgb.bleed
#  list.fold[[4]] <- model.rao.plus.glmnet.bleed
  list.fold[[5]] <- model.rao.plus.xgb.bleed
 # list.fold[[6]] <- predict.rao.bleed
#  list.fold[[7]] <- predict.rao.glm.bleed
#  list.fold[[8]] <- predict.rao.xgb.bleed
#  list.fold[[9]] <- predict.rao.plus.glm.bleed
  list.fold[[10]] <- predict.rao.plus.xgb.bleed
#  list.fold[[11]] <- roc.bleed
##  list.fold[[12]] <- roc.rao.glm.bleed
#  list.fold[[13]] <- roc.rao.xgb.bleed
#  list.fold[[14]] <- roc.rao.plus.glm.bleed
  list.fold[[15]] <- roc.rao.plus.xgb.bleed
#  list.fold[[16]] <- f.bleed
#  list.fold[[17]] <- f.rao.glm.bleed
#  list.fold[[18]] <- f.rao.xgb.bleed
#  list.fold[[19]] <- f.rao.plus.glm.bleed
  list.fold[[20]] <- f.rao.plus.xgb.bleed
  
  
   #list.crossval.results[[f]] <- list.fold
  list.res <- list()
  list.res <- list.fold
}

#save('list.crossval.results', file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/rao_plus_five_fold_data_V8_7JUN2016_folds1to4_14NOV2016.RData')
save('list.crossval.results', file='~/NCDR-ACC/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/BLENDED_results_five_fold_data_V8_7JUN2016_28JUN2016.RData')
cat(paste('Finished 5-fold cross-validation\n'))
aucs.bleed <- vector()
aucs.glm.bleed <- vector()
aucs.xgb.bleed <- vector()
aucs.plus.glm.bleed <- vector()
aucs.plus.xgb.bleed <- vector()

tp.bleed <- vector()
tp.glm.bleed <- vector()
tp.xgb.bleed <- vector()
tp.plus.glm.bleed <- vector()
tp.plus.xgb.bleed <- vector()

fp.bleed <- vector()
fp.glm.bleed <- vector()
fp.xgb.bleed <- vector()
fp.plus.glm.bleed <- vector()
fp.plus.xgb.bleed <- vector()

tn.bleed <- vector()
tn.glm.bleed <- vector()
tn.xgb.bleed <- vector()
tn.plus.glm.bleed <- vector()
tn.plus.xgb.bleed <- vector()

fn.bleed <- vector()
fn.glm.bleed <- vector()
fn.xgb.bleed <- vector()
fn.plus.glm.bleed <- vector()
fn.plus.xgb.bleed <- vector()

v.bleed <- vector()
v.glm.bleed <- vector()
v.xgb.bleed <- vector()
v.plus.glm.bleed <- vector()
v.plus.xgb.bleed <- vector()

f.bleed <- vector()
f.glm.bleed <- vector()
f.xgb.bleed <- vector()
f.plus.glm.bleed <- vector()
f.plus.xgb.bleed <- vector()

for(f in 1:5) {
  aucs.bleed[f] <- list.crossval.results[[f]][[11]]$auc
  aucs.glm.bleed[f] <- list.crossval.results[[f]][[12]]$auc
  aucs.xgb.bleed[f] <- list.crossval.results[[f]][[13]]$auc
  aucs.plus.glm.bleed[f] <- list.crossval.results[[f]][[14]]$auc
  aucs.plus.xgb.bleed[f] <- list.crossval.results[[f]][[15]]$auc
  
  f.bleed[f] <- list.crossval.results[[f]][[16]][[1]]$f.score[1]
  f.glm.bleed[f] <- list.crossval.results[[f]][[17]][[1]]$f.score[1]
  f.xgb.bleed[f] <- list.crossval.results[[f]][[18]][[1]]$f.score[1]
  f.plus.glm.bleed[f] <- list.crossval.results[[f]][[19]][[1]]$f.score[1]
  f.plus.xgb.bleed[f] <- list.crossval.results[[f]][[20]][[1]]$f.score[1]
  
  v.bleed[f] <- list.crossval.results[[f]][[16]][[1]]$v[1]
  v.glm.bleed[f] <- list.crossval.results[[f]][[17]][[1]]$v[1]
  v.xgb.bleed[f] <- list.crossval.results[[f]][[18]][[1]]$v[1]
  v.plus.glm.bleed[f] <- list.crossval.results[[f]][[19]][[1]]$v[1]
  v.plus.xgb.bleed[f] <- list.crossval.results[[f]][[20]][[1]]$v[1]
  
  tp.bleed[f] <- list.crossval.results[[f]][[16]][[1]]$tp[1]
  tp.glm.bleed[f] <- list.crossval.results[[f]][[17]][[1]]$tp[1]
  tp.xgb.bleed[f] <- list.crossval.results[[f]][[18]][[1]]$tp[1]
  tp.plus.glm.bleed[f] <- list.crossval.results[[f]][[19]][[1]]$tp[1]
  tp.plus.xgb.bleed[f] <- list.crossval.results[[f]][[20]][[1]]$tp[1]
  
  tn.bleed[f] <- list.crossval.results[[f]][[16]][[1]]$tn[1]
  tn.glm.bleed[f] <- list.crossval.results[[f]][[17]][[1]]$tn[1]
  tn.xgb.bleed[f] <- list.crossval.results[[f]][[18]][[1]]$tn[1]
  tn.plus.glm.bleed[f] <- list.crossval.results[[f]][[19]][[1]]$tn[1]
  tn.plus.xgb.bleed[f] <- list.crossval.results[[f]][[20]][[1]]$tn[1]
  
  fp.bleed[f] <- list.crossval.results[[f]][[16]][[1]]$fp[1]
  fp.glm.bleed[f] <- list.crossval.results[[f]][[17]][[1]]$fp[1]
  fp.xgb.bleed[f] <- list.crossval.results[[f]][[18]][[1]]$fp[1]
  fp.plus.glm.bleed[f] <- list.crossval.results[[f]][[19]][[1]]$fp[1]
  fp.plus.xgb.bleed[f] <- list.crossval.results[[f]][[20]][[1]]$fp[1]
  
  fn.bleed[f] <- list.crossval.results[[f]][[16]][[1]]$fn[1]
  fn.glm.bleed[f] <- list.crossval.results[[f]][[17]][[1]]$fn[1]
  fn.xgb.bleed[f] <- list.crossval.results[[f]][[18]][[1]]$fn[1]
  fn.plus.glm.bleed[f] <- list.crossval.results[[f]][[19]][[1]]$fn[1]
  fn.plus.xgb.bleed[f] <- list.crossval.results[[f]][[20]][[1]]$fn[1]
}


df <- data.frame(c(t.test(aucs.bleed)$estimate,t.test(aucs.glm.bleed)$estimate,t.test(aucs.xgb.bleed)$estimate,t.test(aucs.plus.glm.bleed)$estimate,t.test(aucs.plus.xgb.bleed)$estimate),
                 c(t.test(aucs.bleed)$conf.int[1],t.test(aucs.glm.bleed)$conf.int[1],t.test(aucs.xgb.bleed)$conf.int[1],t.test(aucs.plus.glm.bleed)$conf.int[1],t.test(aucs.plus.xgb.bleed)$conf.int[1]),
                 c(t.test(aucs.bleed)$conf.int[2],t.test(aucs.glm.bleed)$conf.int[2],t.test(aucs.xgb.bleed)$conf.int[2],t.test(aucs.plus.glm.bleed)$conf.int[2],t.test(aucs.plus.xgb.bleed)$conf.int[2]),
                 
                 c(t.test(f.bleed)$estimate,t.test(f.glm.bleed)$estimate,t.test(f.xgb.bleed)$estimate,t.test(f.plus.glm.bleed)$estimate,t.test(f.plus.xgb.bleed)$estimate),
                 c(t.test(f.bleed)$conf.int[1],t.test(f.glm.bleed)$conf.int[1],t.test(f.xgb.bleed)$conf.int[1],t.test(f.plus.glm.bleed)$conf.int[1],t.test(f.plus.xgb.bleed)$conf.int[1]),
                 c(t.test(f.bleed)$conf.int[2],t.test(f.glm.bleed)$conf.int[2],t.test(f.xgb.bleed)$conf.int[2],t.test(f.plus.glm.bleed)$conf.int[2],t.test(f.plus.xgb.bleed)$conf.int[2]),
                 
                 c(mean(v.bleed),mean(v.glm.bleed),mean(v.xgb.bleed),mean(v.plus.glm.bleed),mean(v.plus.xgb.bleed)),
                 c(sd(v.bleed),sd(v.glm.bleed),sd(v.xgb.bleed),sd(v.plus.glm.bleed),sd(v.plus.xgb.bleed)),
                 
                 c(sum(tp.bleed),sum(tp.glm.bleed),sum(tp.xgb.bleed),sum(tp.plus.glm.bleed),sum(tp.plus.xgb.bleed)),
                 c(sum(tn.bleed),sum(tn.glm.bleed),sum(tn.xgb.bleed),sum(tn.plus.glm.bleed),sum(tn.plus.xgb.bleed)),
                 c(sum(fp.bleed),sum(fp.glm.bleed),sum(fp.xgb.bleed),sum(fp.plus.glm.bleed),sum(fp.plus.xgb.bleed)),
                 c(sum(fn.bleed),sum(fn.glm.bleed),sum(fn.xgb.bleed),sum(fn.plus.glm.bleed),sum(fn.plus.xgb.bleed))
                 )

colnames(df) <- c('Mean AUC', 'Lower 95%' ,'Upper 95%', 'Mean F', 'Lower 95%' ,'Upper 95%', 'Mean Thresh', 'SD Thresh',
                  'Sum TP', 'Sum TN', 'Sum FP', 'Sum FN') 
rownames(df) <- c('rao', 'glm', 'xgb', 'blended glm', 'blended xgb')


for(f in 1:5) {
  aucs.plus.xgb.bleed[f] <- list.crossval.results[[f]][[15]]$auc
  
  f.plus.xgb.bleed[f] <- list.crossval.results[[f]][[20]][[1]]$f.score[1]
  
  v.plus.xgb.bleed[f] <- list.crossval.results[[f]][[20]][[1]]$v[1]
  
  tp.plus.xgb.bleed[f] <- list.crossval.results[[f]][[20]][[1]]$tp[1]
  
  tn.plus.xgb.bleed[f] <- list.crossval.results[[f]][[20]][[1]]$tn[1]
  
  fp.plus.xgb.bleed[f] <- list.crossval.results[[f]][[20]][[1]]$fp[1]
  
  fn.plus.xgb.bleed[f] <- list.crossval.results[[f]][[20]][[1]]$fn[1]
}


rao.preds <- list()
rao.preds[[1]] <- list.crossval.results[[1]][[10]]
rao.preds[[2]] <- list.crossval.results[[2]][[10]]
rao.preds[[3]] <- list.crossval.results[[3]][[10]]
rao.preds[[4]] <- list.crossval.results[[4]][[10]]
rao.preds[[5]] <- list.crossval.results[[5]][[10]]

rao.thresholds <- c(list.crossval.results[[1]][[20]][[1]]$v,list.crossval.results[[2]][[20]][[1]]$v,
                    list.crossval.results[[3]][[20]][[1]]$v,list.crossval.results[[4]][[20]][[1]]$v,
                    list.crossval.results[[5]][[20]][[1]]$v)

labels.all <- c(test1.labels, test2.labels, test3.labels, test4.labels, test5.labels)
rao.probs <- c(rao.preds[[1]],rao.preds[[2]],
               rao.preds[[3]],rao.preds[[4]],
               rao.preds[[5]])
# calib.results.clinical <- calcDeciles(full.clinical.probs, labels.all)
# calib.results.all <- calcDeciles(full.all.probs, labels.all)
dec.vals.clinical <- decPoints(rao.preds, 5)
#dec.vals.all <- decPoints(full.all.preds, 5)

mean.deciles.clinical <- vector()

for(idx in 1:length(dec.vals.clinical)) {
  mean.deciles.clinical[idx] <- mean(dec.vals.clinical[[idx]])
}

calib.results.clinical <- sortDeciles(rao.probs, labels.all, mean.deciles.clinical)
#calib.results.all <- sortDeciles(full.all.probs, labels.all, mean.deciles.all)

# list.res[[1]] <- calibration
# list.res[[2]] <- probs.sorted
# list.res[[3]] <- labels.sorted

idx.highest <- which(calib.results.clinical[[2]] >= mean.deciles.clinical[10])
labels.highest <- calib.results.clinical[[3]][idx.highest]



save.image(file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/BLENDED_10varmodel_Features_05JUL2017.RData')


####
#FEATURE RANKING

list.model.importance <- list()
for(f in c(1,2,3,4,5)) {
  list.model.importance[[f]] <- xgb.importance(feature_names = colnames(train1.data), model = list.crossval.results[[f]][[5]])
}

full.data <- rbind(train1.data, test1.data)
full.labels <- c(train1.labels, test1.labels)

full.model <- xgboost(data=as.matrix(full.data), label=as.numeric(full.labels), verbose=0,
                                    nrounds=1000, eta=0.1, max.depth=6, objective='binary:logistic')

full.model.importance <- xgb.importance(feature_names = colnames(full.data), model = full.model)

not.selected <- colnames(full.data)[which(!(colnames(full.data) %in% full.model.importance$Feature))]

rm(full.data)
rm(full.labels)
gc()

#save.image( file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/BLENDED_tempsave_21JUN2017.RData')


#load( file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/BLENDED_tempsave_21JUN2017.RData')


cat(paste('Forward Selection Feature Ranking\n'))
folds.feat <- foreach(feat.idx=1:length(full.model.importance$Feature)) %dopar% {
  library(pROC)
  library(xgboost)
  library(foreach)
  source('./fMeasure.R')
  #Existing Model in CV
#for(feat.idx in 13:length(full.model.importance$Feature)) {
  #cat(paste('Forward Selcetion, Num Features:', feat.idx,'\n'))
  # folds.tp <- vector()
  # folds.tn <- vector()
  # folds.fp <- vector()
  # folds.fn <- vector()
  # folds.f <- vector()
  folds.auc <- vector()
  #lists.f <- list()
  for(f in folds) {
  #folds.auc <- foreach(f=1:5, .combine=c) %dopar% {
    #library(pROC)
    #library(xgboost)
    #source('./fMeasure.R')
    #for(f in folds) {
    #cat(paste('fold: ', f, '\n'))
    if(f == 1) {
      train.data <- train1.data[,full.model.importance$Feature[1:feat.idx]]
      train.labels <- train1.labels
      test.data <- test1.data[,full.model.importance$Feature[1:feat.idx]]
      test.labels <- test1.labels
    } else if (f == 2) {
      train.data <- train2.data[,full.model.importance$Feature[1:feat.idx]]
      train.labels <- train2.labels
      test.data <- test2.data[,full.model.importance$Feature[1:feat.idx]]
      test.labels <- test2.labels
    } else if (f == 3) {
      train.data <- train3.data[,full.model.importance$Feature[1:feat.idx]]
      train.labels <- train3.labels
      test.data <- test3.data[,full.model.importance$Feature[1:feat.idx]]
      test.labels <- test3.labels
    } else if (f == 4) {
      train.data <- train4.data[,full.model.importance$Feature[1:feat.idx]]
      train.labels <- train4.labels
      test.data <- test4.data[,full.model.importance$Feature[1:feat.idx]]
      test.labels <- test4.labels
    } else {
      train.data <- train5.data[,full.model.importance$Feature[1:feat.idx]]
      train.labels <- train5.labels
      test.data <- test5.data[,full.model.importance$Feature[1:feat.idx]]
      test.labels <- test5.labels
    }
    
    #cat(paste('feat.idx', feat.idx, 'f: ', f,' model building\n'))
    #model.bleed <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), nrounds=100, verbose=0)#cv.glmnet(x=as.matrix(train.data), y=as.factor(train.labels), family='binomial')
    model.bleed <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), verbose=0,
                           nrounds=1000, eta=0.1, max.depth=6, objective='binary:logistic', nthread=32) 
    #cat(paste('feat.idx', feat.idx, 'f: ', f,' predict\n'))
    predict.bleed <- predict(model.bleed, as.matrix(test.data))
    #cat(paste('feat.idx', feat.idx, 'f: ', f,' roc building\n'))
    roc.bleed <- roc(as.numeric(test.labels),  as.numeric(predict.bleed))
    #cat(paste('feat.idx', feat.idx, 'f: ', f,' auc: ', roc.bleed$auc,'\n'))
    #  cat(paste('f: ', f,' f building\n'))
    #f.res <- optimalROC(predict.bleed, test.labels)
    
    #val <- roc.bleed$auc
    
    folds.auc[f] <- roc.bleed$auc
  }
  #cat(paste('feat.idx', feat.idx, 'mean auc:', mean(folds.auc), '\n'))
  folds.vec <- vector()
  folds.vec <- folds.auc
 
  #folds.feat.auc[[feat.idx]] <- folds.auc
  #gc()
}

save('list.model.importance', 'full.model.importance','not.selected', 'folds.feat', file='~/NCDR-ACC/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/BLENDED_Features_28JUN2017.RData')

