#NCDR Final Comparisons
#5-fold cross-validation
library(glmnet)
library(pROC)
library(xgboost)
library(foreach)
library(doParallel)
registerDoParallel()
#MODE <- 'all'#'site'#'clinical'# 'all'
load(file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/five_fold_data_V8_7JUN2016.RData')
#load(file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/var_imp_',MODE,'_five_fold_data_V7_11MAY2016.RData', sep=''))
source('./fMeasure.R')
source('./setup_crossval.R')
#Existing Model in CV
folds <- c(1,2,3,4,5)
#GLM on Rao+People
folds.auc <- vector()
list.crossval.results <- list()
# 
# rao.vars <- c('PriorPCI', 'PriorCVD', 'PriorPAD', 'ChronicLungDisease', 'PriorCardiacArrest','THROM', 'STEMI','NVD23', 'FEMALE', 'LYTICS', 'AGELE70', 'AGEGT70',
#               'BMILE30', 'NEWDIAB2', 'CKD1', 'CKD2', 'CKD4', 'SHOCKPCIS2', 'SHOCKPCIS3','SHOCKPCIS4', 'SHOCKPCIS5', 'NYHA123', 'NYHA4',
#               'LESSCAI23', 'LESSCAI4', 'NEWSEQ2', 'NEWSEQ3','PRETIMINO', 'PREHGBLE13', 'PREHGBGT13', 'SHOCKPRIORANDPCI')#'SHOCKPCIS1' #'HDEF', 'NEWDIAB2'
rao.vars.score <- c('STEMI', 'Age', 'BMI', 'PriorPCI', 'CKD', 'DCARSHOCK', 'PriorCardiacArrest', 'FEMALE', 'PreProcHgb', 'PCIStatus')


#rem.crossval <- crossval.list(ncdr.rem.var, ncdr.rem.var$Bleed, 5)
#rao.vars <- c('PriorPCI', 'CVD', 'PAD', 'ChronicLungDisease', 'PriorMI', 'HDEF','THROM', 'STEMI','NVD23', 'FEMALE', 'LYTICS', 'AGELE70', 'AGEGT70',
#              'BMILE30', 'NEWDIAB1', 'NEWDIAB2', 'CKD1', 'CKD2', 'CKD4', 'SHOCKPCIS1', 'SHOCKPCIS2', 'SHOCKPCIS3','SHOCKPCIS4', 'SHOCKPCIS5', 'NYHA123', 'NYHA4',
#              'LESSCAI23', 'LESSCAI4', 'NEWSEQ2', 'NEWSEQ3','PRETIMINO', 'PREHGBLE13', 'PREHGBGT13')
# rao.vars <- c('STEMI', 'Age', 'BMI', 'PriorPCI', 'CKD', 'DCARSHOCK', 'PriorCardiacArrest', 'FEMALE', 'PreProcHgb', 'PCIStatus')
rao.scores <- function(rao.row) {
  score <- 0
  if(rao.row[1] == 1) {
    score <- score + 15
  }
  
  if(rao.row[2] < 60) {
    score <- score + 0
  } else if(rao.row[2] <= 70) {
    score <- score + 10
  } else if(rao.row[2] <= 79) {
    score <- score + 15
  } else {
    score <- score + 20
  }
  
  if(rao.row[3] < 20) {
    score <- score + 15
  } else if(rao.row[3] <= 30) {
    score <- score + 5
  } else if(rao.row[3] <= 39) {
    score <- score + 0
  } else {
    score <- score + 5
  }
  
  if(rao.row[4] == 0) {
    score <- score + 10
  }
  
  if(rao.row[5] == 4) {
    score <- score + 30
  } else if(rao.row[5] == 2) {
    score <- score + 25
  } else if(rao.row[5] == 1) {
    score <- score + 10
  }
  
  if(rao.row[6] < 4) {
    score <- score + 35
  }
  
  if(rao.row[7] == 1) {
    score <- score + 15
  }
  
  if(rao.row[8] == 1) {
    score <- score + 20
  }
  
  if(rao.row[9] < 13) {
    score <- score + 5
  } else if(rao.row[9] < 15) {
    score <- score + 0
  } else {
    score <- score + 10
  }
  
  if(rao.row[10] == 2) {
    score <- score + 20
  } else if(rao.row[10] > 2) {
    score <- score + 40
  }
  
  score
}

# rao.vars <- c("Smoker" , "Hypertension", "Dyslipidemia"  , "FamilyHxCAD",                
#                        "PriorMI","PriorHF",   "ValveSurgery" ,"PriorPCI","PriorCABG",    
#                        "Height","Weight","CurrentDialysis","PriorCVD","PriorPAD",                   
#                        "ChronicLungDisease","Diabetes" ,                     
#                        "InsPrivate","InsMedicare","InsMedicaid","InsMilitary",                  
#                        "InsState","InsIHS","InsNonUS","InsNone","Age",                  
#                        "Sex","RaceWhite","RaceBlack","RaceAsian","RaceAmIndian",                
#                        "RaceNatHaw","HispOrig","ThromTherapy",                 
#                        "AntiAnginalMed","AA_BetaBlockers","AA_CaChannel","AA_LongActingNitrates","AA_Ranolazine",                
#                        "AA_OtherAgent","Prior2weeksHF","CardioLVSD",  "PeriopEval","PriorCardioShock",             
#                        "PriorCardiacArrest",                  
#                        "CADPresentation","AnginalClass","NYHA",           
#                        "PrePCILVEF",                
#                        "STEMIFirstNoted","PreProcCKMB",               
#                        "PreProcTnl","PreProcTnT","PreProcCreat",                 
#                        "PreProcHgb",               
#                        "STEMI" ,  'AdmtSource', 'DiabetesControl',                    
#                         "AGELE70"   ,                   
#                        "AGEGT70","BMI", "BMILE30","NEWDIAB","NEWDIAB1"  ,                   
#                        "NEWDIAB2","HDEF","FEMALE",                  
#                        "RENFAIL","GFR","CKD","CKD1" ,"CKD2",                         
#                        "CKD3","CKD4" , "LYTICS",                    
#                        "PREHGBLE13" , "PREHGBGT13", 'PCIStatus', 'SHOCKPCIS',
#               'SHOCKPCIS1', 'SHOCKPCIS2', 'SHOCKPCIS3', 'SHOCKPCIS4', 'SHOCKPCIS5', 'SHOCKPCIS6')
train1.data <- train1.data[, rao.vars.score]
train2.data <- train2.data[, rao.vars.score]
train3.data <- train3.data[, rao.vars.score]
train4.data <- train4.data[, rao.vars.score]
train5.data <- train5.data[, rao.vars.score]

test1.data <- test1.data[, rao.vars.score]
test2.data <- test2.data[, rao.vars.score]
test3.data <- test3.data[, rao.vars.score]
test4.data <- test4.data[, rao.vars.score]
test5.data <- test5.data[, rao.vars.score]

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



for (f in c(1,2,3,4,5)) {
  cat(paste('fold: ', f, '\n'))
  list.fold <- list()
  if(f == 1) {
    train.data <- train1.data
    train.labels <- train1.labels
    test.data <- test1.data
    test.labels <- test1.labels
  } else if(f == 2) {
    train.data <- train2.data
    train.labels <- train2.labels
    test.data <- test2.data
    test.labels <- test2.labels
  } else if(f == 3) {
    train.data <- train3.data
    train.labels <- train3.labels
    test.data <- test3.data
    test.labels <- test3.labels
  } else if(f == 4) {
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

    
#  train.data <- train.data[,rao.vars]
#  test.data <- test.data[,rao.vars]
  test.scores <- apply(test.data, 1, rao.scores)
 
  cat(paste('f: ', f,' model building\n'))
  model.xgb <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), nrounds=100, objective='binary:logistic',
                         eta = 0.1, max.depth = 6, verbose=0)#cv.glmnet(x=as.matrix(train.data), y=as.factor(train.labels), family='binomial')
  model.bleed <- cv.glmnet(as.matrix(train.data), as.factor(train.labels), family='binomial', type.measure='auc', parallel=TRUE)
  #model.bleed <- glm(labels~., data=train.data, family=binomial)
  cat(paste('f: ', f,' predict\n'))
  
  v <- 65
  pred <- rep(0, length(test.scores))
  pred[which(test.scores >= v)] <- 1
  #values <- ratesOfPredict(pred, gt)
  tp <- 0
  fp <- 0
  tn <- 0
  fn <- 0
  
  tp <- length(which((pred == test.labels) & (pred == 1)))
  tn <- length(which((pred == test.labels) & (pred == 0)))
  fp <- length(which((pred != test.labels) & (pred == 1)))
  fn <- length(which((pred != test.labels) & (pred == 0)))
  
  list.pred <- list()
  list.pred[[1]] <- tp
  list.pred[[2]] <- tn
  list.pred[[3]] <- fp
  list.pred[[4]] <- fn
  
  
  predict.bleed <- predict(model.bleed, as.matrix(test.data), type='response')
  predict.xgb <- predict(model.xgb, as.matrix(test.data))
  cat(paste('f: ', f,' roc building\n'))
  roc.bleed <- roc(as.numeric(test.labels),  as.numeric(predict.bleed), parallel=TRUE)
  roc.xgb <- roc(as.numeric(test.labels), as.numeric(predict.xgb), parallel=TRUE)
  roc.score <- roc(as.numeric(test.labels), as.numeric(test.scores))
  cat(paste('f: ', f,' auc: ', roc.bleed$auc,'\n'))
  #  cat(paste('f: ', f,' f building\n'))
  f.bleed <- allROC_par(as.numeric(predict.bleed), as.numeric(test.labels))
  f.xgb <- allROC_par(as.numeric(predict.xgb), as.numeric(test.labels))
  f.score <- allROC_par(as.numeric(test.scores), as.numeric(test.labels))
  f.thresh <- allROC_par(as.numeric(pred), as.numeric(test.labels))
  
  list.fold[[1]] <- model.bleed
  list.fold[[2]] <- model.xgb
  list.fold[[3]] <- xgb.importance(feature_names=colnames(train.data), model=model.xgb)
  list.fold[[4]] <- test.scores
  list.fold[[5]] <- predict.bleed
  list.fold[[6]] <- predict.xgb
  list.fold[[7]] <- roc.score
  list.fold[[8]] <- roc.bleed
  list.fold[[9]] <- roc.xgb
  list.fold[[10]] <- f.score
  list.fold[[11]] <- f.bleed
  list.fold[[12]] <- f.xgb
  list.fold[[13]] <- list.pred
  list.fold[[14]] <- pred
  list.fold[[15]] <- f.thresh

  list.crossval.results[[f]] <- list.fold
}

save('list.crossval.results', 'test1.labels', 'test2.labels', 'test3.labels', 'test4.labels', 'test5.labels',
     file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/BLENDED_riskscore_presentation_five_fold_data_V8_7JUN2016.RData')


aucs.score <- vector()
aucs.dyn.thresh.score <- vector()
aucs.glm.bleed <- vector()
aucs.xgb.bleed <- vector()


tp.score <- vector()
tp.dyn.thresh.score <- vector()
tp.glm.bleed <- vector()
tp.xgb.bleed <- vector()

tn.score <- vector()
tn.dyn.thresh.score <- vector()
tn.glm.bleed <- vector()
tn.xgb.bleed <- vector()

fp.score <- vector()
fp.dyn.thresh.score <- vector()
fp.glm.bleed <- vector()
fp.xgb.bleed <- vector()

fn.score <- vector()
fn.dyn.thresh.score <- vector()
fn.glm.bleed <- vector()
fn.xgb.bleed <- vector()

f.score <- vector()
f.dyn.thresh.score <- vector()
f.glm.bleed <- vector()
f.xgb.bleed <- vector()

v.score <- vector()
v.dyn.thresh.score <- vector()
v.glm.bleed <- vector()
v.xgb.bleed <- vector()

for(f in 1:5) {
  aucs.score[f] <- list.crossval.results[[f]][[7]]$auc
  aucs.dyn.thresh.score[f] <- list.crossval.results[[f]][[7]]$auc
  aucs.glm.bleed[f] <- list.crossval.results[[f]][[8]]$auc
  aucs.xgb.bleed[f] <- list.crossval.results[[f]][[9]]$auc
  
  tp.score[f] <- list.crossval.results[[f]][[13]][[1]]
  tp.dyn.thresh.score[f] <- list.crossval.results[[f]][[10]][[1]]$tp[1]
  tp.glm.bleed[f] <- list.crossval.results[[f]][[11]][[1]]$tp[1]
  tp.xgb.bleed[f] <- list.crossval.results[[f]][[12]][[1]]$tp[1]
  
  tn.score[f] <- list.crossval.results[[f]][[13]][[2]]
  tn.dyn.thresh.score[f] <- list.crossval.results[[f]][[10]][[1]]$tn[1]
  tn.glm.bleed[f] <- list.crossval.results[[f]][[11]][[1]]$tn[1]
  tn.xgb.bleed[f] <- list.crossval.results[[f]][[12]][[1]]$tn[1]
  
  fp.score[f] <- list.crossval.results[[f]][[13]][[3]]
  fp.dyn.thresh.score[f] <- list.crossval.results[[f]][[10]][[1]]$fp[1]
  fp.glm.bleed[f] <- list.crossval.results[[f]][[11]][[1]]$fp[1]
  fp.xgb.bleed[f] <- list.crossval.results[[f]][[12]][[1]]$fp[1]
  
  fn.score[f] <- list.crossval.results[[f]][[13]][[4]]
  fn.dyn.thresh.score[f] <- list.crossval.results[[f]][[10]][[1]]$fn[1]
  fn.glm.bleed[f] <- list.crossval.results[[f]][[11]][[1]]$fn[1]
  fn.xgb.bleed[f] <- list.crossval.results[[f]][[12]][[1]]$fn[1]
  
  f.score[f] <- list.crossval.results[[f]][[15]][[1]]$f.score[1]
  f.dyn.thresh.score[f] <- list.crossval.results[[f]][[10]][[1]]$f.score[1]
  f.glm.bleed[f] <- list.crossval.results[[f]][[11]][[1]]$f.score[1]
  f.xgb.bleed[f] <- list.crossval.results[[f]][[12]][[1]]$f.score[1]
  
  v.score[f] <- 65
  v.dyn.thresh.score[f] <- list.crossval.results[[f]][[10]][[1]]$v[1]
  v.glm.bleed[f] <- list.crossval.results[[f]][[11]][[1]]$v[1]
  v.xgb.bleed[f] <- list.crossval.results[[f]][[12]][[1]]$v[1]
  
}

df <- data.frame(c(t.test(aucs.score)$estimate,t.test(aucs.dyn.thresh.score)$estimate,t.test(aucs.glm.bleed)$estimate,t.test(aucs.xgb.bleed)$estimate),
                 c(t.test(aucs.score)$conf.int[1],t.test(aucs.dyn.thresh.score)$conf.int[1],t.test(aucs.glm.bleed)$conf.int[1],t.test(aucs.xgb.bleed)$conf.int[1]),
                 c(t.test(aucs.score)$conf.int[2],t.test(aucs.dyn.thresh.score)$conf.int[2],t.test(aucs.glm.bleed)$conf.int[2],t.test(aucs.xgb.bleed)$conf.int[2]),
                 
                 c(t.test(f.score)$estimate,t.test(f.dyn.thresh.score)$estimate,t.test(f.glm.bleed)$estimate,t.test(f.xgb.bleed)$estimate),
                 c(t.test(f.score)$conf.int[1],t.test(f.dyn.thresh.score)$conf.int[1],t.test(f.glm.bleed)$conf.int[1],t.test(f.xgb.bleed)$conf.int[1]),
                 c(t.test(f.score)$conf.int[2],t.test(f.dyn.thresh.score)$conf.int[2],t.test(f.glm.bleed)$conf.int[2],t.test(f.xgb.bleed)$conf.int[2]),
                 
                 c(mean(v.score),mean(v.dyn.thresh.score),mean(v.glm.bleed),mean(v.xgb.bleed)),
                 c(sd(v.score),sd(v.dyn.thresh.score),sd(v.glm.bleed),sd(v.xgb.bleed)),
                 
                 c(sum(tp.score),sum(tp.dyn.thresh.score),sum(tp.glm.bleed),sum(tp.xgb.bleed)),
                 c(sum(tn.score),sum(tn.dyn.thresh.score),sum(tn.glm.bleed),sum(tn.xgb.bleed)),
                 c(sum(fp.score),sum(fp.dyn.thresh.score),sum(fp.glm.bleed),sum(fp.xgb.bleed)),
                 c(sum(fn.score),sum(fn.dyn.thresh.score),sum(fn.glm.bleed),sum(fn.xgb.bleed))
)

colnames(df) <- c('Mean AUC', 'Lower 95%' ,'Upper 95%', 'Mean F', 'Lower 95%' ,'Upper 95%', 'Mean Thresh', 'SD Thresh',
                  'Sum TP', 'Sum TN', 'Sum FP', 'Sum FN') 
rownames(df) <- c('rao', 'rao dyn thresh', 'glmnet', 'xgb')





total.data <- rbind(train1.data, test1.data)
total.labels <- c(train1.labels, test1.labels)

total.data <- total.data[,rao.vars]

model.xgb <- xgboost(data=as.matrix(total.data), label=as.numeric(total.labels), nrounds=100, objective='binary:logistic',
                     eta = 0.1, max.depth = 6, verbose=0)#cv.glmnet(x=as.matrix(train.data), y=as.factor(train.labels), family='binomial')

model.importance <- xgb.importance(feature_names=colnames(total.data), model=model.xgb)

save('model.importance', 'rao.vars', file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/riskscore_var_importance_presentation_five_fold_data_V8_7JUN2016.RData' )

train1.data <- train1.data[,rao.vars]
train2.data <- train2.data[,rao.vars]
train3.data <- train3.data[,rao.vars]
train4.data <- train4.data[,rao.vars]
train5.data <- train5.data[,rao.vars]

test1.data <- test1.data[,rao.vars]
test2.data <- test2.data[,rao.vars]
test3.data <- test3.data[,rao.vars]
test4.data <- test4.data[,rao.vars]
test5.data <- test5.data[,rao.vars]
folds.feat.auc <- list()
for(feat.idx in 1:20){ #length(feat.names)) {
  cat(paste('Forward Selcetion, Num Features:', feat.idx,'\n'))
  folds.auc <- vector()
  for(f in c(1,2,3,4,5)) {
    cat(paste('fold: ', f, '\n'))
    if(f == 1) {
      train.data <- train1.data[,model.importance$Feature[1:feat.idx]]
      train.labels <- train1.labels
      test.data <- test1.data[,model.importance$Feature[1:feat.idx]]
      test.labels <- test1.labels
    } else if (f == 2) {
      train.data <- train2.data[,model.importance$Feature[1:feat.idx]]
      train.labels <- train2.labels
      test.data <- test2.data[,model.importance$Feature[1:feat.idx]]
      test.labels <- test2.labels
    } else if (f == 3) {
      train.data <- train3.data[,model.importance$Feature[1:feat.idx]]
      train.labels <- train3.labels
      test.data <- test3.data[,model.importance$Feature[1:feat.idx]]
      test.labels <- test3.labels
    } else if (f == 4) {
      train.data <- train4.data[,model.importance$Feature[1:feat.idx]]
      train.labels <- train4.labels
      test.data <- test4.data[,model.importance$Feature[1:feat.idx]]
      test.labels <- test4.labels
    } else {
      train.data <- train5.data[,model.importance$Feature[1:feat.idx]]
      train.labels <- train5.labels
      test.data <- test5.data[,model.importance$Feature[1:feat.idx]]
      test.labels <- test5.labels
    }
    
    cat(paste('f: ', f,' model building\n'))
    #model.bleed <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), nrounds=100, verbose=0)#cv.glmnet(x=as.matrix(train.data), y=as.factor(train.labels), family='binomial')
    model.bleed <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), verbose=0,
                           nrounds=100, eta=0.1, max.depth=6, objective='binary:logistic') 
    cat(paste('f: ', f,' predict\n'))
    predict.bleed <- predict(model.bleed, as.matrix(test.data))
    cat(paste('f: ', f,' roc building\n'))
    roc.bleed <- roc(as.numeric(test.labels),  as.numeric(predict.bleed), parallel=TRUE)
    cat(paste('f: ', f,' auc: ', roc.bleed$auc,'\n'))
    #  cat(paste('f: ', f,' f building\n'))
    #f.res <- optimalROC(predict.bleed, test.labels)
    
    folds.auc[f] <- roc.bleed$auc
  }
  cat(paste('mean auc:', mean(folds.auc), '\n'))
  folds.feat.auc[[feat.idx]] <- folds.auc
  gc()
}#End of Forward Selection Loop
save('folds.feat.auc', 'rao.vars', 'model.importance', file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/riskscore_top20_var_importance_presentation_five_fold_data_V8_7JUN2016.RData' )


#Compare Rao
#
# cols.presentation <- c("Smoker" , "Hypertension", "Dyslipidemia"  , "FamilyHxCAD",                
#                        "PriorMI","PriorHF",   "ValveSurgery" ,"PriorPCI","PriorCABG",    
#                        "Height","Weight","CurrentDialysis","PriorCVD","PriorPAD",                   
#                        "ChronicLungDisease","Diabetes" ,                     
#                        "InsPrivate","InsMedicare","InsMedicaid","InsMilitary",                  
#                        "InsState","InsIHS","InsNonUS","InsNone","Age",                  
#                        "Sex","RaceWhite","RaceBlack","RaceAsian","RaceAmIndian",                
#                        "RaceNatHaw","HispOrig","OnsetTimeEst","OnsetTimeNA","ThromTherapy",                 
#                        "AntiAnginalMed","AA_BetaBlockers","AA_CaChannel","AA_LongActingNitrates","AA_Ranolazine",                
#                        "AA_OtherAgent","Prior2weeksHF","CardioLVSD",  "PeriopEval","PriorCardioShock",             
#                        "PriorCardiacArrest",                  
#                        "CADPresentation","AnginalClass","Prior2weekNYHA",           
#                        "PrePCILVEF", "PrePCILVEFNA",                
#                        "STEMIFirstNoted","PreProcCKMB","PreProcCKND","PreProcCKMBNM",                
#                        "PreProcTnl","PreProcTnlND","PreProcTnT","PreProcTnTND","PreProcCreat",                 
#                        "PreProcCreatND","PreProcHgb","PreProcHgbND",                
#                        "STEMI" ,                      
#                        "AdmtSrc_1" ,"AdmtSrc_2","AdmtSrc_3","DiabetesControlNone" ,         
#                        "DiabetesControlDiet",           "DiabetesControlOral" ,          "DiabetesControlInsulin",        "DiabetesControlOtherOrMissing", "AGELE70"   ,                   
#                        "AGEGT70","BMI", "BMILE30","NEWDIAB","NEWDIAB1"  ,                   
#                        "NEWDIAB2","HDEF","FEMALE",                  
#                        "RENFAIL","GFR","CKD","CKD1" ,"CKD2",                         
#                        "CKD3","CKD4" , "LYTICS",                    
#                        "PREHGBLE13" , "PREHGBGT13")

#Full Data Model XGB Importance

#Top 20 predictors 