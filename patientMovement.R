#First load datasets of Labels
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/labels_v8_7JUN2016.RData")

#full.labels <- c(test1.labels, test2.labels, test3.labels, test4.labels, test5.labels)
full.labels <- list()
full.labels[[1]] <- test1.labels
full.labels[[2]] <- test2.labels
full.labels[[3]] <- test3.labels
full.labels[[4]] <- test4.labels
full.labels[[5]] <- test5.labels
#Then load predictions
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/fulldata_raopeople_glm_five_fold_data_V8_7JUN2016.RData") #Rao+People GLM

rao.people.preds.1 <- list.crossval.results[[1]][[2]]
rao.people.preds.2 <- list.crossval.results[[2]][[2]]
rao.people.preds.3 <- list.crossval.results[[3]][[2]]
rao.people.preds.4 <- list.crossval.results[[4]][[2]]
rao.people.preds.5 <- list.crossval.results[[5]][[2]]

rao.people.preds <- list()
rao.people.preds[[1]] <- rao.people.preds.1
rao.people.preds[[2]] <- rao.people.preds.2
rao.people.preds[[3]] <- rao.people.preds.3
rao.people.preds[[4]] <- rao.people.preds.4
rao.people.preds[[5]] <- rao.people.preds.5

rao.people.thresholds <- c(list.crossval.results[[1]][[4]][[1]]$v,list.crossval.results[[2]][[4]][[1]]$v,
                           list.crossval.results[[3]][[4]][[1]]$v,list.crossval.results[[4]][[4]][[1]]$v,
                           list.crossval.results[[5]][[4]][[1]]$v)

#FULL GLM CLINICAL
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/full_GLM_AUC_clinical_five_fold_data_V8_7JUN2016.RData") #full clinical GLM
full.glm.preds <- list()
full.glm.preds[[1]] <- list.responses[[1]]
full.glm.preds[[2]] <- list.responses[[2]]
full.glm.preds[[3]] <- list.responses[[3]]
full.glm.preds[[4]] <- list.responses[[4]]
full.glm.preds[[5]] <- list.responses[[5]]

full.glm.thresholds <- c(list.f[[1]][[1]]$v,list.f[[2]][[1]]$v,
                              list.f[[3]][[1]]$v,list.f[[4]][[1]]$v,
                              list.f[[5]][[1]]$v)

#FULL XGB CLINICAL
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/XGB_AUC_clinical_five_fold_data_V8_7JUN2016.RData")

full.clinical.preds <- list()
full.clinical.preds[[1]] <- xgboost.list.responses[[1]]
full.clinical.preds[[2]] <- xgboost.list.responses[[2]]
full.clinical.preds[[3]] <- xgboost.list.responses[[3]]
full.clinical.preds[[4]] <- xgboost.list.responses[[4]]
full.clinical.preds[[5]] <- xgboost.list.responses[[5]]

full.clinical.thresholds <- c(xgboost.list.f[[1]][[1]]$v,xgboost.list.f[[2]][[1]]$v,
                              xgboost.list.f[[3]][[1]]$v,xgboost.list.f[[4]][[1]]$v,
                              xgboost.list.f[[5]][[1]]$v)

#Full All
load("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/full_XGB_AUC_all_five_fold_data_V8_7JUN2016.RData")
full.all.preds <- list()
full.all.preds[[1]] <- xgboost.list.responses[[1]]
full.all.preds[[2]] <- xgboost.list.responses[[2]]
full.all.preds[[3]] <- xgboost.list.responses[[3]]
full.all.preds[[4]] <- xgboost.list.responses[[4]]
full.all.preds[[5]] <- xgboost.list.responses[[5]]

full.all.thresholds <- c(xgboost.list.f[[1]][[1]]$v,xgboost.list.f[[2]][[1]]$v,
                              xgboost.list.f[[3]][[1]]$v,xgboost.list.f[[4]][[1]]$v,
                              xgboost.list.f[[5]][[1]]$v)
#Generate the Per Patient Prediction based upon the best F-score
source('./fMeasure.R')
folds <- c(1,2,3,4,5)
#For each fold 
rao.people.results <- list()
full.glm.results <- list()
full.clinical.results <- list()
full.all.results <- list()

rao.people.results.tp <- vector()
full.glm.results.tp <- list()
full.clinical.results.tp <- vector()
full.all.results.tp <- vector()

rao.people.results.tn <- vector()
full.glm.results.tn <- list()
full.clinical.results.tn <- vector()
full.all.results.tn <- vector()

rao.people.results.fp <- vector()
full.glm.results.fp <- list()
full.clinical.results.fp <- vector()
full.all.results.fp <- vector()

rao.people.results.fn <- vector()
full.glm.results.fn <- list()
full.clinical.results.fn <- vector()
full.all.results.fn <- vector()

list.results <- list()

for(f in folds) {
  list.folds <- list()
  test.labels <- full.labels[[f]]
  #Rao+People
  pred.labels <- as.numeric(rao.people.preds[[f]] > rao.people.thresholds[f])
  #encode TP, TN, FP, FN as 0,1,2,3
  pred.results <- rep(NA, length(pred.labels))
  pred.results[which((pred.labels == 1) & (test.labels == 1))] <- 0
  pred.results[which((pred.labels == 0) & (test.labels == 0))] <- 1
  pred.results[which((pred.labels == 1) & (test.labels == 0))] <- 2
  pred.results[which((pred.labels == 0) & (test.labels == 1))] <- 3
  
  rao.people.results[[f]] <- pred.results
  #GLM Clinical
  pred.labels <- as.numeric(full.glm.preds[[f]] > full.glm.thresholds[f])
  #encode TP, TN, FP, FN as 0,1,2,3
  pred.results <- rep(NA, length(pred.labels))
  pred.results[which((pred.labels == 1) & (test.labels == 1))] <- 0
  pred.results[which((pred.labels == 0) & (test.labels == 0))] <- 1
  pred.results[which((pred.labels == 1) & (test.labels == 0))] <- 2
  pred.results[which((pred.labels == 0) & (test.labels == 1))] <- 3
  
  full.glm.results[[f]] <- pred.results
  
  #XGB Clinical
  pred.labels <- as.numeric(full.clinical.preds[[f]] > full.clinical.thresholds[f])
  #encode TP, TN, FP, FN as 0,1,2,3
  pred.results <- rep(NA, length(pred.labels))
  pred.results[which((pred.labels == 1) & (test.labels == 1))] <- 0
  pred.results[which((pred.labels == 0) & (test.labels == 0))] <- 1
  pred.results[which((pred.labels == 1) & (test.labels == 0))] <- 2
  pred.results[which((pred.labels == 0) & (test.labels == 1))] <- 3
  
  full.clinical.results[[f]] <- pred.results
  
  #XGB All
  pred.labels <- as.numeric(full.all.preds[[f]] > full.all.thresholds[f])
  #encode TP, TN, FP, FN as 0,1,2,3
  pred.results <- rep(NA, length(pred.labels))
  pred.results[which((pred.labels == 1) & (test.labels == 1))] <- 0
  pred.results[which((pred.labels == 0) & (test.labels == 0))] <- 1
  pred.results[which((pred.labels == 1) & (test.labels == 0))] <- 2
  pred.results[which((pred.labels == 0) & (test.labels == 1))] <- 3
  
  full.all.results[[f]] <- pred.results
  
  rao.people.tp <- length(which(rao.people.results[[f]] == 0))
  rao.people.tn <- length(which(rao.people.results[[f]] == 1))
  rao.people.fp <- length(which(rao.people.results[[f]] == 2))
  rao.people.fn <- length(which(rao.people.results[[f]] == 3))
  
  idx.rao.people.tp <- which(rao.people.results[[f]] == 0)
  idx.rao.people.tn <- which(rao.people.results[[f]] == 1)
  idx.rao.people.fp <- which(rao.people.results[[f]] == 2)
  idx.rao.people.fn <- which(rao.people.results[[f]] == 3)
  
  full.glm.tp <- length(which(full.glm.results[[f]] == 0))
  full.glm.tn <- length(which(full.glm.results[[f]] == 1))
  full.glm.fp <- length(which(full.glm.results[[f]] == 2))
  full.glm.fn <- length(which(full.glm.results[[f]] == 3))
  
  idx.full.glm.tp <- which(full.glm.results[[f]] == 0)
  idx.full.glm.tn <- which(full.glm.results[[f]] == 1)
  idx.full.glm.fp <- which(full.glm.results[[f]] == 2)
  idx.full.glm.fn <- which(full.glm.results[[f]] == 3)
  
  full.clinical.tp <- length(which(full.clinical.results[[f]] == 0))
  full.clinical.tn <- length(which(full.clinical.results[[f]] == 1))
  full.clinical.fp <- length(which(full.clinical.results[[f]] == 2))
  full.clinical.fn <- length(which(full.clinical.results[[f]] == 3))
  
  idx.full.clinical.tp <- which(full.clinical.results[[f]] == 0)
  idx.full.clinical.tn <- which(full.clinical.results[[f]] == 1)
  idx.full.clinical.fp <- which(full.clinical.results[[f]] == 2)
  idx.full.clinical.fn <- which(full.clinical.results[[f]] == 3)
  
  full.all.tp <- length(which(full.all.results[[f]] == 0))
  full.all.tn <- length(which(full.all.results[[f]] == 1))
  full.all.fp <- length(which(full.all.results[[f]] == 2))
  full.all.fn <- length(which(full.all.results[[f]] == 3))
  
  idx.full.all.tp <- which(full.all.results[[f]] == 0)
  idx.full.all.tn <- which(full.all.results[[f]] == 1)
  idx.full.all.fp <- which(full.all.results[[f]] == 2)
  idx.full.all.fn <- which(full.all.results[[f]] == 3)
  
  #Relative change
  
  relative.change.full.glm.tp.total <- full.glm.tp - rao.people.tp
  relative.change.full.glm.tp.increase <- length(which(rao.people.results[[f]][idx.full.glm.tp] != 0))
  relative.change.full.glm.tp.decrease <- length(which(full.glm.results[[f]][idx.rao.people.tp] != 0))
  
  relative.change.full.glm.tn.total <- full.glm.tn - rao.people.tn
  relative.change.full.glm.tn.increase <- length(which(rao.people.results[[f]][idx.full.glm.tn] != 1))
  relative.change.full.glm.tn.decrease <- length(which(full.glm.results[[f]][idx.rao.people.tn] != 1))
  
  relative.change.full.glm.fp.total <- full.glm.fp - rao.people.fp
  relative.change.full.glm.fp.increase <- length(which(rao.people.results[[f]][idx.full.glm.fp] != 2))
  relative.change.full.glm.fp.decrease <- length(which(full.glm.results[[f]][idx.rao.people.fp] != 2))
  
  relative.change.full.glm.fn.total <- full.glm.fn - rao.people.fn
  relative.change.full.glm.fn.increase <- length(which(rao.people.results[[f]][idx.full.glm.fn] != 3))
  relative.change.full.glm.fn.decrease <- length(which(full.glm.results[[f]][idx.rao.people.fn] != 3))
  list.relative.full.glm <- vector()
  list.relative.full.glm[1] <- relative.change.full.glm.tp.total
  list.relative.full.glm[2] <- relative.change.full.glm.tp.increase
  list.relative.full.glm[3] <- relative.change.full.glm.tp.decrease
  list.relative.full.glm[4] <- relative.change.full.glm.tn.total
  list.relative.full.glm[5] <- relative.change.full.glm.tn.increase
  list.relative.full.glm[6] <- relative.change.full.glm.tn.decrease
  list.relative.full.glm[7] <- relative.change.full.glm.fp.total
  list.relative.full.glm[8] <- relative.change.full.glm.fp.increase
  list.relative.full.glm[9] <- relative.change.full.glm.fp.decrease
  list.relative.full.glm[10] <- relative.change.full.glm.fn.total
  list.relative.full.glm[11] <- relative.change.full.glm.fn.increase
  list.relative.full.glm[12] <- relative.change.full.glm.fn.decrease
  
  relative.change.full.clinical.tp.total <- full.clinical.tp - full.glm.tp
  relative.change.full.clinical.tp.increase <- length(which(full.glm.results[[f]][idx.full.clinical.tp] != 0))
  relative.change.full.clinical.tp.decrease <- length(which(full.clinical.results[[f]][idx.full.glm.tp] != 0))
  
  relative.change.full.clinical.tn.total <- full.clinical.tn - full.glm.tn
  relative.change.full.clinical.tn.increase <- length(which(full.glm.results[[f]][idx.full.clinical.tn] != 1))
  relative.change.full.clinical.tn.decrease <- length(which(full.clinical.results[[f]][idx.full.glm.tn] != 1))
  
  relative.change.full.clinical.fp.total <- full.clinical.fp - full.glm.fp
  relative.change.full.clinical.fp.increase <- length(which(full.glm.results[[f]][idx.full.clinical.fp] != 2))
  relative.change.full.clinical.fp.decrease <- length(which(full.clinical.results[[f]][idx.full.glm.fp] != 2))
  
  relative.change.full.clinical.fn.total <- full.clinical.fn - full.glm.fn
  relative.change.full.clinical.fn.increase <- length(which(full.glm.results[[f]][idx.full.clinical.fn] != 3))
  relative.change.full.clinical.fn.decrease <- length(which(full.clinical.results[[f]][idx.full.glm.fn] != 3))
  list.relative.full.clinical <- vector()
  list.relative.full.clinical[1] <- relative.change.full.clinical.tp.total
  list.relative.full.clinical[2] <- relative.change.full.clinical.tp.increase
  list.relative.full.clinical[3] <- relative.change.full.clinical.tp.decrease
  list.relative.full.clinical[4] <- relative.change.full.clinical.tn.total
  list.relative.full.clinical[5] <- relative.change.full.clinical.tn.increase
  list.relative.full.clinical[6] <- relative.change.full.clinical.tn.decrease
  list.relative.full.clinical[7] <- relative.change.full.clinical.fp.total
  list.relative.full.clinical[8] <- relative.change.full.clinical.fp.increase
  list.relative.full.clinical[9] <- relative.change.full.clinical.fp.decrease
  list.relative.full.clinical[10] <- relative.change.full.clinical.fn.total
  list.relative.full.clinical[11] <- relative.change.full.clinical.fn.increase
  list.relative.full.clinical[12] <- relative.change.full.clinical.fn.decrease
  
  relative.change.full.all.tp.total <- full.all.tp - full.clinical.tp
  relative.change.full.all.tp.increase <- length(which(full.clinical.results[[f]][idx.full.all.tp] != 0))
  relative.change.full.all.tp.decrease <- length(which(full.all.results[[f]][idx.full.clinical.tp] != 0))
  
  relative.change.full.all.tn.total <- full.all.tn - full.clinical.tn
  relative.change.full.all.tn.increase <- length(which(full.clinical.results[[f]][idx.full.all.tn] != 1))
  relative.change.full.all.tn.decrease <- length(which(full.all.results[[f]][idx.full.clinical.tn] != 1))
  
  relative.change.full.all.fp.total <- full.all.fp - full.clinical.fp
  relative.change.full.all.fp.increase <- length(which(full.clinical.results[[f]][idx.full.all.fp] != 2))
  relative.change.full.all.fp.decrease <- length(which(full.all.results[[f]][idx.full.clinical.fp] != 2))
  
  relative.change.full.all.fn.total <- full.all.fn - full.clinical.fn
  relative.change.full.all.fn.increase <- length(which(full.clinical.results[[f]][idx.full.all.fn] != 3))
  relative.change.full.all.fn.decrease <- length(which(full.all.results[[f]][idx.full.clinical.fn] != 3))
  list.relative.full.all <- vector()
  list.relative.full.all[1] <- relative.change.full.all.tp.total
  list.relative.full.all[2] <- relative.change.full.all.tp.increase
  list.relative.full.all[3] <- relative.change.full.all.tp.decrease
  list.relative.full.all[4] <- relative.change.full.all.tn.total
  list.relative.full.all[5] <- relative.change.full.all.tn.increase
  list.relative.full.all[6] <- relative.change.full.all.tn.decrease
  list.relative.full.all[7] <- relative.change.full.all.fp.total
  list.relative.full.all[8] <- relative.change.full.all.fp.increase
  list.relative.full.all[9] <- relative.change.full.all.fp.decrease
  list.relative.full.all[10] <- relative.change.full.all.fn.total
  list.relative.full.all[11] <- relative.change.full.all.fn.increase
  list.relative.full.all[12] <- relative.change.full.all.fn.decrease
  
  #Absolute Change
  change.full.all.tp.total <- full.all.tp - rao.people.tp
  change.full.all.tp.increase <- length(which(rao.people.results[[f]][idx.full.all.tp] != 0))
  change.full.all.tp.decrease <- length(which(full.all.results[[f]][idx.rao.people.tp] != 0))
  
  change.full.all.tn.total <- full.all.tn - rao.people.tn
  change.full.all.tn.increase <- length(which(rao.people.results[[f]][idx.full.all.tn] != 1))
  change.full.all.tn.decrease <- length(which(full.all.results[[f]][idx.rao.people.tn] != 1))
  
  change.full.all.fp.total <- full.all.fp - rao.people.fp
  change.full.all.fp.increase <- length(which(rao.people.results[[f]][idx.full.all.fp] != 2))
  change.full.all.fp.decrease <- length(which(full.all.results[[f]][idx.rao.people.fp] != 2))
  
  change.full.all.fn.total <- full.all.fn - rao.people.fn
  change.full.all.fn.increase <- length(which(rao.people.results[[f]][idx.full.all.fn] != 3))
  change.full.all.fn.decrease <- length(which(full.all.results[[f]][idx.rao.people.fn] != 3))
  list.full.all <- vector()
  list.full.all[1] <- change.full.all.tp.total
  list.full.all[2] <- change.full.all.tp.increase
  list.full.all[3] <- change.full.all.tp.decrease
  list.full.all[4] <- change.full.all.tn.total
  list.full.all[5] <- change.full.all.tn.increase
  list.full.all[6] <- change.full.all.tn.decrease
  list.full.all[7] <- change.full.all.fp.total
  list.full.all[8] <- change.full.all.fp.increase
  list.full.all[9] <- change.full.all.fp.decrease
  list.full.all[10] <- change.full.all.fn.total
  list.full.all[11] <- change.full.all.fn.increase
  list.full.all[12] <- change.full.all.fn.decrease
  
  list.folds[[1]] <- list.relative.full.glm
  list.folds[[2]] <- list.relative.full.clinical
  list.folds[[3]] <- list.relative.full.all
  list.folds[[4]] <- list.full.all
  
  list.results[[f]] <- list.folds
  
  }


rao.tp <- 0
rao.tn <- 0
rao.fp <- 0
rao.fn <- 0
for(f in folds) {
  rao.tp <- rao.tp + length(which(rao.people.results[[f]] == 0))
  rao.tn <- rao.tn + length(which(rao.people.results[[f]] == 1))
  rao.fp <- rao.fp + length(which(rao.people.results[[f]] == 2))
  rao.fn <- rao.fn + length(which(rao.people.results[[f]] == 3))
  
}

full.glm.tp <- 0
full.glm.tn <- 0
full.glm.fp <- 0
full.glm.fn <- 0
full.glm.tp.change <- 0
full.glm.tn.change <- 0
full.glm.fp.change <- 0
full.glm.fn.change <- 0
full.glm.tp.increase <- 0
full.glm.tn.increase <- 0
full.glm.fp.increase <- 0
full.glm.fn.increase <- 0
full.glm.tp.decrease <- 0
full.glm.tn.decrease <- 0
full.glm.fp.decrease <- 0
full.glm.fn.decrease <- 0

for(f in folds) {
  
  full.glm.tp <- full.glm.tp + length(which(full.glm.results[[f]] == 0))
  full.glm.tn <- full.glm.tn + length(which(full.glm.results[[f]] == 1))
  full.glm.fp <- full.glm.fp + length(which(full.glm.results[[f]] == 2))
  full.glm.fn <- full.glm.fn + length(which(full.glm.results[[f]] == 3))
  
  full.glm.tp.change <- full.glm.tp.change + list.results[[f]][[1]][[1]]
  full.glm.tp.increase <- full.glm.tp.increase + list.results[[f]][[1]][[2]]
  full.glm.tp.decrease <- full.glm.tp.decrease + list.results[[f]][[1]][[3]]
  
  full.glm.tn.change <- full.glm.tn.change + list.results[[f]][[1]][[4]]
  full.glm.tn.increase <- full.glm.tn.increase + list.results[[f]][[1]][[5]]
  full.glm.tn.decrease <- full.glm.tn.decrease + list.results[[f]][[1]][[6]]
  
  
  full.glm.fp.change <- full.glm.fp.change + list.results[[f]][[1]][[7]]
  full.glm.fp.increase <- full.glm.fp.increase + list.results[[f]][[1]][[8]]
  full.glm.fp.decrease <- full.glm.fp.decrease + list.results[[f]][[1]][[9]]
  
  
  full.glm.fn.change <- full.glm.fn.change + list.results[[f]][[1]][[10]]
  full.glm.fn.increase <- full.glm.fn.increase + list.results[[f]][[1]][[11]]
  full.glm.fn.decrease <- full.glm.fn.decrease + list.results[[f]][[1]][[12]]
  
}

full.clinical.tp <- 0
full.clinical.tn <- 0
full.clinical.fp <- 0
full.clinical.fn <- 0
full.clinical.tp.change <- 0
full.clinical.tn.change <- 0
full.clinical.fp.change <- 0
full.clinical.fn.change <- 0
full.clinical.tp.increase <- 0
full.clinical.tn.increase <- 0
full.clinical.fp.increase <- 0
full.clinical.fn.increase <- 0
full.clinical.tp.decrease <- 0
full.clinical.tn.decrease <- 0
full.clinical.fp.decrease <- 0
full.clinical.fn.decrease <- 0

for(f in folds) {
  
  full.clinical.tp <- full.clinical.tp + length(which(full.clinical.results[[f]] == 0))
  full.clinical.tn <- full.clinical.tn + length(which(full.clinical.results[[f]] == 1))
  full.clinical.fp <- full.clinical.fp + length(which(full.clinical.results[[f]] == 2))
  full.clinical.fn <- full.clinical.fn + length(which(full.clinical.results[[f]] == 3))
  
  full.clinical.tp.change <- full.clinical.tp.change + list.results[[f]][[2]][[1]]
  full.clinical.tp.increase <- full.clinical.tp.increase + list.results[[f]][[2]][[2]]
  full.clinical.tp.decrease <- full.clinical.tp.decrease + list.results[[f]][[2]][[3]]
  
  full.clinical.tn.change <- full.clinical.tn.change + list.results[[f]][[2]][[4]]
  full.clinical.tn.increase <- full.clinical.tn.increase + list.results[[f]][[2]][[5]]
  full.clinical.tn.decrease <- full.clinical.tn.decrease + list.results[[f]][[2]][[6]]
  
  
  full.clinical.fp.change <- full.clinical.fp.change + list.results[[f]][[2]][[7]]
  full.clinical.fp.increase <- full.clinical.fp.increase + list.results[[f]][[2]][[8]]
  full.clinical.fp.decrease <- full.clinical.fp.decrease + list.results[[f]][[2]][[9]]
  
  
  full.clinical.fn.change <- full.clinical.fn.change + list.results[[f]][[2]][[10]]
  full.clinical.fn.increase <- full.clinical.fn.increase + list.results[[f]][[2]][[11]]
  full.clinical.fn.decrease <- full.clinical.fn.decrease + list.results[[f]][[2]][[12]]
  
}

full.all.tp <- 0
full.all.tn <- 0
full.all.fp <- 0
full.all.fn <- 0
full.all.tp.change <- 0
full.all.tn.change <- 0
full.all.fp.change <- 0
full.all.fn.change <- 0
full.all.tp.increase <- 0
full.all.tn.increase <- 0
full.all.fp.increase <- 0
full.all.fn.increase <- 0
full.all.tp.decrease <- 0
full.all.tn.decrease <- 0
full.all.fp.decrease <- 0
full.all.fn.decrease <- 0

for(f in folds) {
  
  full.all.tp <- full.all.tp + length(which(full.all.results[[f]] == 0))
  full.all.tn <- full.all.tn + length(which(full.all.results[[f]] == 1))
  full.all.fp <- full.all.fp + length(which(full.all.results[[f]] == 2))
  full.all.fn <- full.all.fn + length(which(full.all.results[[f]] == 3))
  
  full.all.tp.change <- full.all.tp.change + list.results[[f]][[3]][[1]]
  full.all.tp.increase <- full.all.tp.increase + list.results[[f]][[3]][[2]]
  full.all.tp.decrease <- full.all.tp.decrease + list.results[[f]][[3]][[3]]
  
  full.all.tn.change <- full.all.tn.change + list.results[[f]][[3]][[4]]
  full.all.tn.increase <- full.all.tn.increase + list.results[[f]][[3]][[5]]
  full.all.tn.decrease <- full.all.tn.decrease + list.results[[f]][[3]][[6]]
  
  
  full.all.fp.change <- full.all.fp.change + list.results[[f]][[3]][[7]]
  full.all.fp.increase <- full.all.fp.increase + list.results[[f]][[3]][[8]]
  full.all.fp.decrease <- full.all.fp.decrease + list.results[[f]][[3]][[9]]
  
  
  full.all.fn.change <- full.all.fn.change + list.results[[f]][[3]][[10]]
  full.all.fn.increase <- full.all.fn.increase + list.results[[f]][[3]][[11]]
  full.all.fn.decrease <- full.all.fn.decrease + list.results[[f]][[3]][[12]]
  
}

full.all.absolute.tp <- 0
full.all.absolute.tn <- 0
full.all.absolute.fp <- 0
full.all.absolute.fn <- 0
full.all.absolute.tp.change <- 0
full.all.absolute.tn.change <- 0
full.all.absolute.fp.change <- 0
full.all.absolute.fn.change <- 0
full.all.absolute.tp.increase <- 0
full.all.absolute.tn.increase <- 0
full.all.absolute.fp.increase <- 0
full.all.absolute.fn.increase <- 0
full.all.absolute.tp.decrease <- 0
full.all.absolute.tn.decrease <- 0
full.all.absolute.fp.decrease <- 0
full.all.absolute.fn.decrease <- 0

for(f in folds) {
  
  full.all.absolute.tp <- full.all.absolute.tp + length(which(full.all.results[[f]] == 0))
  full.all.absolute.tn <- full.all.absolute.tn + length(which(full.all.results[[f]] == 1))
  full.all.absolute.fp <- full.all.absolute.fp + length(which(full.all.results[[f]] == 2))
  full.all.absolute.fn <- full.all.absolute.fn + length(which(full.all.results[[f]] == 3))
  
  full.all.absolute.tp.change <- full.all.absolute.tp.change + list.results[[f]][[4]][[1]]
  full.all.absolute.tp.increase <- full.all.absolute.tp.increase + list.results[[f]][[4]][[2]]
  full.all.absolute.tp.decrease <- full.all.absolute.tp.decrease + list.results[[f]][[4]][[3]]
  
  full.all.absolute.tn.change <- full.all.absolute.tn.change + list.results[[f]][[4]][[4]]
  full.all.absolute.tn.increase <- full.all.absolute.tn.increase + list.results[[f]][[4]][[5]]
  full.all.absolute.tn.decrease <- full.all.absolute.tn.decrease + list.results[[f]][[4]][[6]]
  
  
  full.all.absolute.fp.change <- full.all.absolute.fp.change + list.results[[f]][[4]][[7]]
  full.all.absolute.fp.increase <- full.all.absolute.fp.increase + list.results[[f]][[4]][[8]]
  full.all.absolute.fp.decrease <- full.all.absolute.fp.decrease + list.results[[f]][[4]][[9]]
  
  
  full.all.absolute.fn.change <- full.all.absolute.fn.change + list.results[[f]][[4]][[10]]
  full.all.absolute.fn.increase <- full.all.absolute.fn.increase + list.results[[f]][[4]][[11]]
  full.all.absolute.fn.decrease <- full.all.absolute.fn.decrease + list.results[[f]][[4]][[12]]
  
}

#Then Match Indices where possible
save.image(file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/patient_movement_rao_people_glm.RData')
