#5-fold cross-validation
library(pROC)
library(xgboost)
library(foreach)
library(doParallel)
registerDoParallel()
MODE <- 'clinical'#'site'#'clinical'# 'all'
load(file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/five_fold_data_V8_7JUN2016.RData'))
load(file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/full_var_imp_',MODE,'_five_fold_data_V8_7JUN2016.RData', sep=''))
source('./fMeasure.R')
#Existing Model in CV
folds <- c(1,2,3,4,5)

folds.feat.auc <- list()
for(feat.idx in 1:20){ #length(feat.names)) {
  cat(paste('Forward Selcetion, Num Features:', feat.idx,'\n'))
# folds.tp <- vector()
# folds.tn <- vector()
# folds.fp <- vector()
# folds.fn <- vector()
# folds.f <- vector()
  folds.auc <- vector()
#lists.f <- list()


for(f in folds) {
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
                       nrounds=1000, eta=0.1, max.depth=6, objective='binary:logistic') 
  cat(paste('f: ', f,' predict\n'))
  predict.bleed <- predict(model.bleed, as.matrix(test.data))
  cat(paste('f: ', f,' roc building\n'))
  roc.bleed <- roc(as.numeric(test.labels),  as.numeric(predict.bleed))
  cat(paste('f: ', f,' auc: ', roc.bleed$auc,'\n'))
#  cat(paste('f: ', f,' f building\n'))
  #f.res <- optimalROC(predict.bleed, test.labels)
  
  folds.auc[f] <- roc.bleed$auc
}
  cat(paste('mean auc:', mean(folds.auc), '\n'))
folds.feat.auc[[feat.idx]] <- folds.auc
gc()
}#End of Forward Selection Loop
save(folds.feat.auc, file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/full_XGB_Top20_varauc_',MODE,'_five_fold_data_V8_7JUN2016.RData', sep=''))

list.xgb.models <- list()
folds <- c(1,2,3,4,5)
for(f in folds) {
  cat(paste('fold: ', f, '\n'))
  if(f == 1) {
    train.data <- train1.data
    train.labels <- train1.labels
  } else if (f == 2) {
    rm(train1.data)
    rm(train1.labels)
    train.data <- train2.data
    train.labels <- train2.labels
  } else if (f == 3) {
    rm(train2.data)
    rm(train2.labels)
    train.data <- train3.data
    train.labels <- train3.labels
  } else if (f == 4) {
    rm(train3.data)
    rm(train3.labels)
    train.data <- train4.data
    train.labels <- train4.labels
  } else {
    rm(train4.data)
    rm(train4.labels)
    train.data <- train5.data
    train.labels <- train5.labels
  }
  
  cat(paste('f: ', f,' model building\n'))
  #model.bleed <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), nrounds=100, verbose=0)#cv.glmnet(x=as.matrix(train.data), y=as.factor(train.labels), family='binomial')
  model.bleed <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), verbose=0,
                         nrounds=1000, eta=0.1, max.depth=6, objective='binary:logistic') 
  list.xgb.models[[f]] <- model.bleed
  
}
save('list.xgb.models', file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/xgb_models_five_fold_data_V2_07MAR2016.RData')

#save.image("Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/five_fold_data_V1_glmnet_xgboost_4DEC2015.RData")

load(file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/xgb_models_five_fold_data_V2_07MAR2016.RData')
list.importance <- list()
list.predictors <- list()
search.space <- seq(50,1000,50)
search.space <- c(1,2,3,4,5,10,25,search.space)
for(f in c(1,2,3,4,5)) {
  cat(paste('fold: ', f, '\n'))
  if(f == 1) {
    test.data <- test1.data
    test.labels <- test1.labels
  } else if (f == 2) {
    test.data <- test2.data
    test.labels <- test2.labels
  } else if (f == 3) {
    test.data <- test3.data
    test.labels <- test3.labels
  } else if (f == 4) {
    test.data <- test4.data
    test.labels <- test4.labels
  } else {
    test.data <- test5.data
    test.labels <- test5.labels
  }
  
  #f.res <- optimalROC(xgboost.list.responses[[f]], test.labels)
  #allROC_par(xgboost.list.responses[[f]], test.labels)[[1]]
  #f.results <- allROC_par(xgboost.list.responses[[f]], test.labels)
  #f.res <- f.results[[1]]
  model.bleed <- list.xgb.models[[f]]
  list.predict <- list()
  list.roc <- list()
  list.f <- list()
  
  for(d in 1:length(search.space)) {
    print(search.space[d])
    predict.bleed <- predict(model.bleed, as.matrix(test.data), ntreelimit=search.space[d])
    roc.bleed <- roc(as.numeric(test.labels), as.numeric(predict.bleed))
    f.bleed <- allROC_par(predict.bleed, test.labels)[[1]]
    list.predict[[d]] <- predict.bleed
    list.roc[[d]] <- roc.bleed
    list.f[[d]] <- f.bleed
    }
  
  list.fold <- list()
  list.fold[[1]] <- list.predict
  list.fold[[2]] <- list.roc
  list.fold[[3]] <- list.f
  list.predictors[[f]] <- list.fold
  list.importance[[f]] <- xgb.importance(feature_names = feat.names, model=model.bleed)
#   xgboost.folds.tp[f] <- f.res$tp
#   xgboost.folds.tn[f] <- f.res$tn
#   xgboost.folds.fp[f] <- f.res$fp
#   xgboost.folds.fn[f] <- f.res$fn
#   xgboost.folds.f[f] <- f.res$f.score
}

#XGB Variable Importance

mean.points <- vector()
lower.points <- vector()
upper.points <- vector()

for(j in 1:length(search.space)) {
  feat.vals <- vector()
  feat.vals[1] <- list.predictors[[1]][[2]][[j]]$auc
  feat.vals[2] <- list.predictors[[2]][[2]][[j]]$auc
  feat.vals[3] <- list.predictors[[3]][[2]][[j]]$auc
  feat.vals[4] <- list.predictors[[4]][[2]][[j]]$auc
  feat.vals[5] <- list.predictors[[5]][[2]][[j]]$auc
  
  conf.int <- t.test(feat.vals)
  mean.points[j] <- conf.int$estimate
  lower.points[j] <- conf.int$conf.int[1]
  upper.points[j] <- conf.int$conf.int[2]
  
  }

library(ggplot2)

aucs.df <- data.frame(search.space, mean.points, lower.points, upper.points)

g <- ggplot(aucs.df, aes(x=search.space, y=mean.points))
g + geom_line()



source('calcDeciles.R')
#Create List of Probs - Logistic
list.probs <- list()
list.xgb <- list()
for(i in 1:5) {
  list.probs[[i]] <-xgboost.list.responses[[i]]#list.responses[[i]]
  #list.xgb[[i]] <- xgboost.list.responses[[i]]
}

list.points <- quartPoints(list.probs, 5)
means <- vector()
lowers <- vector()
uppers <- vector()
for(i in 1:5) {
  t <- t.test(list.points[[i]])
  means[i] <- t$estimate
  lowers[i] <- t$conf.int[1]
  uppers[i] <- t$conf.int[2]
}

quarts.df <- data.frame(means, lowers, uppers)
colnames(quarts.df) <- c('means', 'lowers', 'uppers')
rownames(quarts.df) <- c('0%', '25%', '50%', '75%', '100%')

#Unified Probs
probs <- vector()
labels <- vector()
for(i in 1:5) {
  probs <- c(probs, list.probs[[i]])
  if(i == 1) {
    
    test.labels <- test1.labels
  } else if(i == 2) {
    
    test.labels <- test2.labels    
  } else if(i == 3) {
    
    test.labels <- test3.labels
  } else if(i == 4) {
    
    test.labels <- test4.labels
  } else {
    
    test.labels <- test5.labels
  }
  labels <- c(labels, test.labels)
}

quarts.res <- sortQuartiles(probs, labels, quarts.df$means )

list.points <- decPoints(list.probs, 5)
means <- vector()
lowers <- vector()
uppers <- vector()
for(i in 1:11) {
  t <- t.test(list.points[[i]])
  means[i] <- t$estimate
  lowers[i] <- t$conf.int[1]
  uppers[i] <- t$conf.int[2]
}

dec.df <- data.frame(means, lowers, uppers)
colnames(dec.df) <- c('means', 'lowers', 'uppers')
rownames(dec.df) <- c('0%', '10%', '20%', '30%', '40%', '50%', '60%', '70%','80%', '90%', '100%')

#Unified Probs
probs <- vector()
labels <- vector()
for(i in 1:5) {
  probs <- c(probs, list.probs[[i]])
  if(i == 1) {
    
    test.labels <- test1.labels
  } else if(i == 2) {
    
    test.labels <- test2.labels    
  } else if(i == 3) {
    
    test.labels <- test3.labels
  } else if(i == 4) {
    
    test.labels <- test4.labels
  } else {
    
    test.labels <- test5.labels
  }
  labels <- c(labels, test.labels)
}

decs.res <- sortDeciles(probs, labels, dec.df$means )
