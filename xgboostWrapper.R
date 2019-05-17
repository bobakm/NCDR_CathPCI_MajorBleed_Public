library(xgboost)
library(pROC)
library(doParallel)
library(foreach)
#Parameters
#Tree Depth 5 to 10
#Eta 0.1 to 0.8
#Thread all of them
#Iterations 500-1000

xgcv <- function(train.data, train.labels, tree.depth, eta, iters, verbose) {
  #print(tree.depth)
  #print(eta)
  #print(iters)
  #run internal cross validation to determine the best via a grid search
  #expects a vector for tree.depth, eta, and iters
  if(verbose == TRUE) {
    cat(paste('xgcv: setting up internal cv\n'))
  }
  data <- cbind(train.data, train.labels)
  
  #5-fold CV?
  positives <- which(data$train.labels == 1)
  negatives <- which(data$train.labels == 0)
  
  data.positives <- data[positives,]
  data.negatives <- data[negatives,]
  
  numpos <- round(0.2*length(positives))
  numneg <- round(0.2*length(negatives))
  
  cv.fold1.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold1.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold1.pos <- data.positives[cv.fold1.idx.pos,]
  fold1.neg <- data.negatives[cv.fold1.idx.neg,]
  
  data.positives <- data.positives[-cv.fold1.idx.pos,]
  data.negatives <- data.negatives[-cv.fold1.idx.neg,]
  
  cv.fold2.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold2.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold2.pos <- data.positives[cv.fold2.idx.pos,]
  fold2.neg <- data.negatives[cv.fold2.idx.neg,]
  
  data.positives <- data.positives[-cv.fold2.idx.pos,]
  data.negatives <- data.negatives[-cv.fold2.idx.neg,]
  
  cv.fold3.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold3.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold3.pos <- data.positives[cv.fold3.idx.pos,]
  fold3.neg <- data.negatives[cv.fold3.idx.neg,]
  
  data.positives <- data.positives[-cv.fold3.idx.pos,]
  data.negatives <- data.negatives[-cv.fold3.idx.neg,]
  
  cv.fold4.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold4.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold4.pos <- data.positives[cv.fold4.idx.pos,]
  fold4.neg <- data.negatives[cv.fold4.idx.neg,]
  
  data.positives <- data.positives[-cv.fold4.idx.pos,]
  data.negatives <- data.negatives[-cv.fold4.idx.neg,]
  
  fold5.pos <- data.positives
  fold5.neg <- data.negatives
  
  train1 <- rbind(fold2.pos, fold2.neg,
                  fold3.pos, fold3.neg,
                  fold4.pos, fold4.neg,
                  fold5.pos, fold5.neg)
  
  test1 <- rbind(fold1.pos, fold1.neg)
  
  train2 <- rbind(fold1.pos, fold1.neg,
                  fold3.pos, fold3.neg,
                  fold4.pos, fold4.neg,
                  fold5.pos, fold5.neg)
  
  test2 <- rbind(fold2.pos, fold2.neg)
  
  train3 <- rbind(fold2.pos, fold2.neg,
                  fold1.pos, fold1.neg,
                  fold4.pos, fold4.neg,
                  fold5.pos, fold5.neg)
  
  test3 <- rbind(fold3.pos, fold3.neg)
  
  train4 <- rbind(fold2.pos, fold2.neg,
                  fold3.pos, fold3.neg,
                  fold1.pos, fold1.neg,
                  fold5.pos, fold5.neg)
  
  test4 <- rbind(fold4.pos, fold4.neg)
  
  train5 <- rbind(fold2.pos, fold2.neg,
                  fold3.pos, fold3.neg,
                  fold4.pos, fold4.neg,
                  fold1.pos, fold1.neg)
  
  test5 <- rbind(fold5.pos, fold5.neg)
  
  if(verbose == TRUE) {
    cat(paste('xgcv: finished internal cv\n'))
  }
  
  best.auc <- -Inf
  best.tr <- NA
  best.eta <- NA
  best.iter <- NA
  
  for(tr in tree.depth) {
    for(et in eta) {
      for(it in iters) {

          xgb.auc <- vector()
          
        if(verbose == TRUE) {
          cat(paste('-----------------------\n'))
          cat(paste('xgcv: depth:', tr,'\n'))
          cat(paste('xgcv: eta:', et,'\n'))
          cat(paste('xgcv: iter:', it,'\n'))
        }
        
               
        for(f in c(1,2,3,4,5)) {
          #each fold
          if(verbose == TRUE) {
            cat(paste('xgcv: fold:', f,'\n'))
          }
          if(f == 1) {
            train <- train1
            test <- test1
          } else if(f == 2) {
            train <- train2
            test <- test2
          } else if(f == 3) {
            train <- train3
            test <- test3
          } else if(f == 4) {
            train <- train4
            test <- test4
          } else {
            train <- train5
            test <- test5
          }
          train.labels <- train$train.labels
          train.data <- train[,which(!(colnames(train) %in% c('train.labels')))]
#           print(dim(train))
#           print(dim(train.data))
#           print(length(train.labels))
#           print(colnames(train))
#           print(colnames(train.data))
          test.labels <- test$train.labels
          test.data <- test[,which(!(colnames(test) %in% c('train.labels')))]
          
          model <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), verbose=0, nrounds=it,
                           eta=et, max.depth=tr, objective='binary:logistic')
          resp <- predict(model, as.matrix(test.data))
          roc.model <- roc(as.numeric(test.labels), as.numeric(resp))
          xgb.auc <- c(xgb.auc, roc.model$auc)
        } 
        #Return highest mean C-stat
          if(verbose == TRUE) {
            cat(paste('xgcv: prev.auc:', best.auc,' this round:', mean(xgb.auc),'\n'))
            cat(paste('-----------------------\n'))
          }
          
          if(mean(xgb.auc) >= best.auc) {
            best.auc <- mean(xgb.auc)
	          best.tr <- tr
            best.eta <- et
            best.iter <- it
          }
          
      }
    }
  }
  
  #Return parameters in a vector
  return(c(best.eta, best.tr, best.iter))
  
}

xgcv_par <- function(train.data, train.labels, tree.depth, eta, iters, verbose) {
  #print(tree.depth)
  #print(eta)
  #print(iters)
  #run internal cross validation to determine the best via a grid search
  #expects a vector for tree.depth, eta, and iters
  if(verbose == TRUE) {
    cat(paste('xgcv: setting up internal cv\n'))
  }
  data <- cbind(train.data, train.labels)
  
  #5-fold CV?
  positives <- which(data$train.labels == 1)
  negatives <- which(data$train.labels == 0)
  
  data.positives <- data[positives,]
  data.negatives <- data[negatives,]
  
  numpos <- round(0.2*length(positives))
  numneg <- round(0.2*length(negatives))
  
  cv.fold1.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold1.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold1.pos <- data.positives[cv.fold1.idx.pos,]
  fold1.neg <- data.negatives[cv.fold1.idx.neg,]
  
  data.positives <- data.positives[-cv.fold1.idx.pos,]
  data.negatives <- data.negatives[-cv.fold1.idx.neg,]
  
  cv.fold2.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold2.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold2.pos <- data.positives[cv.fold2.idx.pos,]
  fold2.neg <- data.negatives[cv.fold2.idx.neg,]
  
  data.positives <- data.positives[-cv.fold2.idx.pos,]
  data.negatives <- data.negatives[-cv.fold2.idx.neg,]
  
  cv.fold3.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold3.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold3.pos <- data.positives[cv.fold3.idx.pos,]
  fold3.neg <- data.negatives[cv.fold3.idx.neg,]
  
  data.positives <- data.positives[-cv.fold3.idx.pos,]
  data.negatives <- data.negatives[-cv.fold3.idx.neg,]
  
  cv.fold4.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold4.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold4.pos <- data.positives[cv.fold4.idx.pos,]
  fold4.neg <- data.negatives[cv.fold4.idx.neg,]
  
  data.positives <- data.positives[-cv.fold4.idx.pos,]
  data.negatives <- data.negatives[-cv.fold4.idx.neg,]
  
  fold5.pos <- data.positives
  fold5.neg <- data.negatives
  
  train1 <- rbind(fold2.pos, fold2.neg,
                  fold3.pos, fold3.neg,
                  fold4.pos, fold4.neg,
                  fold5.pos, fold5.neg)
  
  test1 <- rbind(fold1.pos, fold1.neg)
  
  train2 <- rbind(fold1.pos, fold1.neg,
                  fold3.pos, fold3.neg,
                  fold4.pos, fold4.neg,
                  fold5.pos, fold5.neg)
  
  test2 <- rbind(fold2.pos, fold2.neg)
  
  train3 <- rbind(fold2.pos, fold2.neg,
                  fold1.pos, fold1.neg,
                  fold4.pos, fold4.neg,
                  fold5.pos, fold5.neg)
  
  test3 <- rbind(fold3.pos, fold3.neg)
  
  train4 <- rbind(fold2.pos, fold2.neg,
                  fold3.pos, fold3.neg,
                  fold1.pos, fold1.neg,
                  fold5.pos, fold5.neg)
  
  test4 <- rbind(fold4.pos, fold4.neg)
  
  train5 <- rbind(fold2.pos, fold2.neg,
                  fold3.pos, fold3.neg,
                  fold4.pos, fold4.neg,
                  fold1.pos, fold1.neg)
  
  test5 <- rbind(fold5.pos, fold5.neg)
  
  if(verbose == TRUE) {
    cat(paste('xgcv: finished internal cv\n'))
    writeLines(c(''), 'xgcv_log.txt')
  }
  
  best.auc <- -Inf
  best.tr <- NA
  best.eta <- NA
  best.iter <- NA
  
  list.options <- list()
  i <- 1
  for(dp in tree.depth) {
    for(et in eta) {
      for(itr in iters) {
        values <- c(dp, et, itr)
        list.options[[i]] <- values
        i <- i+1
      }
    }
  }
  
  registerDoParallel(cores=64)
  
  opts.values <- foreach(opt=list.options, .combine=cbind) %dopar% {
  
              xgb.auc <- vector()
              tr <- opt[1]
              et <- opt[2]
              it <- opt[3]
              
            
            if(verbose == TRUE) {
              #cat(paste('xgcv: ', opt,'\n'))
              sink('xgcv_log.txt', append=TRUE)
		
            }
            
            
            for(f in c(1,2,3,4,5)) {
              #each fold
              if(verbose == TRUE) {
                cat(paste('xgcv: opt:', opt, 'fold:', f,'\n'))
              }
              if(f == 1) {
                train <- train1
                test <- test1
              } else if(f == 2) {
                train <- train2
                test <- test2
              } else if(f == 3) {
                train <- train3
                test <- test3
              } else if(f == 4) {
                train <- train4
                test <- test4
              } else {
                train <- train5
                test <- test5
              }
              train.labels <- train$train.labels
              train.data <- train[,which(!(colnames(train) %in% c('train.labels')))]
              #           print(dim(train))
              #           print(dim(train.data))
              #           print(length(train.labels))
              #           print(colnames(train))
              #           print(colnames(train.data))
              test.labels <- test$train.labels
              test.data <- test[,which(!(colnames(test) %in% c('train.labels')))]
              
              model <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), 
                               verbose=0, nrounds=it,
                               eta=et, max.depth=tr, objective='binary:logistic')
              resp <- predict(model, as.matrix(test.data))
              roc.model <- roc(as.numeric(test.labels), as.numeric(resp))
              xgb.auc <- c(xgb.auc, roc.model$auc)
            }
              if(verbose == TRUE) {
                cat(paste('xgcv: opt:', opt, 'aucs:', xgb.auc,'\n'))
              }
              xgb.auc <- xgb.auc
    
  }
  df <- data.frame(opts.values)
  mean.aucs <- colMeans(df)
  val <- max(mean.aucs)
  idx <- which(val == mean.aucs)[[1]]
  final.opts <- list.options[[idx]]
  best.eta <- final.opts[2]
  best.tr <- final.opts[1]
  best.it <- final.opts[3]
  return(c(best.eta, best.tr, best.iter))
  
}

xgcv_nice <- function(train.data, train.labels, eta, tree.depth, iters, verbose) {
  #print(tree.depth)
  #print(eta)
  #print(iters)
  #run internal cross validation to determine the best via a grid search
  #expects a vector for tree.depth, eta, and iters
  if(verbose == TRUE) {
    cat(paste('xgcv: setting up internal cv\n'))
  }
  data <- cbind(train.data, train.labels)
  
  #5-fold CV?
  positives <- which(data$train.labels == 1)
  negatives <- which(data$train.labels == 0)
  
  data.positives <- data[positives,]
  data.negatives <- data[negatives,]
  
  numpos <- round(0.2*length(positives))
  numneg <- round(0.2*length(negatives))
  
  cv.fold1.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold1.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold1.pos <- data.positives[cv.fold1.idx.pos,]
  fold1.neg <- data.negatives[cv.fold1.idx.neg,]
  
  data.positives <- data.positives[-cv.fold1.idx.pos,]
  data.negatives <- data.negatives[-cv.fold1.idx.neg,]
  
  cv.fold2.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold2.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold2.pos <- data.positives[cv.fold2.idx.pos,]
  fold2.neg <- data.negatives[cv.fold2.idx.neg,]
  
  data.positives <- data.positives[-cv.fold2.idx.pos,]
  data.negatives <- data.negatives[-cv.fold2.idx.neg,]
  
  cv.fold3.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold3.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold3.pos <- data.positives[cv.fold3.idx.pos,]
  fold3.neg <- data.negatives[cv.fold3.idx.neg,]
  
  data.positives <- data.positives[-cv.fold3.idx.pos,]
  data.negatives <- data.negatives[-cv.fold3.idx.neg,]
  
  cv.fold4.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold4.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold4.pos <- data.positives[cv.fold4.idx.pos,]
  fold4.neg <- data.negatives[cv.fold4.idx.neg,]
  
  data.positives <- data.positives[-cv.fold4.idx.pos,]
  data.negatives <- data.negatives[-cv.fold4.idx.neg,]
  
  fold5.pos <- data.positives
  fold5.neg <- data.negatives
  
  train1 <- rbind(fold2.pos, fold2.neg,
                  fold3.pos, fold3.neg,
                  fold4.pos, fold4.neg,
                  fold5.pos, fold5.neg)
  
  test1 <- rbind(fold1.pos, fold1.neg)
  
  train2 <- rbind(fold1.pos, fold1.neg,
                  fold3.pos, fold3.neg,
                  fold4.pos, fold4.neg,
                  fold5.pos, fold5.neg)
  
  test2 <- rbind(fold2.pos, fold2.neg)
  
  train3 <- rbind(fold2.pos, fold2.neg,
                  fold1.pos, fold1.neg,
                  fold4.pos, fold4.neg,
                  fold5.pos, fold5.neg)
  
  test3 <- rbind(fold3.pos, fold3.neg)
  
  train4 <- rbind(fold2.pos, fold2.neg,
                  fold3.pos, fold3.neg,
                  fold1.pos, fold1.neg,
                  fold5.pos, fold5.neg)
  
  test4 <- rbind(fold4.pos, fold4.neg)
  
  train5 <- rbind(fold2.pos, fold2.neg,
                  fold3.pos, fold3.neg,
                  fold4.pos, fold4.neg,
                  fold1.pos, fold1.neg)
  
  test5 <- rbind(fold5.pos, fold5.neg)
  
  if(verbose == TRUE) {
    cat(paste('xgcv: finished internal cv\n'))
  }
  
  best.auc <- -Inf
  best.tr <- NA
  best.eta <- NA
  best.iter <- NA
  
  
  for(tr in tree.depth) {
    for(et in eta) {
      
        
        xgb.auc <- list()
        
        if(verbose == TRUE) {
          cat(paste('-----------------------\n'))
          cat(paste('xgcv: depth:', tr,'\n'))
          cat(paste('xgcv: eta:', et,'\n'))
          
        }
        
        
        for(f in c(1,2,3,4,5)) {
          #each fold
          if(verbose == TRUE) {
            cat(paste('xgcv: fold:', f,'\n'))
          }
          if(f == 1) {
            train <- train1
            test <- test1
          } else if(f == 2) {
            train <- train2
            test <- test2
          } else if(f == 3) {
            train <- train3
            test <- test3
          } else if(f == 4) {
            train <- train4
            test <- test4
          } else {
            train <- train5
            test <- test5
          }
          train.labels <- train$train.labels
          train.data <- train[,which(!(colnames(train) %in% c('train.labels')))]
          #           print(dim(train))
          #           print(dim(train.data))
          #           print(length(train.labels))
          #           print(colnames(train))
          #           print(colnames(train.data))
          test.labels <- test$train.labels
          test.data <- test[,which(!(colnames(test) %in% c('train.labels')))]
          
          model <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), verbose=0, nrounds=1000,
                           eta=et, max.depth=tr, objective='binary:logistic', nthread=16)
          
          xgb.iter.auc <- vector()
          for(it in iters) {
            if(verbose == TRUE) {
              cat(paste('xgcv: fold:', f,'iters:', it,'\n'))
            }
            resp <- predict(model, as.matrix(test.data), ntreelimit=it)
            roc.model <- roc(as.numeric(test.labels), as.numeric(resp))
            xgb.iter.auc <- c(xgb.iter.auc, roc.model$auc)
          }
          
          xgb.auc[[f]] <- xgb.iter.auc
        } 
        
        xgb.iter.means <- vector()
        for(i in 1:length(iters)) {
          xgb.iter.means[i] <- mean(c(xgb.auc[[1]][i],
                                      xgb.auc[[2]][i],
                                      xgb.auc[[3]][i],
                                      xgb.auc[[4]][i],
                                      xgb.auc[[5]][i]))
        }
        best.run <- max(xgb.iter.means)
        it <- iters[which(xgb.iter.means == best.run)]
        #Return highest mean C-stat
        if(verbose == TRUE) {
          cat(paste('xgcv: prev.auc:', best.auc,' this round:', best.run,'\n'))
          cat(paste('-----------------------\n'))
        }
        
        if(best.run >= best.auc) {
          best.auc <- best.run
          best.tr <- tr
          best.eta <- et
          best.iter <- it
        }
        
      
    }
  }
  
  #Return parameters in a vector
  return(c(best.eta, best.tr, best.iter))
  
}

xgcv_nice_par <- function(train.data, train.labels, eta, tree.depth, iters, verbose) {
  #print(tree.depth)
  #print(eta)
  #print(iters)
  #run internal cross validation to determine the best via a grid search
  #expects a vector for tree.depth, eta, and iters
  if(verbose == TRUE) {
    cat(paste('xgcv: setting up internal cv\n'))
  }
  data <- cbind(train.data, train.labels)
  
  #5-fold CV?
  positives <- which(data$train.labels == 1)
  negatives <- which(data$train.labels == 0)
  
  data.positives <- data[positives,]
  data.negatives <- data[negatives,]
  
  numpos <- round(0.2*length(positives))
  numneg <- round(0.2*length(negatives))
  
  cv.fold1.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold1.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold1.pos <- data.positives[cv.fold1.idx.pos,]
  fold1.neg <- data.negatives[cv.fold1.idx.neg,]
  
  data.positives <- data.positives[-cv.fold1.idx.pos,]
  data.negatives <- data.negatives[-cv.fold1.idx.neg,]
  
  cv.fold2.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold2.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold2.pos <- data.positives[cv.fold2.idx.pos,]
  fold2.neg <- data.negatives[cv.fold2.idx.neg,]
  
  data.positives <- data.positives[-cv.fold2.idx.pos,]
  data.negatives <- data.negatives[-cv.fold2.idx.neg,]
  
  cv.fold3.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold3.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold3.pos <- data.positives[cv.fold3.idx.pos,]
  fold3.neg <- data.negatives[cv.fold3.idx.neg,]
  
  data.positives <- data.positives[-cv.fold3.idx.pos,]
  data.negatives <- data.negatives[-cv.fold3.idx.neg,]
  
  cv.fold4.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
  cv.fold4.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))
  
  fold4.pos <- data.positives[cv.fold4.idx.pos,]
  fold4.neg <- data.negatives[cv.fold4.idx.neg,]
  
  data.positives <- data.positives[-cv.fold4.idx.pos,]
  data.negatives <- data.negatives[-cv.fold4.idx.neg,]
  
  fold5.pos <- data.positives
  fold5.neg <- data.negatives
  
  train1 <- rbind(fold2.pos, fold2.neg,
                  fold3.pos, fold3.neg,
                  fold4.pos, fold4.neg,
                  fold5.pos, fold5.neg)
  
  test1 <- rbind(fold1.pos, fold1.neg)
  
  train2 <- rbind(fold1.pos, fold1.neg,
                  fold3.pos, fold3.neg,
                  fold4.pos, fold4.neg,
                  fold5.pos, fold5.neg)
  
  test2 <- rbind(fold2.pos, fold2.neg)
  
  train3 <- rbind(fold2.pos, fold2.neg,
                  fold1.pos, fold1.neg,
                  fold4.pos, fold4.neg,
                  fold5.pos, fold5.neg)
  
  test3 <- rbind(fold3.pos, fold3.neg)
  
  train4 <- rbind(fold2.pos, fold2.neg,
                  fold3.pos, fold3.neg,
                  fold1.pos, fold1.neg,
                  fold5.pos, fold5.neg)
  
  test4 <- rbind(fold4.pos, fold4.neg)
  
  train5 <- rbind(fold2.pos, fold2.neg,
                  fold3.pos, fold3.neg,
                  fold4.pos, fold4.neg,
                  fold1.pos, fold1.neg)
  
  test5 <- rbind(fold5.pos, fold5.neg)
  
  if(verbose == TRUE) {
    cat(paste('xgcv: finished internal cv\n'))
  }
  
  best.auc <- -Inf
  best.tr <- NA
  best.eta <- NA
  best.iter <- NA
  
  list.options <- list()
  i <- 1
  for(dp in tree.depth) {
    for(et in eta) {
      #for(itr in iters) {
        values <- c(dp, et)#, itr)
        list.options[[i]] <- values
        i <- i+1
      #}
    }
  }
  
  registerDoParallel()
  
  opts.values <- foreach(opt=list.options, .combine=cbind) %dopar% {
    
  
  
  
#   for(tr in tree.depth) {
#     for(et in eta) {
      
      
      #xgb.auc <- list()
      
      if(verbose == TRUE) {
        cat(paste('-----------------------\n'))
        cat(paste('xgcv: depth:', tr,'\n'))
        cat(paste('xgcv: eta:', et,'\n'))
        
      }
      
      
      for(f in c(1,2,3,4,5)) {
        #each fold
        if(verbose == TRUE) {
          cat(paste('xgcv: fold:', f,'\n'))
        }
        if(f == 1) {
          train <- train1
          test <- test1
        } else if(f == 2) {
          train <- train2
          test <- test2
        } else if(f == 3) {
          train <- train3
          test <- test3
        } else if(f == 4) {
          train <- train4
          test <- test4
        } else {
          train <- train5
          test <- test5
        }
        train.labels <- train$train.labels
        train.data <- train[,which(!(colnames(train) %in% c('train.labels')))]
        #           print(dim(train))
        #           print(dim(train.data))
        #           print(length(train.labels))
        #           print(colnames(train))
        #           print(colnames(train.data))
        test.labels <- test$train.labels
        test.data <- test[,which(!(colnames(test) %in% c('train.labels')))]
        
        model <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), verbose=0, nrounds=1000,
                         eta=et, max.depth=tr, objective='binary:logistic', nthread=16)
        
        xgb.iter.auc <- vector()
        for(it in iters) {
          if(verbose == TRUE) {
            cat(paste('xgcv: fold:', f,'iters:', it,'\n'))
          }
          resp <- predict(model, as.matrix(test.data), ntreelimit=it)
          roc.model <- roc(as.numeric(test.labels), as.numeric(resp))
          xgb.iter.auc <- c(xgb.iter.auc, roc.model$auc)
        }
        
        xgb.auc[[f]] <- xgb.iter.auc
      } 
      
      xgb.iter.means <- vector()
      for(i in 1:length(iters)) {
        xgb.iter.means[i] <- mean(c(xgb.auc[[1]][i],
                                    xgb.auc[[2]][i],
                                    xgb.auc[[3]][i],
                                    xgb.auc[[4]][i],
                                    xgb.auc[[5]][i]))
      }
      best.run <- max(xgb.iter.means)
      it <- iters[which(xgb.iter.means == best.run)]
      #Return highest mean C-stat
      if(verbose == TRUE) {
        cat(paste('xgcv: prev.auc:', best.auc,' this round:', best.run,'\n'))
        cat(paste('-----------------------\n'))
      }
      
      if(best.run >= best.auc) {
        best.auc <- best.run
        best.tr <- tr
        best.eta <- et
        best.iter <- it
      }
      
      
    }
  }
  
  #Return parameters in a vector
  return(c(best.eta, best.tr, best.iter))
  
}

