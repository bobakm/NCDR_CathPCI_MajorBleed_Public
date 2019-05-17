source('calcDeciles.R')
library(SpecsVerification)
load("/data/Projects/ACC_NCDR/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/labels_v8_7JUN2016.RData")

rao.preds <- list()
rao.preds[[1]] <- list.crossval.results[[1]][[6]]
rao.preds[[2]] <- list.crossval.results[[2]][[6]]
rao.preds[[3]] <- list.crossval.results[[3]][[6]]
rao.preds[[4]] <- list.crossval.results[[4]][[6]]
rao.preds[[5]] <- list.crossval.results[[5]][[6]]

rao.thresholds <- c(list.crossval.results[[1]][[16]][[1]]$v[1],list.crossval.results[[2]][[16]][[1]]$v[1],
                              list.crossval.results[[3]][[16]][[1]]$v[1],list.crossval.results[[4]][[16]][[1]]$v[1],
                              list.crossval.results[[5]][[16]][[1]]$v[1])

rao.probs <- c(rao.preds[[1]],rao.preds[[2]],
               rao.preds[[3]],rao.preds[[4]],
               rao.preds[[5]])

labels.all <- c(test1.labels, test2.labels, test3.labels, test4.labels, test5.labels)

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


blended.preds <- list()
blended.preds[[1]] <- list.crossval.results[[1]][[10]]
blended.preds[[2]] <- list.crossval.results[[2]][[10]]
blended.preds[[3]] <- list.crossval.results[[3]][[10]]
blended.preds[[4]] <- list.crossval.results[[4]][[10]]
blended.preds[[5]] <- list.crossval.results[[5]][[10]]

blended.thresholds <- c(list.crossval.results[[1]][[20]][[1]]$v[1],list.crossval.results[[2]][[20]][[1]]$v[1],
                    list.crossval.results[[3]][[20]][[1]]$v[1],list.crossval.results[[4]][[20]][[1]]$v[1],
                    list.crossval.results[[5]][[20]][[1]]$v[1])

blended.probs <- c(blended.preds[[1]],blended.preds[[2]],
               blended.preds[[3]],blended.preds[[4]],
               blended.preds[[5]])

labels.all <- c(test1.labels, test2.labels, test3.labels, test4.labels, test5.labels)

# calib.results.clinical <- calcDeciles(full.clinical.probs, labels.all)
# calib.results.all <- calcDeciles(full.all.probs, labels.all)
dec.vals.blended <- decPoints(blended.preds, 5)
#dec.vals.all <- decPoints(full.all.preds, 5)

mean.deciles.blended <- vector()

for(idx in 1:length(dec.vals.blended)) {
  mean.deciles.blended[idx] <- mean(dec.vals.blended[[idx]])
}

calib.results.blended <- sortDeciles(blended.probs, labels.all, mean.deciles.blended)
#calib.results.all <- sortDeciles(full.all.probs, labels.all, mean.deciles.all)



save('rao.probs', 'blended.probs', 'rao.thresholds','blended.thresholds',
     'dec.vals.blended', 'mean.deciles.blended',
     'dec.vals.clinical', 'mean.deciles.clinical',
     'calib.results.clinical', 'calib.results.blended',file='Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/BLENDED_model_calibration.RData')


risk.diff <- blended.probs - rao.probs
risk.diff.sorted <- risk.diff[order(risk.diff)]
risk.diff.labels <- labels.all[order(risk.diff)]
risk.diff.labels.factor <- risk.diff.labels
risk.diff.labels.factor[risk.diff.labels.factor == 0] <- 'No Bleed'
risk.diff.labels.factor[risk.diff.labels.factor == 1] <- 'Bleed'
risk.diff.labels.factor <- as.factor(risk.diff.labels.factor)
#Now plot difference
library(ggplot2)
df.risk <- data.frame(risk.diff.sorted, risk.diff.labels.factor, seq(1, length(risk.diff.sorted), 1))
colnames(df.risk) <- c('Risk', 'Labels', 'Index')

p <- ggplot(df.risk)
p <- p + theme_bw() + xlab('Sorted Index of Patients') + ylab('Difference in Calculated Risk')
p <- p + geom_point(aes(x=Index, y=Risk, color=Labels))
p

############# OCT 2018
#All 5 folds labels labels.all
folds <- c(rep(1, length(test1.labels)),
           rep(2, length(test1.labels)),
           rep(3, length(test1.labels)),
           rep(4, length(test1.labels)),
           rep(5, length(test1.labels)))

calib.df <- data.frame(folds, labels.all, rao.probs, blended.probs)
colnames(calib.df) <- c('Fold Index', 'Label', 'Rao_Model_Probability', 'Blended_Model_Probability (Ours)')
save(calib.df, file='/data/Projects/ACC_NCDR/NCDR/BJMDATA/CALIBRATION_DATA/BleedingModel_CalibrationData_26OCT2018.RData')
