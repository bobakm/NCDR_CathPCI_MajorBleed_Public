load("/data/Projects/ACC_NCDR/NCDR/BJMDATA/CALIBRATION_DATA/ncdr_calibration/BleedingModel_CalibrationData_26OCT2018.RData")

#For Decile-based calibration
fold1.data <- calib.df[which(calib.df$`Fold Index` == 1),]
fold2.data <- calib.df[which(calib.df$`Fold Index` == 2),]
fold3.data <- calib.df[which(calib.df$`Fold Index` == 3),]
fold4.data <- calib.df[which(calib.df$`Fold Index` == 4),]
fold5.data <- calib.df[which(calib.df$`Fold Index` == 5),]

source('calcDeciles.R')
list.rao <- list()
list.blended <- list()
for(f in 1:5) {
  if(f == 1) {
    fold.data <- fold1.data
  } else if(f == 2) {
    fold.data <- fold2.data
  } else if(f == 3) {
    fold.data <- fold3.data
  } else if(f == 4) {
    fold.data <- fold4.data
  } else {
    fold.data <- fold5.data
  }
  
  
  rao.deciles <- calcDeciles(fold.data$Rao_Model_Probability,fold.data$Label)
  blended.deciles <- calcDeciles(fold.data$`Blended_Model_Probability (Ours)`, fold.data$Label)
  
  list.rao[[f]] <- rao.deciles
  list.blended[[f]] <- blended.deciles
}



rao.predicted <- cbind(list.rao[[1]][[1]]$`predicted rates`,list.rao[[2]][[1]]$`predicted rates`,list.rao[[3]][[1]]$`predicted rates`,
                       list.rao[[4]][[1]]$`predicted rates`,list.rao[[5]][[1]]$`predicted rates`)/100

rao.observed <- cbind(list.rao[[1]][[1]]$`observed rates`,list.rao[[2]][[1]]$`observed rates`,list.rao[[3]][[1]]$`observed rates`,
                      list.rao[[4]][[1]]$`observed rates`,list.rao[[5]][[1]]$`observed rates`)/100

blended.predicted <- cbind(list.blended[[1]][[1]]$`predicted rates`,list.blended[[2]][[1]]$`predicted rates`,list.blended[[3]][[1]]$`predicted rates`,
                           list.blended[[4]][[1]]$`predicted rates`,list.blended[[5]][[1]]$`predicted rates`)/100

blended.observed <- cbind(list.blended[[1]][[1]]$`observed rates`, list.blended[[2]][[1]]$`observed rates`, list.blended[[3]][[1]]$`observed rates`, 
                          list.blended[[4]][[1]]$`observed rates`, list.blended[[5]][[1]]$`observed rates`)/100


rao.decile.predicted.sd <- cbind(list.rao[[1]][[1]]$`predicted sd`,list.rao[[2]][[1]]$`predicted sd`,list.rao[[3]][[1]]$`predicted sd`,
                          list.rao[[4]][[1]]$`predicted sd`,list.rao[[5]][[1]]$`predicted sd`)
blended.decile.predicted.sd <- cbind(list.blended[[1]][[1]]$`predicted sd`,list.blended[[2]][[1]]$`predicted sd`,list.blended[[3]][[1]]$`predicted sd`,
                              list.blended[[4]][[1]]$`predicted sd`,list.blended[[5]][[1]]$`predicted sd`)

rao.predicted.mean <- rowMeans(rao.predicted)
rao.predicted.sd <- apply(rao.predicted, 1, sd)#rowMeans(rao.decile.predicted.sd)#apply(rao.predicted, 1, sd)

rao.observed.mean <- rowMeans(rao.observed)
rao.observed.sd <- apply(rao.observed, 1, sd)


blended.predicted.mean <- rowMeans(blended.predicted)
blended.predicted.sd <- apply(blended.predicted, 1, sd)#rowMeans(blended.decile.predicted.sd)#apply(blended.predicted, 1, sd)

blended.observed.mean <- rowMeans(blended.observed)
blended.observed.sd <- apply(blended.observed, 1, sd)

rao.predicted.se <- rao.predicted.sd/(sqrt(5))
rao.observed.se <- rao.observed.sd/(sqrt(5))
blended.predicted.se <- blended.predicted.sd/(sqrt(5))
blended.observed.se <- blended.observed.sd/(sqrt(5))
# 
# se <- function(d) {
#   return(sqrt(var(d)/length(d)))
# }
### Now Plot them
library(ggplot2)
calibration.deciles <- data.frame(rao.predicted.mean, rao.observed.mean,
                                  rao.predicted.sd, rao.observed.sd,
                                  blended.predicted.mean, blended.observed.mean,
                                  blended.predicted.sd, blended.observed.sd,
                                  rao.predicted.se, rao.observed.se,
                                  blended.predicted.se, blended.observed.se)

ideal.calibration.predicted <- c(0, 0.25)
ideal.calibration.observed <- c(0, 0.25)


predicted.rate <- c(rao.predicted.mean, blended.predicted.mean)
observed.rate <- c(rao.observed.mean, blended.observed.mean)
predicted.sd <- c(rao.predicted.sd, blended.predicted.sd)
predicted.se <- c(rao.predicted.se, blended.predicted.se)
observed.sd <- c(rao.observed.sd, blended.observed.sd)
observed.se <- c(rao.observed.se, blended.observed.se)
predicted.upper.sd <- predicted.rate + predicted.sd
predicted.lower.sd <- predicted.rate - predicted.sd
predicted.upper.se <- predicted.rate + predicted.se
predicted.lower.se <- predicted.rate - predicted.se
observed.upper.sd <- observed.rate + observed.sd
observed.lower.sd <- observed.rate - observed.sd
observed.upper.se <- observed.rate + observed.se
observed.lower.se <- observed.rate - observed.se


groups <- c(rep('Existing Full Model', length(rao.predicted.mean)), rep('Blended Model', length(blended.predicted.mean)))


calibration.deciles.plot <- data.frame(predicted.rate, observed.rate, groups, predicted.sd, observed.sd, predicted.se, observed.se,
                                       predicted.upper.sd, predicted.lower.sd, predicted.upper.se, predicted.lower.se,
                                       observed.upper.sd, observed.lower.sd, observed.upper.se, observed.lower.se)

g <- ggplot(calibration.deciles.plot, aes(x =predicted.rate, y=observed.rate, color=groups)) + 
  theme(panel.background=element_rect(fill='white', color='black'), legend.position = c(0.2,0.9), legend.text = element_text(size=12),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12), legend.title = element_blank()) +
  geom_point() + scale_color_manual(breaks = c('Existing Full Model', 'Blended Model'), values = c('blue', 'red')) + 
  geom_line() + xlab('Predicted Rate') + ylab('Observed Rate') + scale_x_continuous(limits=c(0, 0.25)) + scale_y_continuous(limits=c(0, 0.25))
  #geom_errorbar(aes(ymin=observed.lower.sd, ymax=observed.upper.sd),width=0.015, size=0.3) + geom_errorbarh(aes(xmin=predicted.lower.sd, xmax=predicted.upper.sd), size = 0.3, height=0.01)
g  
ggsave(g, file='./fig1a_noerrorbar_calib_deciles.eps', device='eps')
#####################
## Continuous Calibration
#####################

require(mgcv)
require(sandwich)
###################
# calibx=seq(0,1,by=0.01)# predictions evaluated 
# caliby.rao=matrix(NA,5,length(calibx)) # observed rates for 5 folds
# caliby.blend=caliby.rao
# 
# for (i in 1:5){
#   select=which(calib.df$`Fold Index`==i)
#   myd=data.frame(x1=calib.df$Rao_Model_Probability[select],x2=calib.df$`Blended_Model_Probability (Ours)`[select],
#                  y=calib.df$Label[select])
#   fit.calib.rao=gam(y~s(x1,bs="cs"),method="REML",data=myd,qr=T)
#   caliby.rao[i,]=predict(fit.calib.rao,newdata=data.frame(x1=calibx))
#   fit.calib.blend=gam(y~s(x2,bs="cs"),method="REML",data=myd,qr=T)
#   caliby.blend[i,]=predict(fit.calib.blend,newdata=data.frame(x2=calibx))
# }
############################
#load("BleedingModel_CalibrationData_26OCT2018.RData")
myd=data.frame(x1=calib.df$Rao_Model_Probability,x2=calib.df$`Blended_Model_Probability (Ours)`,
               y=calib.df$Label)
fit.calib.rao=gam(y~s(x1,bs="cr"),method="ML",data=myd,qr=T)
fit.calib.blend=gam(y~s(x2,bs="cr"),method="ML",data=myd,qr=T)
vcov.rao=vcovHC(fit.calib.rao,type="HC0")
vcov.blend=vcovHC(fit.calib.blend,type="HC0")
fit.calib.rao$Vp=vcov.rao
fit.calib.blend$Vp=vcov.blend
newdata=data.frame(x1=seq(0,1,by=0.01),x2=seq(0,1,by=0.01))
pred.calib.rao=predict(fit.calib.rao,newdata=newdata,se.fit = T)
pred.calib.blend=predict(fit.calib.blend,newdata=newdata,se.fit = T)

##Now need to make a plotting Object
pred.vals <- c(pred.calib.rao$fit, pred.calib.blend$fit)
seq.vals <- rep(seq(0,1,.01),2)
groups.vals <- c(rep('Existing Full Model\n(-- 95% CI)', length(pred.calib.rao$fit)), rep('Blended Model\n(-- 95% CI)', length(pred.calib.blend$fit)))
groups.se.vals <- c(rep('Existing Full Model Confidence Interval', length(pred.calib.rao$fit)), rep('Blended Model Confidence Interval', length(pred.calib.blend$fit)))

pred.se <- c(pred.calib.rao$se.fit, pred.calib.blend$se.fit)
lower.se <- pred.vals - pred.se
upper.se <- pred.vals + pred.se

continuous.calib.plot <- data.frame(pred.vals, seq.vals, groups.vals, lower.se, upper.se, groups.se.vals)
ci.calib.plot <- data.frame(lower.se[1:101], lower.se[102:202], upper.se[1:101], upper.se[102:202], seq(0,1, .01))
colnames(ci.calib.plot) <- c('lower.rao.se', 'lower.blended.se', 'upper.rao.se' ,'upper.blended.se', 'seq')
##colnames(ci.calib.plot) <- c('Existing Full Model Confidence Interval', 'Blended Model Confidence Interval', 'Upper Rao', 'Upper Blended', 'Sequence')
p <- ggplot(continuous.calib.plot, aes(x =seq.vals, y=pred.vals, color=groups.vals)) + 
  theme(panel.background=element_rect(fill='white', color='black'), legend.position = c(0.2,0.9), legend.text = element_text(size=12),
        axis.text.x = element_text(size=12), axis.text.y = element_text(size=12), legend.title = element_blank()) +
  scale_color_manual(breaks = c('Existing Full Model\n(-- 95% CI)', 'Blended Model\n(-- 95% CI)'), values = c('blue', 'red')) + scale_y_continuous(limits=c(0, 1.0))+
  geom_line() + xlab('Predicted Rate') + ylab('Observed Rate') +
  geom_line(aes(y=lower.se), linetype=2) + geom_line(aes(y=upper.se), linetype=2) 
p  
ggsave(p, file='./fig1b_cont_calib.eps', device='eps')
