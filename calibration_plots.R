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
load("BleedingModel_CalibrationData_26OCT2018.RData")
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
