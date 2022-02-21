################################################################################################################
###################################     SCRIPT THAT CHECKS 4 DIFFERENT METHODS OD RECALIBRATION  ###############
###################################### AND WE CHOOSE THE ONE WITH THE HIGHEST AUC   ###########################
##############################################################################################################

RiskData<-RCTs
mean(RiskData$relapse.2y.after.study)
mean(RiskData$Risk)

#method1
#the original with no changes - use logitp and Risk
app_cstat_model <- roc(outcome~logitp,data=RiskData)
app_cstat_model$auc #0.5772

lp1<-RiskData$logitp
#method 2
#Updating of the intercept α only

lp2 <- lrm.fit (y=RiskData$outcome, offset=RiskData$logitp)
RiskData$logitp2<-RiskData$logitp+lp2$linear.predictors
app_cstat_model2 <- roc(outcome~logitp2,data=RiskData)
app_cstat_model2$auc #0.5763
glm(outcome~logitp2,family="binomial",data=RiskData) #c-intercept=-0.09, c-slope=0.84

#method3 - update of the intercept and the slope
lp3 <- lrm.fit (y=RiskData$outcome, x=RiskData$logitp)
RiskData$logitp3<-coef(lp3)[1]+coef(lp3)[2]*RiskData$logitp
app_cstat_model3 <- roc(outcome~logitp3,data=RiskData)
app_cstat_model3$auc #0.5763
glm(outcome~logitp3,family="binomial",data=RiskData) #c-intercept=0.0, c-slope=1

full <- lrm(outcome~age+disease.duration+edss+nr.Gd.enhanced.lesions+nr.relapses.2y.prior.study1+nr.relapses.2y.prior.study2+months.since.last.relapse+treatment.naive.prior.visit+gender,data=RiskData, x=T, y=T)
#method 4 - recalibration and selective re-estimation
for (i in 1:9) {
  fit4 <- lrm.fit (y=full$y, x=cbind (full$x[,i],  lp1))
  print(fit4)
} # some printing of results of fit4
#1,3,4,8
fit4 <- lrm.fit (y=full$y, x=cbind ( lp3$linear.predictors,full$x[,3],full$x[,4],full$x[,8]))
lp4<-coef(fit4)[1]+coef(fit4)[2]*lp3$linear.predictors+coef(fit4)[3]*RiskData$edss+coef(fit4)[4]*RiskData$nr.Gd.enhanced.lesions+coef(fit4)[5]*RiskData$treatment.naive.prior.visit
RiskData$logitp4<-lp4
app_cstat_model4 <- roc(outcome~logitp4,data=RiskData)
app_cstat_model4$auc #0.61
glm(outcome~logitp4,family="binomial",data=RiskData) #c-intercept=0.0, c-slope=1


RiskData$Risknew4<-exp(RiskData$logitp4)/(1+exp(RiskData$logitp4))

summary(RiskData$Risknew4)

