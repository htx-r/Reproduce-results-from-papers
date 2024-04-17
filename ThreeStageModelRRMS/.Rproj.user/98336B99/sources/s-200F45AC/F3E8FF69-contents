#method 2
#Updating of the intercept α only

results_model2 <- matrix(nrow = 500,ncol = 6) # table for the final results of specified lrm model
samples=500
set.seed(231398)
for (i in 1:samples) {
  samp_index <- sample(1:nrow(RiskData), nrow(RiskData), rep=TRUE) # create a sampling index vector
  bs_samp <- RiskData[samp_index,] # index the orignal dataset using the sampling vector to give the bs sample
  lp2 <- lrm.fit (y=RiskData$outcome, offset=RiskData$logitp)
  RiskData$logitp2<-RiskData$logitp+lp2$linear.predictors
  # calculate the apparent performance of the bootstrap model in the bs sample
  app_cstat_model2 <- roc(outcome~logitp2,data=bs_samp)
  results_model2[i,1] <- as.numeric(app_cstat_model2$auc)
  app_citl_model2 <-  glm(outcome~logitp2,family="binomial",data=bs_samp)#c-intercept=-0.09, c-slope=0.84
  results_model2[i,2] <- summary(app_citl_model2)$coefficients[1]
  results_model2[i,3] <- summary(app_citl_model2)$coefficients[2]

# calculate the test performance of the bootstrap model in the original sample
  test_cstat_model2 <- roc(outcome~logitp2,data=RiskData)
  results_model2[i,4] <- as.numeric(test_cstat_model2$auc)
  test_citl_model2 <-  glm(outcome~logitp2,family="binomial",data=RiskData) #c-intercept=-0.09, c-slope=0.84
  results_model2[i,5] <- summary(test_citl_model2)$coefficients[1]
  results_model2[i,6] <- summary(test_citl_model2)$coefficients[2]
}


results_model2 <- as.data.frame(results_model2)
colnames(results_model2) <- c("app_c_stat","app_citl","app_c_slope","test_c_stat","test_citl","test_c_slope")

lp2 <- lrm.fit (y=RiskData$outcome, offset=RiskData$logitp)
RiskData$logitp2<-RiskData$logitp+lp2$linear.predictors
app_cstat_model2<-roc(outcome~logitp2,data=RiskData)
apparent_dis_model2<-as.numeric(app_cstat_model2$auc) # apparent c-index of specified model
C_index_correctedmodel2<-apparent_dis_model2 - (mean(results_model2$app_c_stat)-mean(results_model2$test_c_stat)) # c-index optimism corrected
app_cslope_model2 <- glm(outcome~logitp2,family="binomial",data=RiskData) #c-intercept=-0.09, c-slope=0.84
apparent_cal_model2<- summary(app_cslope_model2)$coefficients[2] #apparent calibration-slope of specified model
Calibration_slope_correctedmodel2<-apparent_cal_model2- (mean(results_model2$app_c_slope)-mean(results_model2$test_c_slope)) # c-slope optimism corrected
apparent_cinter_model2<- summary(app_cslope_model2)$coefficients[1] #apparent calibration-slope of specified model
Calibration_inter_correctedmodel2<-apparent_cinter_model2- (mean(results_model2$app_citl)-mean(results_model2$test_citl)) # c-slope optimism corrected
##Results for model2
print(c(C_index_correctedmodel2,Calibration_slope_correctedmodel2,Calibration_inter_correctedmodel2))


#method3 - update of the intercept and the slope
results_model3 <- matrix(nrow = 500,ncol = 6) # table for the final results of specified lrm model
samples=500
set.seed(231398)
for (i in 1:samples) {
  samp_index <- sample(1:nrow(RiskData), nrow(RiskData), rep=TRUE) # create a sampling index vector
  bs_samp <- RiskData[samp_index,] # index the orignal dataset using the sampling vector to give the bs sample
  lp3 <- lrm.fit (y=RiskData$outcome, x=RiskData$logitp)
  RiskData$logitp3<-coef(lp3)[1]+coef(lp3)[2]*RiskData$logitp
  # calculate the apparent performance of the bootstrap model in the bs sample
  app_cstat_model3 <- roc(outcome~logitp3,data=bs_samp)
  results_model3[i,1] <- as.numeric(app_cstat_model3$auc)
  app_citl_model3 <-  glm(outcome~logitp3,family="binomial",data=bs_samp)#c-intercept=-0.09, c-slope=0.84
  results_model3[i,2] <- summary(app_citl_model3)$coefficients[1]
  results_model3[i,3] <- summary(app_citl_model3)$coefficients[2]

  # calculate the test performance of the bootstrap model in the original sample
  test_cstat_model3 <- roc(outcome~logitp3,data=RiskData)
  results_model3[i,4] <- as.numeric(test_cstat_model3$auc)
  test_citl_model3 <-  glm(outcome~logitp3,family="binomial",data=RiskData) #c-intercept=-0.09, c-slope=0.84
  results_model3[i,5] <- summary(test_citl_model3)$coefficients[1]
  results_model3[i,6] <- summary(test_citl_model3)$coefficients[2]
}


results_model3 <- as.data.frame(results_model3)
colnames(results_model3) <- c("app_c_stat","app_citl","app_c_slope","test_c_stat","test_citl","test_c_slope")

lp3 <- lrm.fit (y=RiskData$outcome, x=RiskData$logitp)
RiskData$logitp3<-coef(lp3)[1]+coef(lp3)[2]*RiskData$logitp
app_cstat_model3<-roc(outcome~logitp3,data=RiskData)
apparent_dis_model3<-as.numeric(app_cstat_model3$auc) # apparent c-index of specified model
C_index_correctedmodel3<-apparent_dis_model3 - (mean(results_model3$app_c_stat)-mean(results_model3$test_c_stat)) # c-index optimism corrected
app_cslope_model3 <- glm(outcome~logitp3,family="binomial",data=RiskData) #c-intercept=-0.09, c-slope=0.84
apparent_cal_model3<- summary(app_cslope_model3)$coefficients[2] #apparent calibration-slope of specified model
Calibration_slope_correctedmodel3<-apparent_cal_model3- (mean(results_model3$app_c_slope)-mean(results_model3$test_c_slope)) # c-slope optimism corrected
apparent_cinter_model3<- summary(app_cslope_model3)$coefficients[1] #apparent calibration-slope of specified model
Calibration_inter_correctedmodel3<-apparent_cinter_model3- (mean(results_model3$app_citl)-mean(results_model3$test_citl)) # c-slope optimism corrected
##Results for model3
print(c(C_index_correctedmodel3,Calibration_slope_correctedmodel3,Calibration_inter_correctedmodel3))



#method 4 - recalibration and selective re-estimation
results_model4 <- matrix(nrow = 500,ncol = 6) # table for the final results of specified lrm model
samples=500
set.seed(231398)
for (i in 1:samples) {
  samp_index <- sample(1:nrow(RiskData), nrow(RiskData), rep=TRUE) # create a sampling index vector
  bs_samp <- RiskData[samp_index,] # index the orignal dataset using the sampling vector to give the bs sample
  full <- lrm(outcome~age+disease.duration+edss+nr.Gd.enhanced.lesions+nr.relapses.2y.prior.study1+nr.relapses.2y.prior.study2+months.since.last.relapse+treatment.naive.prior.visit+gender,data=RiskData, x=T, y=T)
  lp3 <- lrm.fit (y=RiskData$outcome, x=RiskData$logitp)
  fit4 <- lrm.fit (y=full$y, x=cbind ( lp3$linear.predictors,full$x[,3],full$x[,4],full$x[,8]))
  RiskData$logitp4<-coef(fit4)[1]+coef(fit4)[2]*lp3$linear.predictors+coef(fit4)[3]*RiskData$edss+coef(fit4)[4]*RiskData$nr.Gd.enhanced.lesions+coef(fit4)[5]*RiskData$treatment.naive.prior.visit
  # calculate the apparent performance of the bootstrap model in the bs sample
  app_cstat_model4 <- roc(outcome~logitp4,data=bs_samp)
  results_model4[i,1] <- as.numeric(app_cstat_model4$auc)
  app_citl_model4 <-  glm(outcome~logitp4,family="binomial",data=bs_samp)#c-intercept=-0.09, c-slope=0.84
  results_model4[i,2] <- summary(app_citl_model4)$coefficients[1]
  results_model4[i,3] <- summary(app_citl_model4)$coefficients[2]

  # calculate the test performance of the bootstrap model in the original sample
  test_cstat_model4 <- roc(outcome~logitp4,data=RiskData)
  results_model4[i,4] <- as.numeric(test_cstat_model4$auc)
  test_citl_model4 <-  glm(outcome~logitp4,family="binomial",data=RiskData) #c-intercept=-0.09, c-slope=0.84
  results_model4[i,5] <- summary(test_citl_model4)$coefficients[1]
  results_model4[i,6] <- summary(test_citl_model4)$coefficients[2]
}


results_model4 <- as.data.frame(results_model4)
colnames(results_model4) <- c("app_c_stat","app_citl","app_c_slope","test_c_stat","test_citl","test_c_slope")


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
app_cstat_model4<-roc(outcome~logitp4,data=RiskData)
apparent_dis_model4<-as.numeric(app_cstat_model4$auc) # apparent c-index of specified model
C_index_correctedmodel4<-apparent_dis_model4 - (mean(results_model4$app_c_stat)-mean(results_model4$test_c_stat)) # c-index optimism corrected
app_cslope_model4 <- glm(outcome~logitp4,family="binomial",data=RiskData) #c-intercept=-0.09, c-slope=0.84
apparent_cal_model4<- summary(app_cslope_model4)$coefficients[2] #apparent calibration-slope of specified model
Calibration_slope_correctedmodel4<-apparent_cal_model4- (mean(results_model4$app_c_slope)-mean(results_model4$test_c_slope)) # c-slope optimism corrected
apparent_cinter_model4<- summary(app_cslope_model4)$coefficients[1] #apparent calibration-slope of specified model
Calibration_inter_correctedmodel4<-apparent_cinter_model4- (mean(results_model4$app_citl)-mean(results_model4$test_citl)) # c-slope optimism corrected
##Results for model4
print(c(C_index_correctedmodel4,Calibration_slope_correctedmodel4,Calibration_inter_correctedmodel4))
