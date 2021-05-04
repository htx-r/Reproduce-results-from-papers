############################################################################################
###############  Function that returns the optimism for the AUC  ###########################
################ via bootstrap internal validation (500 samples) ###################
#############################################################################

BootstrapValidation_PrModel.fun <- function(data, samples){
results <- matrix(nrow = samples,ncol = 6) # table for the final results of  model
results<-as.data.frame(results)
set.seed(231398)
for (i in 1:samples) {
  samp_index <- sample(1:nrow(data), nrow(data), rep=TRUE) # create a sampling index vector

  bs_samp <- data[samp_index,] # index the original dataset using the sampling vector to give the bs sample
  bs_samp$age<-bs_samp $age-mean(bs_samp $age)
  bs_samp$disease.duration<-bs_samp $disease.duration-mean(bs_samp $disease.duration)
  bs_samp$edss<-bs_samp $edss-mean(bs_samp $edss)
  bs_samp$months.since.last.relapse<-bs_samp $months.since.last.relapse-mean(bs_samp $months.since.last.relapse)

  glm_out_samp<-glm((outcome)~ age + disease.duration + edss + nr.Gd.enhanced.lesions + nr.relapses.2y.prior.study1 + nr.relapses.2y.prior.study2 +
                 months.since.last.relapse + treatment.naive.prior.visit + gender + treatment.during.cycle,
               family = "binomial",data=bs_samp)

  lp_bs<-predict(glm_out_samp) # predict lp from the bootstrap model in the bs sample
  pr_bs<-exp(lp_bs)/(1+exp(lp_bs))  # predict probabilities from the bootstrap model in the bs sample


  lp_test<-predict(glm_out_samp, newdata = data) # predict lp from the bootstrap model in the original sample
  pr_test<-exp(lp_test)/(1+exp(lp_test)) # predict probabilities from the bootstrap model in the original sample

  # calculate the apparent performance of the bootstrap model in the bs sample
  app_cstat_model<- roc(outcome~pr_bs,data=bs_samp)
  results[i,1] <- as.numeric(app_cstat_model$auc)
  app_citl_model<- glm(outcome ~ offset(lp_bs),family=binomial, data=bs_samp)
  results[i,2] <- summary(app_citl_model)$coefficients[1,1]
  app_cslope_model <- glm(outcome ~ lp_bs,family=binomial(link='logit'), data=bs_samp)
  results[i,3] <- summary(app_cslope_model)$coefficients[2,1]

  # calculate the test performance of the bootstrap model in the original sample
  test_cstat_model <- roc(outcome~pr_test,data=data)
  results[i,4] <- test_cstat_model$auc
  test_citl_model <- glm(outcome ~ offset(lp_test),family=binomial, data=data)
  results[i,5] <- summary(test_citl_model)$coefficients[1,1]
  test_cslope_model<- glm(outcome ~ lp_test,family=binomial, data=data)
  results[i,6] <- summary(test_cslope_model)$coefficients[2,1]

}

results2<- as.data.frame(results) #tables of results
colnames(results2) <- c("app_c_stat","app_citl","app_c_slope","test_c_stat","test_citl","test_c_slope")

optimism_auc<-mean(results2$app_c_stat)-mean(results2$test_c_stat)
optimism_cslope<-mean(results2$app_c_slope)-mean(results2$test_c_slope)


return(optimism_auc)
}



