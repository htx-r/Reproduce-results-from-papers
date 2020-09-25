##############################################################################################################
##################################  Function for the bootstrap validation ######################################
##################### Estimation of optimism corrected measures of discrimination and calibration ################
###################################################################################################################

BootstrapValidationPrModel.fun <- function(data, samples){
  results <- matrix(nrow = samples,ncol = 6) # table for the final results of specified lrm model
  results<-as.data.frame(results)
  set.seed(231398)
  for (j in 1:samples) {
    j
    samp_index <- sample(1:nrow(data), nrow(data), rep=TRUE) # create a sampling index vector

    bs_samp <- data[samp_index,] # index the original dataset using the sampling vector to give the bs sample

    bs_samp$nr.Gd.enhanced.lesions<-as.numeric(bs_samp$nr.Gd.enhanced.lesions)-1
    bs_samp$gender<-as.numeric(bs_samp$gender)
    bs_samp$nr.relapses.2y.prior.study1<-as.numeric(bs_samp$nr.relapses.2y.prior.study1)
    bs_samp$nr.relapses.2y.prior.study2<-as.numeric(bs_samp$nr.relapses.2y.prior.study2)
    bs_samp$treatment.naive.prior.visit<-as.numeric(bs_samp$treatment.naive.prior.visit)
    bs_samp$treatment.during.cycle<-as.numeric(bs_samp$treatment.during.cycle)-1

    #give the data
    jagsdataVal <- list(
      Nobservations=nrow(bs_samp),
      outcome=as.numeric(factor(bs_samp$outcome))-1,
      npf=10,
      npid=length(unique(bs_samp$patient.id)),
      subj=as.integer(factor(bs_samp$patient.id)),
      age=bs_samp$age-mean(bs_samp$age),
      disease.duration=bs_samp$disease.duration-mean(bs_samp$disease.duration),
      edss=bs_samp$edss-mean(bs_samp$edss),
      nr.Gd.enhanced.lesions=as.factor(bs_samp$nr.Gd.enhanced.lesions),
      nr.relapses.2y.prior.study1=as.factor(bs_samp$nr.relapses.2y.prior.study1),
      nr.relapses.2y.prior.study2=as.factor(bs_samp$nr.relapses.2y.prior.study2),
      months.since.last.relapse=bs_samp$months.since.last.relapse-mean(bs_samp$months.since.last.relapse),
      treatment.naive.prior.visit=as.factor(bs_samp$treatment.naive.prior.visit),
      gender=as.factor(bs_samp$gender),
      treatment.during.cycle=as.factor(bs_samp$treatment.during.cycle)
    )
    # run the jugs model
    set.seed(2000)
    bs_sampjagsResults <- jags.parallel(data = jagsdataVal ,inits=NULL,parameters.to.save = c('b','sigma', 'rho' ),model.file = jagsmodelSMSC,
                                        n.chains=2,n.iter = 100000,n.burnin = 1000,n.thin = 10)


    bs_samp_logitp<-NA
    for (i in 1:nrow(bs_samp)){
      bs_samp_logitp[i]=bs_sampjagsResults$BUGSoutput$summary[1,1]+bs_sampjagsResults$BUGSoutput$summary[2,1]*(bs_samp$age[i]-mean(bs_samp$age))+bs_sampjagsResults$BUGSoutput$summary[3,1]*(bs_samp$disease.duration[i]-mean(bs_samp$disease.duration))+bs_sampjagsResults$BUGSoutput$summary[4,1]*(bs_samp$edss[i]-mean(bs_samp$edss))+bs_sampjagsResults$BUGSoutput$summary[5,1]*bs_samp$nr.Gd.enhanced.lesions[i]+bs_sampjagsResults$BUGSoutput$summary[6,1]*bs_samp$nr.relapses.2y.prior.study1[i]+bs_sampjagsResults$BUGSoutput$summary[7,1]*bs_samp$nr.relapses.2y.prior.study2[i]+bs_sampjagsResults$BUGSoutput$summary[8,1]*(bs_samp$months.since.last.relapse[i]-mean(bs_samp$months.since.last.relapse))+bs_sampjagsResults$BUGSoutput$summary[9,1]*bs_samp$treatment.naive.prior.visit[i]+bs_sampjagsResults$BUGSoutput$summary[10,1]*bs_samp$gender[i]+bs_sampjagsResults$BUGSoutput$summary[11,1]*bs_samp$treatment.during.cycle[i]
    }
    bs_samp$logitp<-bs_samp_logitp

    bs_samp$Risk<-exp(bs_samp$logitp)/(1+exp(bs_samp$logitp))

    # calculate the apparent performance of the bootstrap model in the bs sample
    app_cstat_modelsamp <- roc(outcome~Risk,data=bs_samp, ci=TRUE)
    results[j,1] <- as.numeric(app_cstat_modelsamp$auc)
    app_citl_modelsamp<- glm(outcome ~ offset(logitp),family=binomial, data=bs_samp)
    results[j,2] <- summary(app_citl_modelsamp)$coefficients[1,1]
    app_cslope_modelelasticnet <- glm(outcome ~ logitp,family=binomial(link='logit'), data=bs_samp)
    results[j,3] <- summary(app_cslope_modelelasticnet )$coefficients[2,1]

    # calculate the test performance of the bootstrap model in the original sample


    data$nr.Gd.enhanced.lesions<-as.numeric(data$nr.Gd.enhanced.lesions)-1
    data$gender<-as.numeric(data$gender)
    data$nr.relapses.2y.prior.study1<-as.numeric(data$nr.relapses.2y.prior.study1)
    data$nr.relapses.2y.prior.study2<-as.numeric(data$nr.relapses.2y.prior.study2)
    data$treatment.naive.prior.visit<-as.numeric(data$treatment.naive.prior.visit)
    data$treatment.during.cycle<-as.numeric(data$treatment.during.cycle)-1

    data_logitp<-NA
    for (i in 1:nrow(data)){
      data_logitp[i]=bs_sampjagsResults$BUGSoutput$summary[1,1]+bs_sampjagsResults$BUGSoutput$summary[2,1]*(data$age[i]-mean(data$age))+bs_sampjagsResults$BUGSoutput$summary[3,1]*(data$disease.duration[i]-mean(data$disease.duration))+bs_sampjagsResults$BUGSoutput$summary[4,1]*(data$edss[i]-mean(data$edss))+bs_sampjagsResults$BUGSoutput$summary[5,1]*data$nr.Gd.enhanced.lesions[i]+bs_sampjagsResults$BUGSoutput$summary[6,1]*data$nr.relapses.2y.prior.study1[i]+bs_sampjagsResults$BUGSoutput$summary[7,1]*data$nr.relapses.2y.prior.study2[i]+bs_sampjagsResults$BUGSoutput$summary[8,1]*(data$months.since.last.relapse[i]-mean(data$months.since.last.relapse))+bs_sampjagsResults$BUGSoutput$summary[9,1]*data$treatment.naive.prior.visit[i]+bs_sampjagsResults$BUGSoutput$summary[10,1]*data$gender[i]+bs_sampjagsResults$BUGSoutput$summary[11,1]*data$treatment.during.cycle[i]
    }
    data$logitp<-data_logitp

    data$Risk<-exp(data$logitp)/(1+exp(data$logitp))


    test_cstat_modelsamp<- roc(outcome~Risk,data=data)
    results[j,4] <- test_cstat_modelsamp$auc
    test_citl_modelsamp <- glm(outcome ~ offset(logitp),family=binomial, data=data)
    results[j,5] <- summary(test_citl_modelsamp)$coefficients[1,1]
    test_cslope_modelsamp<- glm(outcome ~ logitp,family=binomial, data=data)
    results[j,6] <- summary(test_cslope_modelsamp)$coefficients[2,1]

  }
  colnames(results) <- c("app_c_stat","app_citl","app_c_slope","test_c_stat","test_citl","test_c_slope")

  #####for Elastic net model
  # optimism adjusted statistics

  app_cstat_model <- roc(outcome~Risk,data=SMSCdataC, ci=TRUE)
  apparent_dis<-app_cstat_model$auc #0.67
  C_index_corrected<- apparent_dis - (mean(results$app_c_stat)-mean(results$test_c_stat)) # c-index optimism corrected
  app_cslopeL <- glm(outcome~logitp,family="binomial",data=SMSCdataC,x=TRUE,y=TRUE)
  apparent_cal <- summary(app_cslopeL)$coefficients[2,1] ##apparent c-slope of the original model
  Calibration_slope_corrected<-apparent_cal - (mean(results$app_c_slope)-mean(results$test_c_slope)) # c-slope optimism corrected

  return(list(results,  apparent_dis, apparent_cal,  C_index_corrected, Calibration_slope_corrected ))
}

