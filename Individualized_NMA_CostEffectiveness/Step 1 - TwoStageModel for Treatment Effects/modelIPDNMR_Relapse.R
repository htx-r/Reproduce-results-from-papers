############################ IPD NMR Bayesian Model ###########################3

modelIPDNMR_Relapse<-function(){
  ###likelihood
  for (i in 1:Np){
    outcome[i]~dbern(p[i])
    ###formula
    logit(p[i])<-u[studyid[i]] + d[studyid[i], arm[i]] + g[studyid[i]]*(Risk[i]-mean_cov) + g.w[studyid[i],arm[i]]*(Risk[i]-mean_cov)
  }

  ## g fixed across studies

  for (i in 1:Nstudies){
    g[i]<-gamma
  }

  #####treatment effect - fixed across studies and correction for multi-arm studies
  for(i in 1:Nstudies){
    d[i,1] <- 0
    w[i,1] <- 0
    g.w[i,1]<-0

    for(k in 2:na[i]){
      d[i,k]<-md[i, k]
      md[i, k] <- mean[i, k] + sw[i, k]
      w[i, k] <- (d[i, k] - mean[i, k])
      sw[i, k] <- sum(w[i, 1:(k - 1)])/(k - 1)
      mean[i, k] <- delta[treat[i, k]] - delta[treat[i, 1]]

      g.w[i,k]<-gamma.w[treat[i,k]]-gamma.w[treat[i,1]]

    }
  }


  ###priors

  gamma~dnorm(0,0.001)

  ##independent ui for each study
  for (i in 1:Nstudies){
    u[i]~dnorm(0,0.001)
  }

  delta[ref] <- 0 # treatment effect is zero for reference treatment = PLACEBO
  gamma.w[ref] <- 0

  for (k in 1:(ref-1)){
    delta[k] ~ dnorm(0, 0.001)
    gamma.w[k] ~ dnorm(0, 0.001)

  }
  for (k in (ref+1):nt){
    delta[k] ~ dnorm(0, 0.001)
    gamma.w[k] ~ dnorm(0, 0.001)


  }

  ###odds for placebo arm via the placebo arm dataset
  for (i in 1:NpPlacebo){
    outcomeP[i]~dbern(pplacebo[i])
    ###formula
    logit(pplacebo[i])<-logit_Placebo[studyidP[i]]+g0_Placebo[studyidP[i]]*(Risk_p[i]-mean_cov)
 }
  for (j in 1:NPstudies){
    logit_Placebo[j]<-logitPlacebo
    g0_Placebo[j]<-g0Placebo}

  #prior for logitpplacebo
  logitPlacebo~dnorm(0,0.001)
  g0Placebo~dnorm(0,0.001)

  for(j in 1:nt){ORref[j]<- exp(delta[j] - delta[ref])}

  ##### calculation of predicted risk to new patients

  for (i in 1:Nnew){
    for(j in 1:nt){
      logitp[i,j]<-logitPlacebo+ delta[j]+g0Placebo*(logitRisknew[i,1]-mean_cov)+ gamma.w[j]*(logitRisknew[i,1]-mean_cov)
      prob[i,j]<-exp(logitp[i,j])/(1+exp(logitp[i,j]))
      annualized[i,j]<--log(1-prob[i,j])/2

       }
  }
  for(j in 1:nt){
    logit_probability[j]<-logitPlacebo+ delta[j]
    probability[j]<-exp(logit_probability[j])/(1+exp(logit_probability[j]))
    annualized_rate[j]<--log(1- probability[j])/2
  }

  for (i in 1:Np){
    logit_probability_internal[i]<-logitPlacebo+ delta[treatment[i]]+ g0Placebo*(logitRisk_internal[i]-mean_cov_internal)+ gamma.w[treatment[i]]*(logitRisk_internal[i]-mean_cov_internal)
    prob_internal[i]<-exp(logit_probability_internal[i])/(1+exp(logit_probability_internal[i]))
    annualized_internal[i]<--log(1-prob_internal[i])/2

  }



  #for (i in 1:99){
  # for(j in 1:nt){
  #  p[i,j]<-exp(logitp[i,j])/(1+exp(logitp[i,j]))
  #}
}
