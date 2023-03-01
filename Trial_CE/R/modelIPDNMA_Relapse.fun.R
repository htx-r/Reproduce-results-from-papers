############################ IPD NMA Bayesian Model ###########################3

modelIPDNMA_Relapse<-function(){
  ###likelihood
  for (i in 1:Np){
    outcome[i]~dbern(p[i])
    ###formula
    logit(p[i])<-u[studyid[i]] + d[studyid[i], arm[i]]
  }

  ## g fixed across studies


  #####treatment effect - fixed across studies and correction for multi-arm studies
  for(i in 1:Nstudies){
    d[i,1] <- 0
    w[i,1] <- 0

    for(k in 2:na[i]){
      d[i,k]<-md[i, k]
      md[i, k] <- mean[i, k] + sw[i, k]
      w[i, k] <- (d[i, k] - mean[i, k])
      sw[i, k] <- sum(w[i, 1:(k - 1)])/(k - 1)
      mean[i, k] <- delta[treat[i, k]] - delta[treat[i, 1]]

    }
  }


  ###priors


  ##independent ui for each study
  for (i in 1:Nstudies){
    u[i]~dnorm(0,0.001)
  }

  delta[ref] <- 0 # treatment effect is zero for reference treatment = PLACEBO

  for (k in 1:(ref-1)){
    delta[k] ~ dnorm(0, 0.001)

  }
  for (k in (ref+1):nt){
    delta[k] ~ dnorm(0, 0.001)


  }

  ###odds for placebo arm via the placebo arm dataset
  for (i in 1:NpPlacebo){
    outcomeP[i]~dbern(pplacebo[i])
    ###formula
    logit(pplacebo[i])<-logitpplacebo
  }
  #prior for logitpplacebo
  logitpplacebo~dnorm(0,0.001)

  ###odds for placebo arm via the placebo arm dataset
  for (i in 1:NpPlacebo2){
    outcomeP2[i]~dbern(pplacebo2[i])
    ###formula
    logit(pplacebo2[i])<-logitpplacebo2
  }
  #prior for logitpplacebo
  logitpplacebo2~dnorm(0,0.001)

  for(j in 1:nt){ORref[j]<- exp(delta[j] - delta[ref])}

  ##### calculation of predicted risk to new patients


    for(j in 1:nt){
      logitp[j]<-logitpplacebo+ delta[j]
      logitp2[j]<-logitpplacebo2+ delta[j]
  }


  #for (i in 1:99){
  # for(j in 1:nt){
  #  p[i,j]<-exp(logitp[i,j])/(1+exp(logitp[i,j]))
  #}
}
