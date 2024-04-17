
modelIPDNMR<-function(){
  ###likelihood
  for (i in 1:Np){
    outcome[i]~dbern(p[i])
    ###formula
    logit(p[i])<-u[studyid[i]] + d[studyid[i], arm[i]]+ (g[studyid[i]]+ g.w[studyid[i],arm[i]])*(Risk[i])
    + (g.b[studyid[i],arm[i]]-g.w[studyid[i],arm[i]])*(meanRisk[studyid[i]])
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
    g.b[i,1]<-0
    for(k in 2:na[i]){
      d[i,k]<-md[i, k]
      md[i, k] <- mean[i, k] + sw[i, k]
      w[i, k] <- (d[i, k] - mean[i, k])
      sw[i, k] <- sum(w[i, 1:(k - 1)])/(k - 1)
      mean[i, k] <- delta[treat[i, k]] - delta[treat[i, 1]]
      
      
      g.w[i,k]<-gamma.w[treat[i,k]]-gamma.w[treat[i,1]]
      g.b[i,k]<-gamma.b[treat[i,k]]-gamma.b[treat[i,1]]
      
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
  gamma.b[ref] <- 0
  
  for (k in 1:(ref-1)){
    delta[k] ~ dnorm(0, 0.001)
    gamma.w[k] ~ dnorm(0, 0.001)
    gamma.b[k] <-gamma.w[k]
    
  }
  for (k in (ref+1):nt){
    delta[k] ~ dnorm(0, 0.001)
    gamma.w[k] ~ dnorm(0, 0.001)
    gamma.b[k] <-gamma.w[k]
    
    
  }
  
  ###odds for placebo arm via the placebo arm dataset
  for (i in 1:NpPlacebo){
    outcomeP[i]~dbern(pplacebo[i])
    ###formula
    logit(pplacebo[i])<-logitpplacebo+g0_placebo*Risk_placebo[i]
  }
  #prior for logitpplacebo
  g0_placebo~dnorm(0,0.001)
  logitpplacebo~dnorm(0,0.001)
  
  for(j in 1:nt){ORref[j]<- exp(delta[j] - delta[ref])}
  
  ##### calculation of predicted risk to new patients
  
  for (i in 1:Nnew){
    for(j in 1:nt){
      logitp[i,j]<-logitpplacebo+ delta[j]+ (g0_placebo+gamma.w[j])*(logitRisknew[i,1]) + (gamma.b[j]-gamma.w[j])*(logitmeanRisknew)
    }
  }
  
  
  #for (i in 1:99){
  # for(j in 1:nt){
  #  p[i,j]<-exp(logitp[i,j])/(1+exp(logitp[i,j]))
  #}
}
