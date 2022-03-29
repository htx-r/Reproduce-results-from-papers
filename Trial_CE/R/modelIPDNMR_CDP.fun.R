###################################################################################################################################
##############################   Model for IPD and AD NMR  #######################################
##############################################################################################################################


modelIPDNMR_HR<-function(){
  for(i in 1:N) {
    is.censored[i] ~ dinterval(t.obs[i], t.cens[i])
    t.obs[i] ~ dexp(lambda[i])
    log(lambda[i]) <- u[studyid[i]] + d[studyid[i], arm[i]]+ g0[studyid[i]]*(cov[i]) + g[studyid[i],arm[i]]*(cov[i])
    }
  for (j in 1:Nstudies){
    g0[j]<-gamma0}
  gamma0~dnorm(0,0.001)
  for(j in 1:Nstudies){
    d[j,1] <- 0
    w[j,1] <- 0
    g[j,1]<-0
    for(k in 2:na[j]){
      d[j,k]<-md[j, k]
      md[j, k] <- mean[j, k] + sw[j, k]
      w[j, k] <- (d[j, k] - mean[j, k])
      sw[j, k] <- sum(w[j, 1:(k - 1)])/(k - 1)
      mean[j, k] <- delta[treat[j, k]] - delta[treat[j, 1]]
      g[j,k]<-gamma[treat[j,k]]-gamma[treat[j,1]]}}
  for (j in 1:Nstudies){
    u[j]~dnorm(0,0.001)}

  ###odds for placebo arm via the placebo arm dataset
  for (i in 1:NpPlacebo){
    is.censoredP[i] ~ dinterval(t.obsP[i], t.censP[i])
    t.obsP[i] ~ dexp(lambdaP[i])
    ###formula
    log(lambdaP[i])<-logHazardsPlacebo[studyidP[i]]
  }

  for (j in 1:NPstudies){
    logHazardsPlacebo[j]<-logPlacebo}
  #prior for logitpplacebo
  logPlacebo~dnorm(0,0.001)
  delta[ref] <- 0
  gamma[ref] <- 0
  for (k in 1:(ref-1)){
    delta[k] ~ dnorm(0, 0.001)
    gamma[k] ~ dnorm(0, 0.001)}
  for (k in (ref+1):nt){
    delta[k] ~ dnorm(0, 0.001)
    gamma[k] ~ dnorm(0, 0.001)}

  for (i in 1:Nnew){
    for(j in 1:nt){
      logh[i,j]<-logPlacebo+ delta[j]+ gamma0*(logitRisknew[i,1])+ gamma[j]*(logitRisknew[i,1])
    }
  }
}


