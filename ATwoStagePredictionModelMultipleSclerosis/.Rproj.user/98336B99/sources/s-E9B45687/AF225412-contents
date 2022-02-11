###################################################################################################################################
##############################   Model for IPD NMR with Risk only as prognostic factor #######################################
##############################################################################################################################

modelIPDNMRPr<-function(){
  ###likelihood
  for (i in 1:Np){
    outcome[i]~dbern(p[i])
    ###formula
    logit(p[i])<-u[studyid[i]] + d[studyid[i], arm[i]] + g[studyid[i]]*(Risk[i]-meanRisk[studyid[i]])}

  ## g fixed across studies

  for (i in 1:Nstudies){
    g[i]<-gamma
  }

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

  gamma~dnorm(0,0.001)

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
}
