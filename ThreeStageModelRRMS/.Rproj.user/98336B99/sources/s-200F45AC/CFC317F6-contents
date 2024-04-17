#########################################################################
####################  Model for IPD & AD NMR #########################
#### with imputations in missing study-level covariates  #####
#########################################################################3


modelIPDADNMR<-function(){

  #############Part I: model for IPD data
  for (i in 1:Np){
    outcome[i]~dbern(p[i])
    ###formula
    logit(p[i])<-u[studyid[i]] + d[studyid[i], arm[i]]+ (g[studyid[i]]+ g.w[studyid[i],arm[i]])*(Risk[i])
    + (g.b[studyid[i],arm[i]]-g.w[studyid[i],arm[i]])*(meanRisk[studyid[i]])

  }

  #####treatment effects - fixed across studies & correction for multi-arm studies
  for(i in 1:N.IPD.studies){
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

  ###fixed across studies for g
  for (i in 1:N.IPD.studies){
    g[i]<-gamma


  }


  ##Part II: Model for AD data


  for(i in (N.IPD.studies+1):(N.AD.studies +N.IPD.studies)){
    d[i,treat[i,1] ]<- 0
    g.b[i,treat[i,1]]<-0
    ### formula for study's reference
    logit(pa[i, treat[i,1] ])<-u[i]

    for (k in 1:na[i]){
      ##likelihood
      r[i,treat[i,k]]~dbin(pa[i,treat[i,k]], n[i,treat[i,k]])
    }
    for(k in 2:na[i]){
      ### formula - fixed across studies
      logit(pa[i, treat[i,k] ])<-u[i] + d[i, treat[i,k]] #+ g.b[i,treat[i,k]]*meanRisk[i]

      d[i, treat[i,k]]<-(delta[treat[i, k]] - delta[treat[i, 1]])+(gamma.b[treat[i,k]]-gamma.b[treat[i,1]])*meanRisk[i]




      #You need the recoalibrated risk score

      #md[i, treat[i,k]]<-(delta[treat[i, k]] - delta[treat[i, 1]])+(gamma.b[treat[i,k]]-gamma.b[treat[i,1]])*meanRisk[i]

      #md[i, treat[i,k]]<- mean[i, k] + sw[i, k]
      #w[i, k] <- (d[i, treat[i, k]] - mean[i, k])
      #sw[i, k] <- sum(w[i, 1:(k - 1)])/(k - 1)
      #mean[i, k] <- (delta[treat[i, k]] - delta[treat[i, 1]])+(gamma.b[treat[i,k]]-gamma.b[treat[i,1]])*meanRisk[i]
      # g.b[i, treat[i,k]<-gamma.b[treat[i,k]]-gamma.b[treat[i,1]]
    }
  }

  gamma~dnorm(0,0.001)

  #independent ui for each study
  for (i in 1:(N.IPD.studies+N.AD.studies)){
    u[i]~dnorm(0,0.001)
  }

  delta[ref]<-0
  gamma.w[ref]<-0
  gamma.b[ref]<-0
  for(k in 1:(ref-1)){
    delta[k]~dnorm(0,0.001)
    gamma.w[k] ~ dnorm(0, 0.001)
    gamma.b[k] <- gamma.w[k]
  }
  for(k in (ref+1):nt){
    delta[k]~dnorm(0,0.001)
    gamma.w[k] ~ dnorm(0, 0.001)
    gamma.b[k] <- gamma.w[k]
  }



  ###Calculation of ORs
  for(j in 1:nt){ORref[j]<- exp(delta[j] - delta[ref])}


  ###odds for RCTs setting, using placebo arms via the placebo arm dataset
  for (i in 1:NpPlacebo){
    outcomeP[i]~dbern(pplacebo[i])
    ###formula
    logit(pplacebo[i])<-logitpplacebo
  }
  #prior for logitpplacebo
  logitpplacebo~dnorm(0,0.001)
  ###odds for observational setting, using SMSC

  for (i in 1:NpPlacebo2){
    outcomeP2[i]~dbern(pplacebo2[i])
    ###formula
    logit(pplacebo2[i])<-logitpplacebo2+g0_placebo2*RiskPlacebo2[i]
  }

  for (i in 1:NpPlacebo3){
    outcomeP3[i]~dbern(pplacebo3[i])
    ###formula
    logit(pplacebo3[i])<-logitpplacebo3+g0_placebo3*Risk_placebo3[i]
  }
  #prior for logitpplacebo
  logitpplacebo2~dnorm(0,0.001)
  logitpplacebo3~dnorm(0,0.001)
  g0_placebo3~dnorm(0,0.001)
  g0_placebo2~dnorm(0,0.001)

  ##### calculation of predicted risk to new  RCTs patients

  for (i in 1:Nnew){
    for(j in 1:nt){
      logitp[i,j]<-logitpplacebo3+ delta[j]+ (g0_placebo3+gamma.w[j])*(logitRisknew[i,1]) + (gamma.b[j]-gamma.w[j])*(logitmeanRisknew)
    }
  }

  ##### calculation of predicted risk to new  RCTs patients

  for (i in 1:Nnew){
    for(j in 1:nt){
      logitp2[i,j]<-logitpplacebo2+ delta[j]+ (g0_placebo2+gamma.w[j])*(logitRisknew[i,1]) + (gamma.b[j]-gamma.w[j])*(logitmeanRisknew2)
    }
  }


  for (i in 1:Np){
    logitp_internal2[i]<-logitpplacebo3+ delta[treatment[i]]+ (g0_placebo3+gamma.w[treatment[i]])*(Risk[i]) + (gamma.b[treatment[i]]-gamma.w[treatment[i]])*(logitmeanRisknew)
  }


}
