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

  for (i in 1:(N.IPD.studies+N.AD.studies)){
    meanRisk[i]~dnorm(mumeanRisk[i], 1000)
    mumeanRisk[i]<-coef0+coef1*(coef3+coef4*(0.294872 + PooledEst[1,1]+PooledEst[2,1]*(ageAD[i]-meanAge)+PooledEst[3,1]*(disease.durationAD[i]-meanDiseaseDuration)+PooledEst[4,1]*(edssAD[i]-meanEdss)+PooledEst[5,1]*(exp(lognr.Gd.enhanced.lesionsAD[i])/(1+exp(lognr.Gd.enhanced.lesionsAD[i])))+PooledEst[6,1]*(exp(lognr.relapses.2y.prior.study1AD[i])/(1+exp(lognr.relapses.2y.prior.study1AD[i])))+PooledEst[7,1]*(exp(lognr.relapses.2y.prior.study2AD[i])/(1+exp(lognr.relapses.2y.prior.study2AD[i])))+PooledEst[8,1]*(months.since.last.relapseAD[i]-meanMonths.since.last.relapse)+PooledEst[9,1]*(exp(logtreatment.naive.prior.visitAD[i])/(1+exp(logtreatment.naive.prior.visitAD[i])))+PooledEst[10,1]*(exp(loggenderAD[i])/(1+exp(loggenderAD[i])))))+coef5*edssAD[i]+coef6*(exp(lognr.Gd.enhanced.lesionsAD[i])/(1+exp(lognr.Gd.enhanced.lesionsAD[i])))+coef7*(exp(logtreatment.naive.prior.visitAD[i])/(1+exp(logtreatment.naive.prior.visitAD[i])))

    lognr.relapses.2y.prior.study1AD[i]~dnorm(mulognr.relapses.2y.prior.study1AD[i], 0.01)
    mulognr.relapses.2y.prior.study1AD[i]<-gamma90+gamma91*ageAD[i]+gamma92*edssAD[i]+gamma93*months.since.last.relapseAD[i]+gamma94*disease.durarionAD[i]+gamma95*loggenderAD[i]+gamma96*lognr.Gd.enhanced.lesionsAD[i]+gamma97*logtreatment.naive.prior.visitAD[i]+gamma98*lognr.relapses.2y.prior.study2AD[i]


    lognr.relapses.2y.prior.study2AD[i]~dnorm(mulognr.relapses.2y.prior.study2AD[i], 0.01)
    mulognr.relapses.2y.prior.study2AD[i]<-gamma80+gamma81*ageAD[i]+gamma82*edssAD[i]+gamma83*months.since.last.relapseAD[i]+gamma84*disease.durarionAD[i]+gamma85*loggenderAD[i]+gamma86*lognr.Gd.enhanced.lesionsAD[i]+gamma87*logtreatment.naive.prior.visitAD[i]


    logtreatment.naive.prior.visitAD[i]~dnorm(mulogtreatment.naive.prior.visitAD[i], 0.01)
    mulogtreatment.naive.prior.visitAD[i]<-gamma70+gamma71*ageAD[i]+gamma72*edssAD[i]+gamma73*months.since.last.relapseAD[i]+gamma74*disease.durarionAD[i]+gamma75*loggenderAD[i]+gamma76*lognr.Gd.enhanced.lesionsAD[i]

    lognr.Gd.enhanced.lesionsAD[i]~dnorm(mulognr.Gd.enhanced.lesionsAD[i], 0.01)
    mulognr.Gd.enhanced.lesionsAD[i]<-gamma60+gamma61*ageAD[i]+gamma62*edssAD[i]+gamma63*months.since.last.relapseAD[i]+gamma64*disease.durarionAD[i]+gamma65*loggenderAD[i]


    loggenderAD[i]~dnorm(muloggenderAD[i], 0.01)
    muloggenderAD[i]<-gamma50+gamma51*ageAD[i]+gamma52*edssAD[i]+gamma53*months.since.last.relapseAD[i]+gamma54*disease.durarionAD[i]


    disease.durarionAD[i]~dnorm(mudisease.durationAD[i], 0.01)
    mudisease.durationAD[i]<-gamma40+gamma41*ageAD[i]+gamma42*edssAD[i]+gamma43*months.since.last.relapseAD[i]


    months.since.last.relapseAD[i]~dnorm(mumonths.since.last.relapseAD[i], 0.01)
    mumonths.since.last.relapseAD[i]<-gamma30+gamma31*ageAD[i]+gamma32*edssAD[i]

    edssAD[i]~dnorm(muedssAD[i],0.01)
    muedssAD[i]<-gamma20+gamma21*ageAD[i]

    ageAD[i]~dnorm(muageAD[i],0.01)
    muageAD[i]<-gamma10


    # mumeanRisk[i]<-as.numeric(coef.fit4[1]+coef.fit4[2]*(coef.lp3[1]+coef.lp3[2]*(0.294872 + Pooled2[1,1]+Pooled2[2,1]*(ageAD[i]-meanAge)+Pooled2[8,1]*(months.since.last.relapseAD[i]-meanMonths.since.last.relapse)
    #)))

  }

  ####PART III: Model for combining all treatment effect estimates
  #Vague priors
  #taumonths<-1/(sigma_x1*sigma_x1)
  #tauage<-1/(sigma_x2*sigma_x2)
  #sigma_x1~dunif(0,100)
  #sigma_x2~dunif(0,100)
  gamma10~dnorm(0,0.01)
  gamma20~dnorm(0,0.01)
  gamma21~dnorm(0,0.01)
  gamma30~dnorm(0,0.01)
  gamma31~dnorm(0,0.01)
  gamma32~dnorm(0,0.01)

  gamma40~dnorm(0,0.01)
  gamma41~dnorm(0,0.01)
  gamma42~dnorm(0,0.01)
  gamma43~dnorm(0,0.01)

  gamma50~dnorm(0,0.01)
  gamma51~dnorm(0,0.01)
  gamma52~dnorm(0,0.01)
  gamma53~dnorm(0,0.01)
  gamma54~dnorm(0,0.01)

  gamma60~dnorm(0,0.01)
  gamma61~dnorm(0,0.01)
  gamma62~dnorm(0,0.01)
  gamma63~dnorm(0,0.01)
  gamma64~dnorm(0,0.01)
  gamma65~dnorm(0,0.01)

  gamma70~dnorm(0,0.01)
  gamma71~dnorm(0,0.01)
  gamma72~dnorm(0,0.01)
  gamma73~dnorm(0,0.01)
  gamma74~dnorm(0,0.01)
  gamma75~dnorm(0,0.01)
  gamma76~dnorm(0,0.01)

  gamma80~dnorm(0,0.01)
  gamma81~dnorm(0,0.01)
  gamma82~dnorm(0,0.01)
  gamma83~dnorm(0,0.01)
  gamma84~dnorm(0,0.01)
  gamma85~dnorm(0,0.01)
  gamma86~dnorm(0,0.01)
  gamma87~dnorm(0,0.01)

  gamma90~dnorm(0,0.01)
  gamma91~dnorm(0,0.01)
  gamma92~dnorm(0,0.01)
  gamma93~dnorm(0,0.01)
  gamma94~dnorm(0,0.01)
  gamma95~dnorm(0,0.01)
  gamma96~dnorm(0,0.01)
  gamma97~dnorm(0,0.01)
  gamma98~dnorm(0,0.01)

  ##independent ui for each study
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


  ###odds for placebo arm via the placebo arm dataset
  for (i in 1:NpPlacebo){
    outcomeP[i]~dbern(pplacebo[i])
    ###formula
    logit(pplacebo[i])<-logitpplacebo
  }
  #prior for logitpplacebo
  logitpplacebo~dnorm(0,0.001)

  ##### calculation of predicted risk to new patients

  for (i in 1:Nnew){
    for(j in 1:nt){
      logitp[i,j]<-logitpplacebo+ delta[j]+ (gamma+gamma.w[j])*(logitRisknew[i,1]) + (gamma.b[j]-gamma.w[j])*(logitmeanRisknew)
    }
  }

}
