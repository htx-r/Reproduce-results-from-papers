#########################################################################
####################  Model for IPD & AD NMR #########################
#### with imputations in missing study-level covariates  #####
#########################################################################3


AD_imputation<-function(){
  for (i in 1:(N.IPD.studies+N.AD.studies)){
    logmean_relapse[i]~dnorm(mulogmean_relapse[i], 1000)
    mulogmean_relapse[i]<-gamma110+gamma119*mulognr.relapses.2y.prior.study1AD[i]+gamma111*ageAD[i]+gamma112*edssAD[i]+gamma113*months.since.last.relapseAD[i]+gamma114*disease.durarionAD[i]+gamma115*loggenderAD[i]+gamma116*lognr.Gd.enhanced.lesionsAD[i]+gamma117*logtreatment.naive.prior.visitAD[i]+gamma118*lognr.relapses.2y.prior.study2AD[i]
    
      
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

  gamma110~dnorm(0,0.01)
  gamma111~dnorm(0,0.01)
  gamma112~dnorm(0,0.01)
  gamma113~dnorm(0,0.01)
  gamma114~dnorm(0,0.01)
  gamma115~dnorm(0,0.01)
  gamma116~dnorm(0,0.01)
  gamma117~dnorm(0,0.01)
  gamma118~dnorm(0,0.01)
  gamma119~dnorm(0,0.01)
  
  
}
