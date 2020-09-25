###############################################################################################################
################################# JAGS MODEL FOR GENERALIZED MIXED EFFECTS - BINARY OUTCOME ####################################
#################################         including the Laplace shrinkage of coefficients              ####################################
###############################################################################################################

########################## vARIANCE-COVARIANCE MATRIX: sigma^2 in the diagonals and all the other rho*sigma^2 ##############################################


jagsmodelSMSC <-function()
{


  for( i in 1:Nobservations)
  {
    outcome[i]~dbern(p[i])
    #likelihood
    logit(p[i])<-b[1]+u[subj[i],1] + (b[2]+u[subj[i],2])*age[i]+(b[3]+u[subj[i],3])*disease.duration[i]+
      (b[4]+u[subj[i],4])*edss[i]+(b[5]+u[subj[i],5])*nr.Gd.enhanced.lesions[i]+(b[6]+u[subj[i],6])*nr.relapses.2y.prior.study1[i]+
      (b[7]+u[subj[i],7])*nr.relapses.2y.prior.study2[i]+
      (b[8]+u[subj[i],8])*months.since.last.relapse[i]+(b[9]+u[subj[i],9])*treatment.naive.prior.visit[i] +
      (b[10]+u[subj[i],10])*gender[i]+ (b[11]+u[subj[i],11])*treatment.during.cycle[i]

  }


  #priors for fixed effects for b[i] (fixef intercept)
    b[1]~dnorm(0,0.001)
  for (i in 2:(npf+1)){
    #priors for fixed effects for b[i] slopes - Laplace priors for shrinkage
    b[i]~ddexp(0,tauBeta)
  }


    tauBeta <-pow(sdBeta,-2)
    sdBeta ~ dunif(0,20)

  for(i in 1:(npf+1)){
    zero[i]<-0
  }

  for (i in 1:npid){
    u[i,1:(npf+1)]~dmnorm(zero, Omega.u) #precision matrix
  }

  #prior on precision Omega.u
  Omega.u~dwish(R.u,npf+1)

  ## fill in the R.u variance-covariance matrix
  for(i in 1:(npf+1)){
    R.u[i,i]<-pow(sigma,2)
  }

  for(i in 1:npf) {
    for(j in (i+1):(npf+1)){
      R.u[i,j]<-rho*pow(sigma,2)
    }
  }

  for (i in 2:(npf+1)){
    for(j in (1:(i-1))){
      R.u[i,j]<-rho*pow(sigma,2)
    }
  }

  # prior for the sigma
  sigma~dunif(0,0.5)

  #prior for the correlation rho
  rho~dunif(-0.1,1)

}
