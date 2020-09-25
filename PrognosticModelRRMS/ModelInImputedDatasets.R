###############################################################################################################
#####################  Run the Bayesian model to each one of the imputed datasets #####################
##############################################################################################################



#give the data for imputed dataset 1
jagsdataSMSC1 <- list(
  Nobservations=nrow(imp.list[[1]]),
  outcome=as.numeric(factor(imp.list[[1]]$outcome))-1,
  npf=10,
  npid=length(unique(imp.list[[1]]$patient.id)),
  subj=as.integer(factor(imp.list[[1]]$patient.id)),
  age=imp.list[[1]]$age-mean(imp.list[[1]]$age),
  disease.duration=imp.list[[1]]$disease.duration-mean(imp.list[[1]]$disease.duration),
  edss=imp.list[[1]]$edss-mean(imp.list[[1]]$edss),
  nr.Gd.enhanced.lesions=as.factor(imp.list[[1]]$nr.Gd.enhanced.lesions),
  nr.relapses.2y.prior.study1=as.factor(imp.list[[1]]$nr.relapses.2y.prior.study1),
  nr.relapses.2y.prior.study2=as.factor(imp.list[[1]]$nr.relapses.2y.prior.study2),
  months.since.last.relapse=imp.list[[1]]$months.since.last.relapse-mean(imp.list[[1]]$months.since.last.relapse),
  treatment.naive.prior.visit=as.factor(imp.list[[1]]$treatment.naive.prior.visit),
  gender=as.factor(imp.list[[1]]$gender),
  treatment.during.cycle=as.factor(imp.list[[1]]$treatment.during.cycle)
)

#give the data for imputed dataset 1
jagsdataSMSC2<- list(
  Nobservations=nrow(imp.list[[2]]),
  outcome=as.numeric(factor(imp.list[[2]]$outcome))-1,
  npf=10,
  npid=length(unique(imp.list[[2]]$patient.id)),
  subj=as.integer(factor(imp.list[[2]]$patient.id)),
  age=imp.list[[2]]$age-mean(imp.list[[2]]$age),
  disease.duration=imp.list[[2]]$disease.duration-mean(imp.list[[2]]$disease.duration),
  edss=imp.list[[2]]$edss-mean(imp.list[[2]]$edss),
  nr.Gd.enhanced.lesions=as.factor(imp.list[[2]]$nr.Gd.enhanced.lesions),
  nr.relapses.2y.prior.study1=as.factor(imp.list[[2]]$nr.relapses.2y.prior.study1),
  nr.relapses.2y.prior.study2=as.factor(imp.list[[2]]$nr.relapses.2y.prior.study2),
  months.since.last.relapse=imp.list[[2]]$months.since.last.relapse-mean(imp.list[[2]]$months.since.last.relapse),
  treatment.naive.prior.visit=as.factor(imp.list[[2]]$treatment.naive.prior.visit),
  gender=as.factor(imp.list[[2]]$gender),
  treatment.during.cycle=as.factor(imp.list[[2]]$treatment.during.cycle)
)

#give the data for imputed dataset 3
jagsdataSMSC3<- list(
  Nobservations=nrow(imp.list[[3]]),
  outcome=as.numeric(factor(imp.list[[3]]$outcome))-1,
  npf=10,
  npid=length(unique(imp.list[[3]]$patient.id)),
  subj=as.integer(factor(imp.list[[3]]$patient.id)),
  age=imp.list[[3]]$age-mean(imp.list[[3]]$age),
  disease.duration=imp.list[[3]]$disease.duration-mean(imp.list[[3]]$disease.duration),
  edss=imp.list[[3]]$edss-mean(imp.list[[3]]$edss),
  nr.Gd.enhanced.lesions=as.factor(imp.list[[3]]$nr.Gd.enhanced.lesions),
  nr.relapses.2y.prior.study1=as.factor(imp.list[[3]]$nr.relapses.2y.prior.study1),
  nr.relapses.2y.prior.study2=as.factor(imp.list[[3]]$nr.relapses.2y.prior.study2),
  months.since.last.relapse=imp.list[[3]]$months.since.last.relapse-mean(imp.list[[3]]$months.since.last.relapse),
  treatment.naive.prior.visit=as.factor(imp.list[[3]]$treatment.naive.prior.visit),
  gender=as.factor(imp.list[[3]]$gender),
  treatment.during.cycle=as.factor(imp.list[[3]]$treatment.during.cycle)
)

#give the data for imputed dataset 4
jagsdataSMSC4<- list(
  Nobservations=nrow(imp.list[[4]]),
  outcome=as.numeric(factor(imp.list[[4]]$outcome))-1,
  npf=10,
  npid=length(unique(imp.list[[4]]$patient.id)),
  subj=as.integer(factor(imp.list[[4]]$patient.id)),
  age=imp.list[[4]]$age-mean(imp.list[[4]]$age),
  disease.duration=imp.list[[4]]$disease.duration-mean(imp.list[[4]]$disease.duration),
  edss=imp.list[[4]]$edss-mean(imp.list[[4]]$edss),
  nr.Gd.enhanced.lesions=as.factor(imp.list[[4]]$nr.Gd.enhanced.lesions),
  nr.relapses.2y.prior.study1=as.factor(imp.list[[4]]$nr.relapses.2y.prior.study1),
  nr.relapses.2y.prior.study2=as.factor(imp.list[[4]]$nr.relapses.2y.prior.study2),
  months.since.last.relapse=imp.list[[4]]$months.since.last.relapse-mean(imp.list[[4]]$months.since.last.relapse),
  treatment.naive.prior.visit=as.factor(imp.list[[4]]$treatment.naive.prior.visit),
  gender=as.factor(imp.list[[4]]$gender),
  treatment.during.cycle=as.factor(imp.list[[4]]$treatment.during.cycle)
)

#give the data for imputed dataset 5
jagsdataSMSC5<- list(
  Nobservations=nrow(imp.list[[5]]),
  outcome=as.numeric(factor(imp.list[[5]]$outcome))-1,
  npf=10,
  npid=length(unique(imp.list[[5]]$patient.id)),
  subj=as.integer(factor(imp.list[[5]]$patient.id)),
  age=imp.list[[5]]$age-mean(imp.list[[5]]$age),
  disease.duration=imp.list[[5]]$disease.duration-mean(imp.list[[5]]$disease.duration),
  edss=imp.list[[5]]$edss-mean(imp.list[[5]]$edss),
  nr.Gd.enhanced.lesions=as.factor(imp.list[[5]]$nr.Gd.enhanced.lesions),
  nr.relapses.2y.prior.study1=as.factor(imp.list[[5]]$nr.relapses.2y.prior.study1),
  nr.relapses.2y.prior.study2=as.factor(imp.list[[5]]$nr.relapses.2y.prior.study2),
  months.since.last.relapse=imp.list[[5]]$months.since.last.relapse-mean(imp.list[[5]]$months.since.last.relapse),
  treatment.naive.prior.visit=as.factor(imp.list[[5]]$treatment.naive.prior.visit),
  gender=as.factor(imp.list[[5]]$gender),
  treatment.during.cycle=as.factor(imp.list[[5]]$treatment.during.cycle)
)

#give the data for imputed dataset 6
jagsdataSMSC6<- list(
  Nobservations=nrow(imp.list[[6]]),
  outcome=as.numeric(factor(imp.list[[6]]$outcome))-1,
  npf=10,
  npid=length(unique(imp.list[[6]]$patient.id)),
  subj=as.integer(factor(imp.list[[6]]$patient.id)),
  age=imp.list[[6]]$age-mean(imp.list[[6]]$age),
  disease.duration=imp.list[[6]]$disease.duration-mean(imp.list[[6]]$disease.duration),
  edss=imp.list[[6]]$edss-mean(imp.list[[6]]$edss),
  nr.Gd.enhanced.lesions=as.factor(imp.list[[6]]$nr.Gd.enhanced.lesions),
  nr.relapses.2y.prior.study1=as.factor(imp.list[[6]]$nr.relapses.2y.prior.study1),
  nr.relapses.2y.prior.study2=as.factor(imp.list[[6]]$nr.relapses.2y.prior.study2),
  months.since.last.relapse=imp.list[[6]]$months.since.last.relapse-mean(imp.list[[6]]$months.since.last.relapse),
  treatment.naive.prior.visit=as.factor(imp.list[[6]]$treatment.naive.prior.visit),
  gender=as.factor(imp.list[[6]]$gender),
  treatment.during.cycle=as.factor(imp.list[[6]]$treatment.during.cycle)
)

#give the data for imputed dataset 7
jagsdataSMSC7<- list(
  Nobservations=nrow(imp.list[[7]]),
  outcome=as.numeric(factor(imp.list[[7]]$outcome))-1,
  npf=10,
  npid=length(unique(imp.list[[7]]$patient.id)),
  subj=as.integer(factor(imp.list[[7]]$patient.id)),
  age=imp.list[[7]]$age-mean(imp.list[[7]]$age),
  disease.duration=imp.list[[7]]$disease.duration-mean(imp.list[[7]]$disease.duration),
  edss=imp.list[[7]]$edss-mean(imp.list[[7]]$edss),
  nr.Gd.enhanced.lesions=as.factor(imp.list[[7]]$nr.Gd.enhanced.lesions),
  nr.relapses.2y.prior.study1=as.factor(imp.list[[7]]$nr.relapses.2y.prior.study1),
  nr.relapses.2y.prior.study2=as.factor(imp.list[[7]]$nr.relapses.2y.prior.study2),
  months.since.last.relapse=imp.list[[7]]$months.since.last.relapse-mean(imp.list[[7]]$months.since.last.relapse),
  treatment.naive.prior.visit=as.factor(imp.list[[7]]$treatment.naive.prior.visit),
  gender=as.factor(imp.list[[7]]$gender),
  treatment.during.cycle=as.factor(imp.list[[7]]$treatment.during.cycle)
)

#give the data for imputed dataset 8
jagsdataSMSC8<- list(
  Nobservations=nrow(imp.list[[8]]),
  outcome=as.numeric(factor(imp.list[[8]]$outcome))-1,
  npf=10,
  npid=length(unique(imp.list[[8]]$patient.id)),
  subj=as.integer(factor(imp.list[[8]]$patient.id)),
  age=imp.list[[8]]$age-mean(imp.list[[8]]$age),
  disease.duration=imp.list[[8]]$disease.duration-mean(imp.list[[8]]$disease.duration),
  edss=imp.list[[8]]$edss-mean(imp.list[[8]]$edss),
  nr.Gd.enhanced.lesions=as.factor(imp.list[[8]]$nr.Gd.enhanced.lesions),
  nr.relapses.2y.prior.study1=as.factor(imp.list[[8]]$nr.relapses.2y.prior.study1),
  nr.relapses.2y.prior.study2=as.factor(imp.list[[8]]$nr.relapses.2y.prior.study2),
  months.since.last.relapse=imp.list[[8]]$months.since.last.relapse-mean(imp.list[[8]]$months.since.last.relapse),
  treatment.naive.prior.visit=as.factor(imp.list[[8]]$treatment.naive.prior.visit),
  gender=as.factor(imp.list[[8]]$gender),
  treatment.during.cycle=as.factor(imp.list[[8]]$treatment.during.cycle)
)

#give the data for imputed dataset 9
jagsdataSMSC9<- list(
  Nobservations=nrow(imp.list[[9]]),
  outcome=as.numeric(factor(imp.list[[9]]$outcome))-1,
  npf=10,
  npid=length(unique(imp.list[[9]]$patient.id)),
  subj=as.integer(factor(imp.list[[9]]$patient.id)),
  age=imp.list[[9]]$age-mean(imp.list[[9]]$age),
  disease.duration=imp.list[[9]]$disease.duration-mean(imp.list[[9]]$disease.duration),
  edss=imp.list[[9]]$edss-mean(imp.list[[9]]$edss),
  nr.Gd.enhanced.lesions=as.factor(imp.list[[9]]$nr.Gd.enhanced.lesions),
  nr.relapses.2y.prior.study1=as.factor(imp.list[[9]]$nr.relapses.2y.prior.study1),
  nr.relapses.2y.prior.study2=as.factor(imp.list[[9]]$nr.relapses.2y.prior.study2),
  months.since.last.relapse=imp.list[[9]]$months.since.last.relapse-mean(imp.list[[9]]$months.since.last.relapse),
  treatment.naive.prior.visit=as.factor(imp.list[[9]]$treatment.naive.prior.visit),
  gender=as.factor(imp.list[[9]]$gender),
  treatment.during.cycle=as.factor(imp.list[[9]]$treatment.during.cycle)
)

#give the data for imputed dataset 10
jagsdataSMSC10<- list(
  Nobservations=nrow(imp.list[[10]]),
  outcome=as.numeric(factor(imp.list[[10]]$outcome))-1,
  npf=10,
  npid=length(unique(imp.list[[10]]$patient.id)),
  subj=as.integer(factor(imp.list[[10]]$patient.id)),
  age=imp.list[[10]]$age-mean(imp.list[[10]]$age),
  disease.duration=imp.list[[10]]$disease.duration-mean(imp.list[[10]]$disease.duration),
  edss=imp.list[[10]]$edss-mean(imp.list[[10]]$edss),
  nr.Gd.enhanced.lesions=as.factor(imp.list[[10]]$nr.Gd.enhanced.lesions),
  nr.relapses.2y.prior.study1=as.factor(imp.list[[10]]$nr.relapses.2y.prior.study1),
  nr.relapses.2y.prior.study2=as.factor(imp.list[[10]]$nr.relapses.2y.prior.study2),
  months.since.last.relapse=imp.list[[10]]$months.since.last.relapse-mean(imp.list[[10]]$months.since.last.relapse),
  treatment.naive.prior.visit=as.factor(imp.list[[10]]$treatment.naive.prior.visit),
  gender=as.factor(imp.list[[10]]$gender),
  treatment.during.cycle=as.factor(imp.list[[10]]$treatment.during.cycle)
)


jags.inits <- function(){
  list("b"=c(0,0,0,0,0,0,0,0,0,0,0),"sigma"=0.02,"rho"=0.4)
}

set.seed(2000)
SMSCjagsResults1 <- jags.parallel(data = jagsdataSMSC1 ,inits=jags.inits,parameters.to.save = c('b','sigma', 'rho' ),model.file = jagsmodelSMSC,
                                  n.chains=2,n.iter = 50000,n.burnin = 10000,n.thin = 10)
set.seed(2000)
SMSCjagsResults2 <- jags.parallel(data = jagsdataSMSC2 ,inits=jags.inits,parameters.to.save = c('b','sigma', 'rho' ),model.file = jagsmodelSMSC,
                                  n.chains=2,n.iter = 50000,n.burnin = 10000,n.thin = 10)
set.seed(2000)
SMSCjagsResults3 <- jags.parallel(data = jagsdataSMSC3 ,inits=jags.inits,parameters.to.save = c('b','sigma', 'rho' ),model.file = jagsmodelSMSC,
                                  n.chains=2,n.iter = 50000,n.burnin = 10000,n.thin = 10)
set.seed(2000)
SMSCjagsResults4 <- jags.parallel(data = jagsdataSMSC4 ,inits=jags.inits,parameters.to.save = c('b','sigma', 'rho' ),model.file = jagsmodelSMSC,
                                  n.chains=2,n.iter = 50000,n.burnin = 10000,n.thin = 10)
set.seed(2000)
SMSCjagsResults5 <- jags.parallel(data = jagsdataSMSC5 ,inits=jags.inits,parameters.to.save = c('b','sigma', 'rho' ),model.file = jagsmodelSMSC,
                                  n.chains=2,n.iter = 50000,n.burnin = 10000,n.thin = 10)
set.seed(2000)
SMSCjagsResults6 <- jags.parallel(data = jagsdataSMSC6 ,inits=jags.inits,parameters.to.save = c('b','sigma', 'rho' ),model.file = jagsmodelSMSC,
                                  n.chains=2,n.iter = 50000,n.burnin = 10000,n.thin = 10)
set.seed(2000)
SMSCjagsResults7 <- jags.parallel(data = jagsdataSMSC7 ,inits=jags.inits,parameters.to.save = c('b','sigma', 'rho' ),model.file = jagsmodelSMSC,
                                  n.chains=2,n.iter = 50000,n.burnin = 10000,n.thin = 10)
set.seed(2000)
SMSCjagsResults8 <- jags.parallel(data = jagsdataSMSC8 ,inits=jags.inits,parameters.to.save = c('b','sigma', 'rho' ),model.file = jagsmodelSMSC,
                                  n.chains=2,n.iter = 50000,n.burnin = 10000,n.thin = 10)
set.seed(2000)
SMSCjagsResults9 <- jags.parallel(data = jagsdataSMSC9 ,inits=jags.inits,parameters.to.save = c('b','sigma', 'rho' ),model.file = jagsmodelSMSC,
                                   n.chains=2,n.iter = 50000,n.burnin = 10000,n.thin = 10)
set.seed(2000)
SMSCjagsResults10 <- jags.parallel(data = jagsdataSMSC10 ,inits=jags.inits,parameters.to.save = c('b','sigma', 'rho' ),model.file = jagsmodelSMSC,
                                   n.chains=2,n.iter = 50000,n.burnin = 10000,n.thin = 10)
