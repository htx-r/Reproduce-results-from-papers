######PseudoData creation for baseline risk estimation of AD studies

##Variance covariance matrix estimated via the IPD
tokeep<-c("age","gender","edss","disease.duration","months.since.last.relapse","treatment.naive.prior.visit","nr.Gd.enhanced.lesions","nr.relapses.2y.prior.study1","nr.relapses.2y.prior.study2")
df<-RiskData[,(names(RiskData) %in% tokeep)]
cov_matrix<-cov(df)
#mean values as given in AD Bornstein study or estimated via the imputation method
mu1<-c(30.5,0.3227734,3.05,2.750471,2.4,2.090741,0.1362102,-2.415478,2.164327)

#pseudo_data creation for Bornstein AD study
set.seed(2000)
pseudo1<-mvrnorm(n = 50, mu=mu1, Sigma=cov_matrix, tol = 1e-6, empirical = TRUE, EISPACK = FALSE)

pseudo1<-as.data.frame(pseudo1)
pseudo1$treatment.naive.prior.visit<-expit(pseudo1$treatment.naive.prior.visit)
pseudo1$gender<-expit(pseudo1$gender)
pseudo1$nr.Gd.enhanced.lesions<-expit(pseudo1$nr.Gd.enhanced.lesions)
pseudo1$nr.relapses.2y.prior.study1<-expit(pseudo1$nr.relapses.2y.prior.study1)
pseudo1$nr.relapses.2y.prior.study2<-expit(pseudo1$nr.relapses.2y.prior.study2)


pseudo1$logitp_new<--1.137-0.025*(pseudo1$age-37.04233)+0.237*(pseudo1$disease.duration-2.824242)+0.265*(pseudo1$edss-2.390698)+0.217*pseudo1$nr.Gd.enhanced.lesions-0.049*pseudo1$nr.relapses.2y.prior.study1+0.093*pseudo1$nr.relapses.2y.prior.study2-0.335*(pseudo1$months.since.last.relapse-2.774328)-0.244*pseudo1$treatment.naive.prior.visit+0.178*pseudo1$gender
pseudo1$Risk_new<-expit(pseudo1$logitp_new)
summary(pseudo1$Risk_new)


mu2<-c(34.45,1.0098970,2.6,2.829678,2.5,2.716616,0.5580447,-1.658228,1.295046)

cov_matrix2<-cov_matrix
cov_matrix2[1,1]<-6.25^2
cov_matrix2[3,3]<-1.25^2
cov_matrix2[4,4]<-2.7^2

set.seed(2000)
pseudo2<-mvrnorm(n = 251, mu=mu2, Sigma=cov_matrix2, tol = 1e-6, empirical = TRUE, EISPACK = FALSE)

pseudo2<-as.data.frame(pseudo2)
pseudo2$treatment.naive.prior.visit<-expit(pseudo2$treatment.naive.prior.visit)
pseudo2$gender<-expit(pseudo2$gender)
pseudo2$nr.Gd.enhanced.lesions<-expit(pseudo2$nr.Gd.enhanced.lesions)
pseudo2$nr.relapses.2y.prior.study1<-expit(pseudo2$nr.relapses.2y.prior.study1)
pseudo2$nr.relapses.2y.prior.study2<-expit(pseudo2$nr.relapses.2y.prior.study2)

pseudo2$logitp_new<--1.137-0.025*(pseudo2$age-37.04233)+0.237*(pseudo2$disease.duration-2.824242)+0.265*(pseudo2$edss-2.390698)+0.217*pseudo2$nr.Gd.enhanced.lesions-0.049*pseudo2$nr.relapses.2y.prior.study1+0.093*pseudo2$nr.relapses.2y.prior.study2-0.335*(pseudo2$months.since.last.relapse-2.774328)-0.244*pseudo2$treatment.naive.prior.visit+0.178*pseudo2$gender
pseudo2$Risk_new<-expit(pseudo2$logitp_new)
summary(pseudo2$Risk_new)

