################## CHECK FOR the proper distribution ####################
####  Exponential versus Weibull #####################

## First the models, that will be in all cases used
#Weibull model
model_Weibull<-function(){
  for(i in 1:N){
    is.censored[i]~dinterval(t.obs[i],t.cens[i])
    t.obs[i]~dweibull(shape,lambda[i])
    log(lambda[i])<-m

  }
  m~dnorm(0,0.0001)
  shape~dgamma(0.01,0.001)


}
# Exponential model
model_Exponential<-function(){
  for(i in 1:N){
    is.censored[i]~dinterval(t.obs[i],t.cens[i])
    t.obs[i]~dexp(lambda[i])
    log(lambda[i])<-m

  }
  m~dnorm(0,0.001)
}


data_CDP_CC$is.censored[(data_CDP_CC$CDP==1)]<-0
data_CDP_CC$is.censored[(data_CDP_CC$CDP==0)]<-1

data_CDP_CC$Censored_Time[is.na(data_CDP_CC$Censored_Time)]<-0
data_CDP_CC$CDP<-as.numeric(data_CDP_CC$CDP)-1


DEFINE<-data_CDP_CC[data_CDP_CC$STUDYID==1,]
CONFIRM<-data_CDP_CC[data_CDP_CC$STUDYID==2,]
AFFIRM<-data_CDP_CC[data_CDP_CC$STUDYID==3,]


#jags data
jags_data_DEFINE<-list(
  N=nrow(DEFINE),
  t.obs=DEFINE$Time_to_CDP,
  t.cens=DEFINE$Censored_Time,
  is.censored=as.numeric(DEFINE$is.censored))

jags_data_CONFIRM<-list(
  N=nrow(CONFIRM),
  t.obs=CONFIRM$Time_to_CDP,
  t.cens=CONFIRM$Censored_Time,
  is.censored=as.numeric(CONFIRM$is.censored))

jags_data_AFFIRM<-list(
  N=nrow(AFFIRM),
  t.obs=AFFIRM$Time_to_CDP,
  t.cens=AFFIRM$Censored_Time,
  is.censored=as.numeric(AFFIRM$is.censored))


#### 1. DEFINE STUDY
#results from Exponential:
IPDJAGSresults_DEFINE_Exponential<-jags.parallel(data=jags_data_DEFINE, inits=NULL, parameters.to.save = c("m"), model.file = model_Exponential,
                                                          n.chains = 2, n.iter = 100000, n.burnin = 1000, DIC=F, n.thin=10)
#print(IPDJAGSresults_DEFINE_Exponential)
##print(IPDJAGSresults_seraphin_Exponential)
IPDJAGSresults_DEFINE_Weibull<-jags.parallel(data=jags_data_DEFINE, inits=NULL, parameters.to.save = c("m","shape"), model.file = model_Weibull,
                                                      n.chains = 2, n.iter = 100000, n.burnin = 1000, DIC=F, n.thin=10)
#print(IPDJAGSresults_DEFINE_Weibull)
lambda_DEFINE<-exp(IPDJAGSresults_DEFINE_Weibull$BUGSoutput$summary[1,1])
ni_DEFINE<-IPDJAGSresults_DEFINE_Weibull$BUGSoutput$summary[2,1]


###Combine all of them together in one plot to compare the curves
KM_DEFINE<-survfit(Surv(Time_to_CDP_orCensored, CDP) ~ 1, data=DEFINE)
t_DEFINE<-KM_DEFINE$time
Kaplan_Meier_DEFINE<-KM_DEFINE$surv
Exponential_DEFINE<-1-pexp(t_DEFINE, rate=exp(IPDJAGSresults_DEFINE_Exponential$BUGSoutput$summary[1]))
Weibull_DEFINE<-1-pweibull(t_DEFINE, shape= ni_DEFINE , scale= 1/(lambda_DEFINE)^(1/ni_DEFINE))
data_survivals_DEFINE<-as.data.frame(cbind(t_DEFINE,Kaplan_Meier_DEFINE,Exponential_DEFINE,Weibull_DEFINE))

names(data_survivals_DEFINE)<-c("time","Kaplan_Meier","Exponential","Weibull")

DEFINE_curves_EW<-ggplot(data_survivals_DEFINE, aes(time))+
  geom_line(aes(y = Kaplan_Meier_DEFINE, colour = "Kaplan_Meier"),size=1)+
  geom_line(aes(y = Exponential_DEFINE, colour = "Exponential"),size=1)+
  geom_line(aes(y = Weibull_DEFINE, colour = "Weibull"),size=1)+ylim(0,1)+
  ggtitle("Comparison of distributions in DEFINE study")+
  labs(y= "Survival", x = "Time (weeks)")

DEFINE_curves_EW


###### 2. CONFIRM

#results from Exponential:
IPDJAGSresults_CONFIRM_Exponential<-jags.parallel(data=jags_data_CONFIRM, inits=NULL, parameters.to.save = c("m"), model.file = model_Exponential,
                                                 n.chains = 2, n.iter = 100000, n.burnin = 1000, DIC=F, n.thin=10)
#print(IPDJAGSresults_CONFIRM_Exponential)
##print(IPDJAGSresults_seraphin_Exponential)
IPDJAGSresults_CONFIRM_Weibull<-jags.parallel(data=jags_data_CONFIRM, inits=NULL, parameters.to.save = c("m","shape"), model.file = model_Weibull,
                                             n.chains = 2, n.iter = 100000, n.burnin = 1000, DIC=F, n.thin=10)
#print(IPDJAGSresults_CONFIRM_Weibull)
lambda_CONFIRM<-exp(IPDJAGSresults_CONFIRM_Weibull$BUGSoutput$summary[1,1])
ni_CONFIRM<-IPDJAGSresults_CONFIRM_Weibull$BUGSoutput$summary[2,1]

CONFIRM$CDP<-as.numeric(CONFIRM$CDP)
###Combine all of them together in one plot to compare the curves
KM_CONFIRM<-survfit(Surv(Time_to_CDP_orCensored, CDP) ~ 1, data=CONFIRM)

t_CONFIRM<-KM_CONFIRM$time
Kaplan_Meier_CONFIRM<-KM_CONFIRM$surv
Exponential_CONFIRM<-1-pexp(t_CONFIRM, rate=exp(IPDJAGSresults_CONFIRM_Exponential$BUGSoutput$summary[1]))
Weibull_CONFIRM<-1-pweibull(t_CONFIRM, shape= ni_CONFIRM , scale= 1/(lambda_CONFIRM)^(1/ni_CONFIRM))
data_survivals_CONFIRM<-as.data.frame(cbind(t_CONFIRM,Kaplan_Meier_CONFIRM,Exponential_CONFIRM,Weibull_CONFIRM))

names(data_survivals_CONFIRM)<-c("time","Kaplan_Meier","Exponential","Weibull")

CONFIRM_curves_EW<-ggplot(data_survivals_CONFIRM, aes(time))+
  geom_line(aes(y = Kaplan_Meier_CONFIRM, colour = "Kaplan_Meier"),size=1)+
  geom_line(aes(y = Exponential_CONFIRM, colour = "Exponential"),size=1)+
  geom_line(aes(y = Weibull_CONFIRM, colour = "Weibull"),size=1)+ylim(0,1)+
  ggtitle("Comparison of distributions in CONFIRM study")+
  labs(y= "Survival", x = "Time (weeks)")

CONFIRM_curves_EW


#### 3. AFFIRM STUDY
#results from Exponential:
IPDJAGSresults_AFFIRM_Exponential<-jags.parallel(data=jags_data_AFFIRM, inits=NULL, parameters.to.save = c("m"), model.file = model_Exponential,
                                                 n.chains = 2, n.iter = 100000, n.burnin = 1000, DIC=F, n.thin=10)
#print(IPDJAGSresults_AFFIRM_Exponential)
##print(IPDJAGSresults_seraphin_Exponential)
IPDJAGSresults_AFFIRM_Weibull<-jags.parallel(data=jags_data_AFFIRM, inits=NULL, parameters.to.save = c("m","shape"), model.file = model_Weibull,
                                             n.chains = 2, n.iter = 100000, n.burnin = 1000, DIC=F, n.thin=10)
#print(IPDJAGSresults_AFFIRM_Weibull)
lambda_AFFIRM<-exp(IPDJAGSresults_AFFIRM_Weibull$BUGSoutput$summary[1,1])
ni_AFFIRM<-IPDJAGSresults_AFFIRM_Weibull$BUGSoutput$summary[2,1]


###Combine all of them together in one plot to compare the curves
KM_AFFIRM<-survfit(Surv(Time_to_CDP_orCensored, CDP) ~ 1, data=AFFIRM)
t_AFFIRM<-KM_AFFIRM$time
Kaplan_Meier_AFFIRM<-KM_AFFIRM$surv
Exponential_AFFIRM<-1-pexp(t_AFFIRM, rate=exp(IPDJAGSresults_AFFIRM_Exponential$BUGSoutput$summary[1]))
Weibull_AFFIRM<-1-pweibull(t_AFFIRM, shape= ni_AFFIRM , scale= 1/(lambda_AFFIRM)^(1/ni_AFFIRM))
data_survivals_AFFIRM<-as.data.frame(cbind(t_AFFIRM,Kaplan_Meier_AFFIRM,Exponential_AFFIRM,Weibull_AFFIRM))

names(data_survivals_AFFIRM)<-c("time","Kaplan_Meier","Exponential","Weibull")

AFFIRM_curves_EW<-ggplot(data_survivals_AFFIRM, aes(time))+
  geom_line(aes(y = Kaplan_Meier_AFFIRM, colour = "Kaplan_Meier"),size=1)+
  geom_line(aes(y = Exponential_AFFIRM, colour = "Exponential"),size=1)+
  geom_line(aes(y = Weibull_AFFIRM, colour = "Weibull"),size=1)+ylim(0,1)+
  ggtitle("Comparison of distributions in AFFIRM study")+
  labs(y= "Survival", x = "Time (weeks)")

AFFIRM_curves_EW

ggarrange(DEFINE_curves_EW,CONFIRM_curves_EW,AFFIRM_curves_EW, ncol = 2, nrow=2)
