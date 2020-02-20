

dataset<-MSrelapse
X<-na.omit(dataset)

###logit risk and risk for each one of the patients based one the model
X$logitRiskLASSO<-predict(LASSOModel$lassomodel,new=X)
X$RiskLASSO<-exp(X$logitRiskLASSO)/(1+exp(X$logitRiskLASSO))
###logit risk and risk for each one of the patients based one the model
X$logitRiskPreSpecified<-predict(PreSpecifiedModel$PreSpecifiedmodel,new=X)
X$RiskPreSpecified<-exp(X$logitRiskPreSpecified)/(1+exp(X$logitRiskPreSpecified))

##make treatment and study id numeric for the prediction model
X$TRT01A<-as.numeric(X$TRT01A)
X$TRT01A<-recode(X$TRT01A, "2='1'; 4='2';5='3'; 7='4';")
###1= Dimethyl fumarate, 2=Glatiramer acetate , 3=Natalizumab, 4=Placebo
X$TRT01A<-as.factor(X$TRT01A)
#recode of studyid
X$STUDYID<-as.numeric(X$STUDYID)-1
## 1=DEFINE, 2=CONFIRM, 3=AFFIRM
X$STUDYID<-as.factor(X$STUDYID)
RiskData<-X

