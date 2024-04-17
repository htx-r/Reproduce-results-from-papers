###############################################################################
########## FUNCTION FOR adding the predicted risk to CDP for each patients    #########################
########################################################################## #######



dataset<-data_CDP_CC
X<-dataset

X$logitRisk<-predict(finalmodel.pen,new=X)
X$Risk<-exp(X$logitRisk)/(1+exp(X$logitRisk))

##make treatment and study id numeric for the prediction model
X$TRT01A<-as.numeric(X$TRT01A)
X$TRT01A<-recode(X$TRT01A, "2='1'; 4='2';5='3'; 7='4';")
###1= Dimethyl fumarate, 2=Glatiramer acetate , 3=Natalizumab, 4=Placebo
X$TRT01A<-as.factor(X$TRT01A)
#recode of studyid
X$STUDYID<-as.numeric(X$STUDYID)-1
## 1=DEFINE, 2=CONFIRM, 3=AFFIRM
X$STUDYID<-as.factor(X$STUDYID)
data_CDP_CC<-X

