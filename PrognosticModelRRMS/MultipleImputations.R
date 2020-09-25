###########################################################################################################
####################### Script that creates 10 imputed datasets, and checks for convergence ############
######################################################################################################

MISMSCdata<-MISMSCdata[which(!is.na(MISMSCdata$kfs.2)),] #in order to use this variable as auxiiary (we need it complete)

####################################### Multiple imputations ###########################################
#Let's organize our variable to be sure they have the correct type
#factors with 2 categories have to be inserted as numeric
MISMSCdata<-within(MISMSCdata, outcome<-as.numeric(outcome)-1)
MISMSCdata<-within(MISMSCdata, age<-as.numeric(age))
MISMSCdata<-within(MISMSCdata, disease.duration<-as.numeric(disease.duration))
MISMSCdata<-within(MISMSCdata, edss<-as.numeric(edss))
MISMSCdata<-within(MISMSCdata, nr.Gd.enhanced.lesions<-as.numeric(nr.Gd.enhanced.lesions)-1)
MISMSCdata<-within(MISMSCdata, months.since.last.relapse<-as.numeric(months.since.last.relapse))
MISMSCdata<-within(MISMSCdata, treatment.naive.prior.visit<-as.numeric(treatment.naive.prior.visit)-1)
MISMSCdata<-within(MISMSCdata, gender<-as.numeric(gender)-1)
MISMSCdata<-within(MISMSCdata, treatment.during.cycle<-as.numeric(treatment.during.cycle)-1)

#create dummy variables for categorical variables with more than 2 categories (required for jomo)
#nr.relapses
MISMSCdata$nr.relapses.2y.prior.study1<-MISMSCdata$nr.relapses.2y.prior.study
MISMSCdata$nr.relapses.2y.prior.study1[which(MISMSCdata$nr.relapses.2y.prior.study1==2)]<-0
MISMSCdata$nr.relapses.2y.prior.study1<-as.numeric(MISMSCdata$nr.relapses.2y.prior.study1)-1

MISMSCdata$nr.relapses.2y.prior.study2<-MISMSCdata$nr.relapses.2y.prior.study
MISMSCdata$nr.relapses.2y.prior.study2[which(MISMSCdata$nr.relapses.2y.prior.study2==1)]<-0
MISMSCdata$nr.relapses.2y.prior.study2[which(MISMSCdata$nr.relapses.2y.prior.study2==2)]<-1
MISMSCdata$nr.relapses.2y.prior.study2<-as.numeric(MISMSCdata$nr.relapses.2y.prior.study2)-1

#auxiliary variable kfs.2
MISMSCdata$kfs.21<-MISMSCdata$kfs.2
MISMSCdata$kfs.21[which(MISMSCdata$kfs.21==2)]<-0
MISMSCdata$kfs.21[which(MISMSCdata$kfs.21==3)]<-0
MISMSCdata$kfs.21<-as.numeric(MISMSCdata$kfs.21)-1

MISMSCdata$kfs.22<-MISMSCdata$kfs.2
MISMSCdata$kfs.22[which(MISMSCdata$kfs.22==1)]<-0
MISMSCdata$kfs.22[which(MISMSCdata$kfs.22==2)]<-1
MISMSCdata$kfs.22[which(MISMSCdata$kfs.22==3)]<-0
MISMSCdata$kfs.22<-as.numeric(MISMSCdata$kfs.22)-1

MISMSCdata$kfs.23<-MISMSCdata$kfs.2
MISMSCdata$kfs.23[which(MISMSCdata$kfs.23==1)]<-0
MISMSCdata$kfs.23[which(MISMSCdata$kfs.23==2)]<-0
MISMSCdata$kfs.23[which(MISMSCdata$kfs.23==3)]<-1
MISMSCdata$kfs.23<-as.numeric(MISMSCdata$kfs.23)-1

MISMSCdata$nr.Gd.enhanced.lesions<-MISMSCdata$nr.Gd.enhanced.lesions
MISMSCdata$nr.Gd.enhanced.lesions<-as.factor(MISMSCdata$nr.Gd.enhanced.lesions)
MISMSCdata$treatment.during.cycle<-as.factor(MISMSCdata$treatment.during.cycle)


#formula
fml<-disease.duration+months.since.last.relapse+nr.Gd.enhanced.lesions+treatment.during.cycle~ age+edss + nr.relapses.2y.prior.study1 +
  nr.relapses.2y.prior.study2+treatment.naive.prior.visit + gender+outcome+kfs.21+kfs.22+kfs.23+(age+nr.relapses.2y.prior.study1+nr.relapses.2y.prior.study2+treatment.naive.prior.visit+gender+outcome+kfs.21+kfs.22+kfs.23|patient.id) + (1|patient.id)
#+(nr.relapses.2y.prior.study2|patient.id)+treatment.naive.prior.visit+ (treatment.naive.prior.visit|patient.id) + gender+(gender|patient.id)+outcome+ (outcome|patient.id)+(1|patient.id)
#multiple imputations
set.seed(2000)
imp3<-jomoImpute(data=MISMSCdata,formula=fml,n.burn=1000, n.iter=1000, m=10, seed=1569) #25 minutes
summary(imp3)
#check the convergence
plot(imp3)
#imputed datasets
imp.list<-mitmlComplete(imp3,print = "all")
