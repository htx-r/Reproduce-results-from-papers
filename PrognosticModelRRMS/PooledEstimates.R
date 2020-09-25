
############################################################################################################
####################### Table of Bayesian results for each one of the imputed datasets ############
########################## and Pooled estimates via Rubin's rules #############################
##############################################################################################################



############################# Table of Bayesian results for each one of the imputed datasets   #####################

##complete cases (Bayesian)
int<-paste(round(SMSCjagsResults$BUGSoutput$summary[1,1],2), "(", round(SMSCjagsResults$BUGSoutput$summary[1,3],2), ",", round(SMSCjagsResults$BUGSoutput$summary[1,7],2), ")")
age<-paste(round(SMSCjagsResults$BUGSoutput$summary[2,1],2), "(", round(SMSCjagsResults$BUGSoutput$summary[2,3],2), ",", round(SMSCjagsResults$BUGSoutput$summary[2,7],2), ")")
disease_duration<-paste(round(SMSCjagsResults$BUGSoutput$summary[3,1],2), "(", round(SMSCjagsResults$BUGSoutput$summary[3,3],2), ",", round(SMSCjagsResults$BUGSoutput$summary[3,7],2), ")")
Edss<-paste(round(SMSCjagsResults$BUGSoutput$summary[4,1],2), "(", round(SMSCjagsResults$BUGSoutput$summary[4,3],2), ",", round(SMSCjagsResults$BUGSoutput$summary[4,7],2), ")")
nrGd_lesions<-paste(round(SMSCjagsResults$BUGSoutput$summary[5,1],2), "(", round(SMSCjagsResults$BUGSoutput$summary[5,3],2), ",", round(SMSCjagsResults$BUGSoutput$summary[5,7],2), ")")
PriorRelapses_1<-paste(round(SMSCjagsResults$BUGSoutput$summary[6,1],2), "(", round(SMSCjagsResults$BUGSoutput$summary[6,3],2), ",", round(SMSCjagsResults$BUGSoutput$summary[6,7],2), ")")
PriorRelapses_2<-paste(round(SMSCjagsResults$BUGSoutput$summary[7,1],2), "(", round(SMSCjagsResults$BUGSoutput$summary[7,3],2), ",", round(SMSCjagsResults$BUGSoutput$summary[7,7],2), ")")
MonthsLastRelapse<-paste(round(SMSCjagsResults$BUGSoutput$summary[8,1],2), "(", round(SMSCjagsResults$BUGSoutput$summary[8,3],2), ",", round(SMSCjagsResults$BUGSoutput$summary[8,7],2), ")")
TreatmentNaive<-paste(round(SMSCjagsResults$BUGSoutput$summary[9,1],2), "(", round(SMSCjagsResults$BUGSoutput$summary[9,3],2), ",", round(SMSCjagsResults$BUGSoutput$summary[9,7],2), ")")
Gender<-paste(round(SMSCjagsResults$BUGSoutput$summary[10,1],2), "(", round(SMSCjagsResults$BUGSoutput$summary[10,3],2), ",", round(SMSCjagsResults$BUGSoutput$summary[10,7],2), ")")
Treatment<-paste(round(SMSCjagsResults$BUGSoutput$summary[11,1],2), "(", round(SMSCjagsResults$BUGSoutput$summary[11,3],2), ",", round(SMSCjagsResults$BUGSoutput$summary[11,7],2), ")")
Rho<-paste(round(SMSCjagsResults$BUGSoutput$summary[13,1],2), "(", round(SMSCjagsResults$BUGSoutput$summary[13,3],2), ",", round(SMSCjagsResults$BUGSoutput$summary[13,7],2), ")")
Sigma<-paste(round(SMSCjagsResults$BUGSoutput$summary[14,1],2), "(", round(SMSCjagsResults$BUGSoutput$summary[14,3],2), ",", round(SMSCjagsResults$BUGSoutput$summary[14,7],2), ")")
complete_cases<-as.data.frame(rbind(int,age,disease_duration,Edss,nrGd_lesions,PriorRelapses_1,PriorRelapses_2,MonthsLastRelapse,
                                    TreatmentNaive,Gender,Treatment,Rho,Sigma))
#1st imputed dataset
int1<-paste(round(SMSCjagsResults1$BUGSoutput$summary[1,1],2), "(", round(SMSCjagsResults1$BUGSoutput$summary[1,3],2), ",", round(SMSCjagsResults1$BUGSoutput$summary[1,7],2), ")")
age1<-paste(round(SMSCjagsResults1$BUGSoutput$summary[2,1],2), "(", round(SMSCjagsResults1$BUGSoutput$summary[2,3],2), ",", round(SMSCjagsResults1$BUGSoutput$summary[2,7],2), ")")
disease_duration1<-paste(round(SMSCjagsResults1$BUGSoutput$summary[3,1],2), "(", round(SMSCjagsResults1$BUGSoutput$summary[3,3],2), ",", round(SMSCjagsResults1$BUGSoutput$summary[3,7],2), ")")
Edss1<-paste(round(SMSCjagsResults1$BUGSoutput$summary[4,1],2), "(", round(SMSCjagsResults1$BUGSoutput$summary[4,3],2), ",", round(SMSCjagsResults1$BUGSoutput$summary[4,7],2), ")")
nrGd_lesions1<-paste(round(SMSCjagsResults1$BUGSoutput$summary[5,1],2), "(", round(SMSCjagsResults1$BUGSoutput$summary[5,3],2), ",", round(SMSCjagsResults1$BUGSoutput$summary[5,7],2), ")")
PriorRelapses_11<-paste(round(SMSCjagsResults1$BUGSoutput$summary[6,1],2), "(", round(SMSCjagsResults1$BUGSoutput$summary[6,3],2), ",", round(SMSCjagsResults1$BUGSoutput$summary[6,7],2), ")")
PriorRelapses_21<-paste(round(SMSCjagsResults1$BUGSoutput$summary[7,1],2), "(", round(SMSCjagsResults1$BUGSoutput$summary[7,3],2), ",", round(SMSCjagsResults1$BUGSoutput$summary[7,7],2), ")")
MonthsLastRelapse1<-paste(round(SMSCjagsResults1$BUGSoutput$summary[8,1],2), "(", round(SMSCjagsResults1$BUGSoutput$summary[8,3],2), ",", round(SMSCjagsResults1$BUGSoutput$summary[8,7],2), ")")
TreatmentNaive1<-paste(round(SMSCjagsResults1$BUGSoutput$summary[9,1],2), "(", round(SMSCjagsResults1$BUGSoutput$summary[9,3],2), ",", round(SMSCjagsResults1$BUGSoutput$summary[9,7],2), ")")
Gender1<-paste(round(SMSCjagsResults1$BUGSoutput$summary[10,1],2), "(", round(SMSCjagsResults1$BUGSoutput$summary[10,3],2), ",", round(SMSCjagsResults1$BUGSoutput$summary[10,7],2), ")")
Treatment1<-paste(round(SMSCjagsResults1$BUGSoutput$summary[11,1],2), "(", round(SMSCjagsResults1$BUGSoutput$summary[11,3],2), ",", round(SMSCjagsResults1$BUGSoutput$summary[11,7],2), ")")
Rho1<-paste(round(SMSCjagsResults1$BUGSoutput$summary[13,1],2), "(", round(SMSCjagsResults1$BUGSoutput$summary[13,3],2), ",", round(SMSCjagsResults1$BUGSoutput$summary[13,7],2), ")")
Sigma1<-paste(round(SMSCjagsResults1$BUGSoutput$summary[14,1],2), "(", round(SMSCjagsResults1$BUGSoutput$summary[14,3],2), ",", round(SMSCjagsResults1$BUGSoutput$summary[14,7],2), ")")
ImputedDataset1<-as.data.frame(rbind(int1,age1,disease_duration1,Edss1,nrGd_lesions1,PriorRelapses_11,PriorRelapses_21,MonthsLastRelapse1,
                                     TreatmentNaive1,Gender1,Treatment1,Rho1,Sigma1))

#2nd imputed dataset
int2<-paste(round(SMSCjagsResults2$BUGSoutput$summary[1,1],2), "(", round(SMSCjagsResults2$BUGSoutput$summary[1,3],2), ",", round(SMSCjagsResults2$BUGSoutput$summary[1,7],2), ")")
age2<-paste(round(SMSCjagsResults2$BUGSoutput$summary[2,1],2), "(", round(SMSCjagsResults2$BUGSoutput$summary[2,3],2), ",", round(SMSCjagsResults2$BUGSoutput$summary[2,7],2), ")")
disease_duration2<-paste(round(SMSCjagsResults2$BUGSoutput$summary[3,1],2), "(", round(SMSCjagsResults2$BUGSoutput$summary[3,3],2), ",", round(SMSCjagsResults2$BUGSoutput$summary[3,7],2), ")")
Edss2<-paste(round(SMSCjagsResults2$BUGSoutput$summary[4,1],2), "(", round(SMSCjagsResults2$BUGSoutput$summary[4,3],2), ",", round(SMSCjagsResults2$BUGSoutput$summary[4,7],2), ")")
nrGd_lesions2<-paste(round(SMSCjagsResults2$BUGSoutput$summary[5,1],2), "(", round(SMSCjagsResults2$BUGSoutput$summary[5,3],2), ",", round(SMSCjagsResults2$BUGSoutput$summary[5,7],2), ")")
PriorRelapses_12<-paste(round(SMSCjagsResults2$BUGSoutput$summary[6,1],2), "(", round(SMSCjagsResults2$BUGSoutput$summary[6,3],2), ",", round(SMSCjagsResults2$BUGSoutput$summary[6,7],2), ")")
PriorRelapses_22<-paste(round(SMSCjagsResults2$BUGSoutput$summary[7,1],2), "(", round(SMSCjagsResults2$BUGSoutput$summary[7,3],2), ",", round(SMSCjagsResults2$BUGSoutput$summary[7,7],2), ")")
MonthsLastRelapse2<-paste(round(SMSCjagsResults2$BUGSoutput$summary[8,1],2), "(", round(SMSCjagsResults2$BUGSoutput$summary[8,3],2), ",", round(SMSCjagsResults2$BUGSoutput$summary[8,7],2), ")")
TreatmentNaive2<-paste(round(SMSCjagsResults2$BUGSoutput$summary[9,1],2), "(", round(SMSCjagsResults2$BUGSoutput$summary[9,3],2), ",", round(SMSCjagsResults2$BUGSoutput$summary[9,7],2), ")")
Gender2<-paste(round(SMSCjagsResults2$BUGSoutput$summary[10,1],2), "(", round(SMSCjagsResults2$BUGSoutput$summary[10,3],2), ",", round(SMSCjagsResults2$BUGSoutput$summary[10,7],2), ")")
Treatment2<-paste(round(SMSCjagsResults2$BUGSoutput$summary[11,1],2), "(", round(SMSCjagsResults2$BUGSoutput$summary[11,3],2), ",", round(SMSCjagsResults2$BUGSoutput$summary[11,7],2), ")")
Rho2<-paste(round(SMSCjagsResults2$BUGSoutput$summary[13,1],2), "(", round(SMSCjagsResults2$BUGSoutput$summary[13,3],2), ",", round(SMSCjagsResults2$BUGSoutput$summary[13,7],2), ")")
Sigma2<-paste(round(SMSCjagsResults2$BUGSoutput$summary[14,1],2), "(", round(SMSCjagsResults2$BUGSoutput$summary[14,3],2), ",", round(SMSCjagsResults2$BUGSoutput$summary[14,7],2), ")")
ImputedDataset2<-as.data.frame(rbind(int2,age2,disease_duration2,Edss2,nrGd_lesions2,PriorRelapses_12,PriorRelapses_22,MonthsLastRelapse2,
                                     TreatmentNaive2,Gender2,Treatment2,Rho2,Sigma2))
#3rd imputed dataset
int3<-paste(round(SMSCjagsResults3$BUGSoutput$summary[1,1],2), "(", round(SMSCjagsResults3$BUGSoutput$summary[1,3],2), ",", round(SMSCjagsResults3$BUGSoutput$summary[1,7],2), ")")
age3<-paste(round(SMSCjagsResults3$BUGSoutput$summary[2,1],2), "(", round(SMSCjagsResults3$BUGSoutput$summary[2,3],2), ",", round(SMSCjagsResults3$BUGSoutput$summary[2,7],2), ")")
disease_duration3<-paste(round(SMSCjagsResults3$BUGSoutput$summary[3,1],2), "(", round(SMSCjagsResults3$BUGSoutput$summary[3,3],2), ",", round(SMSCjagsResults3$BUGSoutput$summary[3,7],2), ")")
Edss3<-paste(round(SMSCjagsResults3$BUGSoutput$summary[4,1],2), "(", round(SMSCjagsResults3$BUGSoutput$summary[4,3],2), ",", round(SMSCjagsResults3$BUGSoutput$summary[4,7],2), ")")
nrGd_lesions3<-paste(round(SMSCjagsResults3$BUGSoutput$summary[5,1],2), "(", round(SMSCjagsResults3$BUGSoutput$summary[5,3],2), ",", round(SMSCjagsResults3$BUGSoutput$summary[5,7],2), ")")
PriorRelapses_13<-paste(round(SMSCjagsResults3$BUGSoutput$summary[6,1],2), "(", round(SMSCjagsResults3$BUGSoutput$summary[6,3],2), ",", round(SMSCjagsResults3$BUGSoutput$summary[6,7],2), ")")
PriorRelapses_23<-paste(round(SMSCjagsResults3$BUGSoutput$summary[7,1],2), "(", round(SMSCjagsResults3$BUGSoutput$summary[7,3],2), ",", round(SMSCjagsResults3$BUGSoutput$summary[7,7],2), ")")
MonthsLastRelapse3<-paste(round(SMSCjagsResults3$BUGSoutput$summary[8,1],2), "(", round(SMSCjagsResults3$BUGSoutput$summary[8,3],2), ",", round(SMSCjagsResults3$BUGSoutput$summary[8,7],2), ")")
TreatmentNaive3<-paste(round(SMSCjagsResults3$BUGSoutput$summary[9,1],2), "(", round(SMSCjagsResults3$BUGSoutput$summary[9,3],2), ",", round(SMSCjagsResults3$BUGSoutput$summary[9,7],2), ")")
Gender3<-paste(round(SMSCjagsResults3$BUGSoutput$summary[10,1],2), "(", round(SMSCjagsResults3$BUGSoutput$summary[10,3],2), ",", round(SMSCjagsResults3$BUGSoutput$summary[10,7],2), ")")
Treatment3<-paste(round(SMSCjagsResults3$BUGSoutput$summary[11,1],2), "(", round(SMSCjagsResults3$BUGSoutput$summary[11,3],2), ",", round(SMSCjagsResults3$BUGSoutput$summary[11,7],2), ")")
Rho3<-paste(round(SMSCjagsResults3$BUGSoutput$summary[13,1],2), "(", round(SMSCjagsResults3$BUGSoutput$summary[13,3],2), ",", round(SMSCjagsResults3$BUGSoutput$summary[13,7],2), ")")
Sigma3<-paste(round(SMSCjagsResults3$BUGSoutput$summary[14,1],2), "(", round(SMSCjagsResults3$BUGSoutput$summary[14,3],2), ",", round(SMSCjagsResults3$BUGSoutput$summary[14,7],2), ")")
ImputedDataset3<-as.data.frame(rbind(int3,age3,disease_duration3,Edss3,nrGd_lesions3,PriorRelapses_13,PriorRelapses_23,MonthsLastRelapse3,
                                     TreatmentNaive3,Gender3,Treatment3,Rho3,Sigma3))
#4th imputed dataset
int4<-paste(round(SMSCjagsResults4$BUGSoutput$summary[1,1],2), "(", round(SMSCjagsResults4$BUGSoutput$summary[1,3],2), ",", round(SMSCjagsResults4$BUGSoutput$summary[1,7],2), ")")
age4<-paste(round(SMSCjagsResults4$BUGSoutput$summary[2,1],2), "(", round(SMSCjagsResults4$BUGSoutput$summary[2,3],2), ",", round(SMSCjagsResults4$BUGSoutput$summary[2,7],2), ")")
disease_duration4<-paste(round(SMSCjagsResults4$BUGSoutput$summary[3,1],2), "(", round(SMSCjagsResults4$BUGSoutput$summary[3,3],2), ",", round(SMSCjagsResults4$BUGSoutput$summary[3,7],2), ")")
Edss4<-paste(round(SMSCjagsResults4$BUGSoutput$summary[4,1],2), "(", round(SMSCjagsResults4$BUGSoutput$summary[4,3],2), ",", round(SMSCjagsResults4$BUGSoutput$summary[4,7],2), ")")
nrGd_lesions4<-paste(round(SMSCjagsResults4$BUGSoutput$summary[5,1],2), "(", round(SMSCjagsResults4$BUGSoutput$summary[5,3],2), ",", round(SMSCjagsResults4$BUGSoutput$summary[5,7],2), ")")
PriorRelapses_14<-paste(round(SMSCjagsResults4$BUGSoutput$summary[6,1],2), "(", round(SMSCjagsResults4$BUGSoutput$summary[6,3],2), ",", round(SMSCjagsResults4$BUGSoutput$summary[6,7],2), ")")
PriorRelapses_24<-paste(round(SMSCjagsResults4$BUGSoutput$summary[7,1],2), "(", round(SMSCjagsResults4$BUGSoutput$summary[7,3],2), ",", round(SMSCjagsResults4$BUGSoutput$summary[7,7],2), ")")
MonthsLastRelapse4<-paste(round(SMSCjagsResults4$BUGSoutput$summary[8,1],2), "(", round(SMSCjagsResults4$BUGSoutput$summary[8,3],2), ",", round(SMSCjagsResults4$BUGSoutput$summary[8,7],2), ")")
TreatmentNaive4<-paste(round(SMSCjagsResults4$BUGSoutput$summary[9,1],2), "(", round(SMSCjagsResults4$BUGSoutput$summary[9,3],2), ",", round(SMSCjagsResults4$BUGSoutput$summary[9,7],2), ")")
Gender4<-paste(round(SMSCjagsResults4$BUGSoutput$summary[10,1],2), "(", round(SMSCjagsResults4$BUGSoutput$summary[10,3],2), ",", round(SMSCjagsResults4$BUGSoutput$summary[10,7],2), ")")
Treatment4<-paste(round(SMSCjagsResults4$BUGSoutput$summary[11,1],2), "(", round(SMSCjagsResults4$BUGSoutput$summary[11,3],2), ",", round(SMSCjagsResults4$BUGSoutput$summary[11,7],2), ")")
Rho4<-paste(round(SMSCjagsResults4$BUGSoutput$summary[13,1],2), "(", round(SMSCjagsResults4$BUGSoutput$summary[13,3],2), ",", round(SMSCjagsResults4$BUGSoutput$summary[13,7],2), ")")
Sigma4<-paste(round(SMSCjagsResults4$BUGSoutput$summary[14,1],2), "(", round(SMSCjagsResults4$BUGSoutput$summary[14,3],2), ",", round(SMSCjagsResults4$BUGSoutput$summary[14,7],2), ")")
ImputedDataset4<-as.data.frame(rbind(int4,age4,disease_duration4,Edss4,nrGd_lesions4,PriorRelapses_14,PriorRelapses_24,MonthsLastRelapse4,
                                     TreatmentNaive4,Gender4, Treatment4, Rho4,Sigma4))
#5th imputed dataset
int5<-paste(round(SMSCjagsResults5$BUGSoutput$summary[1,1],2), "(", round(SMSCjagsResults5$BUGSoutput$summary[1,3],2), ",", round(SMSCjagsResults5$BUGSoutput$summary[1,7],2), ")")
age5<-paste(round(SMSCjagsResults5$BUGSoutput$summary[2,1],2), "(", round(SMSCjagsResults5$BUGSoutput$summary[2,3],2), ",", round(SMSCjagsResults5$BUGSoutput$summary[2,7],2), ")")
disease_duration5<-paste(round(SMSCjagsResults5$BUGSoutput$summary[3,1],2), "(", round(SMSCjagsResults5$BUGSoutput$summary[3,3],2), ",", round(SMSCjagsResults5$BUGSoutput$summary[3,7],2), ")")
Edss5<-paste(round(SMSCjagsResults5$BUGSoutput$summary[4,1],2), "(", round(SMSCjagsResults5$BUGSoutput$summary[4,3],2), ",", round(SMSCjagsResults5$BUGSoutput$summary[4,7],2), ")")
nrGd_lesions5<-paste(round(SMSCjagsResults5$BUGSoutput$summary[5,1],2), "(", round(SMSCjagsResults5$BUGSoutput$summary[5,3],2), ",", round(SMSCjagsResults5$BUGSoutput$summary[5,7],2), ")")
PriorRelapses_15<-paste(round(SMSCjagsResults5$BUGSoutput$summary[6,1],2), "(", round(SMSCjagsResults5$BUGSoutput$summary[6,3],2), ",", round(SMSCjagsResults5$BUGSoutput$summary[6,7],2), ")")
PriorRelapses_25<-paste(round(SMSCjagsResults5$BUGSoutput$summary[7,1],2), "(", round(SMSCjagsResults5$BUGSoutput$summary[7,3],2), ",", round(SMSCjagsResults5$BUGSoutput$summary[7,7],2), ")")
MonthsLastRelapse5<-paste(round(SMSCjagsResults5$BUGSoutput$summary[8,1],2), "(", round(SMSCjagsResults5$BUGSoutput$summary[8,3],2), ",", round(SMSCjagsResults5$BUGSoutput$summary[8,7],2), ")")
TreatmentNaive5<-paste(round(SMSCjagsResults5$BUGSoutput$summary[9,1],2), "(", round(SMSCjagsResults5$BUGSoutput$summary[9,3],2), ",", round(SMSCjagsResults5$BUGSoutput$summary[9,7],2), ")")
Gender5<-paste(round(SMSCjagsResults5$BUGSoutput$summary[10,1],2), "(", round(SMSCjagsResults5$BUGSoutput$summary[10,3],2), ",", round(SMSCjagsResults5$BUGSoutput$summary[10,7],2), ")")
Treatment5<-paste(round(SMSCjagsResults5$BUGSoutput$summary[11,1],2), "(", round(SMSCjagsResults5$BUGSoutput$summary[11,3],2), ",", round(SMSCjagsResults5$BUGSoutput$summary[11,7],2), ")")
Rho5<-paste(round(SMSCjagsResults5$BUGSoutput$summary[13,1],2), "(", round(SMSCjagsResults5$BUGSoutput$summary[13,3],2), ",", round(SMSCjagsResults5$BUGSoutput$summary[13,7],2), ")")
Sigma5<-paste(round(SMSCjagsResults5$BUGSoutput$summary[14,1],2), "(", round(SMSCjagsResults5$BUGSoutput$summary[14,3],2), ",", round(SMSCjagsResults5$BUGSoutput$summary[14,7],2), ")")
ImputedDataset5<-as.data.frame(rbind(int5,age5,disease_duration5,Edss5,nrGd_lesions5,PriorRelapses_15,PriorRelapses_25,MonthsLastRelapse5,
                                     TreatmentNaive5,Gender5,Treatment5, Rho5,Sigma5))
#6th imputed dataset
int6<-paste(round(SMSCjagsResults6$BUGSoutput$summary[1,1],2), "(", round(SMSCjagsResults6$BUGSoutput$summary[1,3],2), ",", round(SMSCjagsResults6$BUGSoutput$summary[1,7],2), ")")
age6<-paste(round(SMSCjagsResults6$BUGSoutput$summary[2,1],2), "(", round(SMSCjagsResults6$BUGSoutput$summary[2,3],2), ",", round(SMSCjagsResults6$BUGSoutput$summary[2,7],2), ")")
disease_duration6<-paste(round(SMSCjagsResults6$BUGSoutput$summary[3,1],2), "(", round(SMSCjagsResults6$BUGSoutput$summary[3,3],2), ",", round(SMSCjagsResults6$BUGSoutput$summary[3,7],2), ")")
Edss6<-paste(round(SMSCjagsResults6$BUGSoutput$summary[4,1],2), "(", round(SMSCjagsResults6$BUGSoutput$summary[4,3],2), ",", round(SMSCjagsResults6$BUGSoutput$summary[4,7],2), ")")
nrGd_lesions6<-paste(round(SMSCjagsResults6$BUGSoutput$summary[5,1],2), "(", round(SMSCjagsResults6$BUGSoutput$summary[5,3],2), ",", round(SMSCjagsResults6$BUGSoutput$summary[5,7],2), ")")
PriorRelapses_16<-paste(round(SMSCjagsResults6$BUGSoutput$summary[6,1],2), "(", round(SMSCjagsResults6$BUGSoutput$summary[6,3],2), ",", round(SMSCjagsResults6$BUGSoutput$summary[6,7],2), ")")
PriorRelapses_26<-paste(round(SMSCjagsResults6$BUGSoutput$summary[7,1],2), "(", round(SMSCjagsResults6$BUGSoutput$summary[7,3],2), ",", round(SMSCjagsResults6$BUGSoutput$summary[7,7],2), ")")
MonthsLastRelapse6<-paste(round(SMSCjagsResults6$BUGSoutput$summary[8,1],2), "(", round(SMSCjagsResults6$BUGSoutput$summary[8,3],2), ",", round(SMSCjagsResults6$BUGSoutput$summary[8,7],2), ")")
TreatmentNaive6<-paste(round(SMSCjagsResults6$BUGSoutput$summary[9,1],2), "(", round(SMSCjagsResults6$BUGSoutput$summary[9,3],2), ",", round(SMSCjagsResults6$BUGSoutput$summary[9,7],2), ")")
Gender6<-paste(round(SMSCjagsResults6$BUGSoutput$summary[10,1],2), "(", round(SMSCjagsResults6$BUGSoutput$summary[10,3],2), ",", round(SMSCjagsResults6$BUGSoutput$summary[10,7],2), ")")
Treatment6<-paste(round(SMSCjagsResults6$BUGSoutput$summary[11,1],2), "(", round(SMSCjagsResults6$BUGSoutput$summary[11,3],2), ",", round(SMSCjagsResults6$BUGSoutput$summary[11,7],2), ")")
Rho6<-paste(round(SMSCjagsResults6$BUGSoutput$summary[13,1],2), "(", round(SMSCjagsResults6$BUGSoutput$summary[13,3],2), ",", round(SMSCjagsResults6$BUGSoutput$summary[13,7],2), ")")
Sigma6<-paste(round(SMSCjagsResults6$BUGSoutput$summary[14,1],2), "(", round(SMSCjagsResults6$BUGSoutput$summary[14,3],2), ",", round(SMSCjagsResults6$BUGSoutput$summary[14,7],2), ")")
ImputedDataset6<-as.data.frame(rbind(int6,age6,disease_duration6,Edss6,nrGd_lesions6,PriorRelapses_16,PriorRelapses_26,MonthsLastRelapse6,
                                     TreatmentNaive6,Gender6,Treatment6,Rho6,Sigma6))
#7th imputed dataset
int7<-paste(round(SMSCjagsResults7$BUGSoutput$summary[1,1],2), "(", round(SMSCjagsResults7$BUGSoutput$summary[1,3],2), ",", round(SMSCjagsResults7$BUGSoutput$summary[1,7],2), ")")
age7<-paste(round(SMSCjagsResults7$BUGSoutput$summary[2,1],2), "(", round(SMSCjagsResults7$BUGSoutput$summary[2,3],2), ",", round(SMSCjagsResults7$BUGSoutput$summary[2,7],2), ")")
disease_duration7<-paste(round(SMSCjagsResults7$BUGSoutput$summary[3,1],2), "(", round(SMSCjagsResults7$BUGSoutput$summary[3,3],2), ",", round(SMSCjagsResults7$BUGSoutput$summary[3,7],2), ")")
Edss7<-paste(round(SMSCjagsResults7$BUGSoutput$summary[4,1],2), "(", round(SMSCjagsResults7$BUGSoutput$summary[4,3],2), ",", round(SMSCjagsResults7$BUGSoutput$summary[4,7],2), ")")
nrGd_lesions7<-paste(round(SMSCjagsResults7$BUGSoutput$summary[5,1],2), "(", round(SMSCjagsResults7$BUGSoutput$summary[5,3],2), ",", round(SMSCjagsResults7$BUGSoutput$summary[5,7],2), ")")
PriorRelapses_17<-paste(round(SMSCjagsResults7$BUGSoutput$summary[6,1],2), "(", round(SMSCjagsResults7$BUGSoutput$summary[6,3],2), ",", round(SMSCjagsResults7$BUGSoutput$summary[6,7],2), ")")
PriorRelapses_27<-paste(round(SMSCjagsResults7$BUGSoutput$summary[7,1],2), "(", round(SMSCjagsResults7$BUGSoutput$summary[7,3],2), ",", round(SMSCjagsResults7$BUGSoutput$summary[7,7],2), ")")
MonthsLastRelapse7<-paste(round(SMSCjagsResults7$BUGSoutput$summary[8,1],2), "(", round(SMSCjagsResults7$BUGSoutput$summary[8,3],2), ",", round(SMSCjagsResults7$BUGSoutput$summary[8,7],2), ")")
TreatmentNaive7<-paste(round(SMSCjagsResults7$BUGSoutput$summary[9,1],2), "(", round(SMSCjagsResults7$BUGSoutput$summary[9,3],2), ",", round(SMSCjagsResults7$BUGSoutput$summary[9,7],2), ")")
Gender7<-paste(round(SMSCjagsResults7$BUGSoutput$summary[10,1],2), "(", round(SMSCjagsResults7$BUGSoutput$summary[10,3],2), ",", round(SMSCjagsResults7$BUGSoutput$summary[10,7],2), ")")
Treatment7<-paste(round(SMSCjagsResults7$BUGSoutput$summary[11,1],2), "(", round(SMSCjagsResults7$BUGSoutput$summary[11,3],2), ",", round(SMSCjagsResults7$BUGSoutput$summary[11,7],2), ")")
Rho7<-paste(round(SMSCjagsResults7$BUGSoutput$summary[13,1],2), "(", round(SMSCjagsResults7$BUGSoutput$summary[13,3],2), ",", round(SMSCjagsResults7$BUGSoutput$summary[13,7],2), ")")
Sigma7<-paste(round(SMSCjagsResults7$BUGSoutput$summary[14,1],2), "(", round(SMSCjagsResults7$BUGSoutput$summary[14,3],2), ",", round(SMSCjagsResults7$BUGSoutput$summary[14,7],2), ")")
ImputedDataset7<-as.data.frame(rbind(int7,age7,disease_duration7,Edss7,nrGd_lesions7,PriorRelapses_17,PriorRelapses_27,MonthsLastRelapse7,
                                     TreatmentNaive7,Gender7, Treatment7, Rho7,Sigma7))
#8th imputed dataset
int8<-paste(round(SMSCjagsResults8$BUGSoutput$summary[1,1],2), "(", round(SMSCjagsResults8$BUGSoutput$summary[1,3],2), ",", round(SMSCjagsResults8$BUGSoutput$summary[1,7],2), ")")
age8<-paste(round(SMSCjagsResults8$BUGSoutput$summary[2,1],2), "(", round(SMSCjagsResults8$BUGSoutput$summary[2,3],2), ",", round(SMSCjagsResults8$BUGSoutput$summary[2,7],2), ")")
disease_duration8<-paste(round(SMSCjagsResults8$BUGSoutput$summary[3,1],2), "(", round(SMSCjagsResults8$BUGSoutput$summary[3,3],2), ",", round(SMSCjagsResults8$BUGSoutput$summary[3,7],2), ")")
Edss8<-paste(round(SMSCjagsResults8$BUGSoutput$summary[4,1],2), "(", round(SMSCjagsResults8$BUGSoutput$summary[4,3],2), ",", round(SMSCjagsResults8$BUGSoutput$summary[4,7],2), ")")
nrGd_lesions8<-paste(round(SMSCjagsResults8$BUGSoutput$summary[5,1],2), "(", round(SMSCjagsResults8$BUGSoutput$summary[5,3],2), ",", round(SMSCjagsResults8$BUGSoutput$summary[5,7],2), ")")
PriorRelapses_18<-paste(round(SMSCjagsResults8$BUGSoutput$summary[6,1],2), "(", round(SMSCjagsResults8$BUGSoutput$summary[6,3],2), ",", round(SMSCjagsResults8$BUGSoutput$summary[6,7],2), ")")
PriorRelapses_28<-paste(round(SMSCjagsResults8$BUGSoutput$summary[7,1],2), "(", round(SMSCjagsResults8$BUGSoutput$summary[7,3],2), ",", round(SMSCjagsResults8$BUGSoutput$summary[7,7],2), ")")
MonthsLastRelapse8<-paste(round(SMSCjagsResults8$BUGSoutput$summary[8,1],2), "(", round(SMSCjagsResults8$BUGSoutput$summary[8,3],2), ",", round(SMSCjagsResults8$BUGSoutput$summary[8,7],2), ")")
TreatmentNaive8<-paste(round(SMSCjagsResults8$BUGSoutput$summary[9,1],2), "(", round(SMSCjagsResults8$BUGSoutput$summary[9,3],2), ",", round(SMSCjagsResults8$BUGSoutput$summary[9,7],2), ")")
Gender8<-paste(round(SMSCjagsResults8$BUGSoutput$summary[10,1],2), "(", round(SMSCjagsResults8$BUGSoutput$summary[10,3],2), ",", round(SMSCjagsResults8$BUGSoutput$summary[10,7],2), ")")
Treatment8<-paste(round(SMSCjagsResults8$BUGSoutput$summary[11,1],2), "(", round(SMSCjagsResults8$BUGSoutput$summary[11,3],2), ",", round(SMSCjagsResults8$BUGSoutput$summary[11,7],2), ")")
Rho8<-paste(round(SMSCjagsResults8$BUGSoutput$summary[13,1],2), "(", round(SMSCjagsResults8$BUGSoutput$summary[13,3],2), ",", round(SMSCjagsResults8$BUGSoutput$summary[13,7],2), ")")
Sigma8<-paste(round(SMSCjagsResults8$BUGSoutput$summary[14,1],2), "(", round(SMSCjagsResults8$BUGSoutput$summary[14,3],2), ",", round(SMSCjagsResults8$BUGSoutput$summary[14,7],2), ")")
ImputedDataset8<-as.data.frame(rbind(int8,age8,disease_duration8,Edss8,nrGd_lesions8,PriorRelapses_18,PriorRelapses_28,MonthsLastRelapse8,
                                     TreatmentNaive8,Gender8, Treatment8, Rho8,Sigma8))
#9th imputed dataset
int9<-paste(round(SMSCjagsResults9$BUGSoutput$summary[1,1],2), "(", round(SMSCjagsResults9$BUGSoutput$summary[1,3],2), ",", round(SMSCjagsResults9$BUGSoutput$summary[1,7],2), ")")
age9<-paste(round(SMSCjagsResults9$BUGSoutput$summary[2,1],2), "(", round(SMSCjagsResults9$BUGSoutput$summary[2,3],2), ",", round(SMSCjagsResults9$BUGSoutput$summary[2,7],2), ")")
disease_duration9<-paste(round(SMSCjagsResults9$BUGSoutput$summary[3,1],2), "(", round(SMSCjagsResults9$BUGSoutput$summary[3,3],2), ",", round(SMSCjagsResults9$BUGSoutput$summary[3,7],2), ")")
Edss9<-paste(round(SMSCjagsResults9$BUGSoutput$summary[4,1],2), "(", round(SMSCjagsResults9$BUGSoutput$summary[4,3],2), ",", round(SMSCjagsResults9$BUGSoutput$summary[4,7],2), ")")
nrGd_lesions9<-paste(round(SMSCjagsResults9$BUGSoutput$summary[5,1],2), "(", round(SMSCjagsResults9$BUGSoutput$summary[5,3],2), ",", round(SMSCjagsResults9$BUGSoutput$summary[5,7],2), ")")
PriorRelapses_19<-paste(round(SMSCjagsResults9$BUGSoutput$summary[6,1],2), "(", round(SMSCjagsResults9$BUGSoutput$summary[6,3],2), ",", round(SMSCjagsResults9$BUGSoutput$summary[6,7],2), ")")
PriorRelapses_29<-paste(round(SMSCjagsResults9$BUGSoutput$summary[7,1],2), "(", round(SMSCjagsResults9$BUGSoutput$summary[7,3],2), ",", round(SMSCjagsResults9$BUGSoutput$summary[7,7],2), ")")
MonthsLastRelapse9<-paste(round(SMSCjagsResults9$BUGSoutput$summary[8,1],2), "(", round(SMSCjagsResults9$BUGSoutput$summary[8,3],2), ",", round(SMSCjagsResults9$BUGSoutput$summary[8,7],2), ")")
TreatmentNaive9<-paste(round(SMSCjagsResults9$BUGSoutput$summary[9,1],2), "(", round(SMSCjagsResults9$BUGSoutput$summary[9,3],2), ",", round(SMSCjagsResults9$BUGSoutput$summary[9,7],2), ")")
Gender9<-paste(round(SMSCjagsResults9$BUGSoutput$summary[10,1],2), "(", round(SMSCjagsResults9$BUGSoutput$summary[10,3],2), ",", round(SMSCjagsResults9$BUGSoutput$summary[10,7],2), ")")
Treatment9<-paste(round(SMSCjagsResults9$BUGSoutput$summary[11,1],2), "(", round(SMSCjagsResults9$BUGSoutput$summary[11,3],2), ",", round(SMSCjagsResults9$BUGSoutput$summary[11,7],2), ")")
Rho9<-paste(round(SMSCjagsResults9$BUGSoutput$summary[13,1],2), "(", round(SMSCjagsResults9$BUGSoutput$summary[13,3],2), ",", round(SMSCjagsResults9$BUGSoutput$summary[13,7],2), ")")
Sigma9<-paste(round(SMSCjagsResults9$BUGSoutput$summary[14,1],2), "(", round(SMSCjagsResults9$BUGSoutput$summary[14,3],2), ",", round(SMSCjagsResults9$BUGSoutput$summary[14,7],2), ")")
ImputedDataset9<-as.data.frame(rbind(int9,age9,disease_duration9,Edss9,nrGd_lesions9,PriorRelapses_19,PriorRelapses_29,MonthsLastRelapse9,
                                     TreatmentNaive9,Gender9,Treatment9, Rho9,Sigma9))
#10th imputed dataset
int10<-paste(round(SMSCjagsResults10$BUGSoutput$summary[1,1],2), "(", round(SMSCjagsResults10$BUGSoutput$summary[1,3],2), ",", round(SMSCjagsResults10$BUGSoutput$summary[1,7],2), ")")
age10<-paste(round(SMSCjagsResults10$BUGSoutput$summary[2,1],2), "(", round(SMSCjagsResults10$BUGSoutput$summary[2,3],2), ",", round(SMSCjagsResults10$BUGSoutput$summary[2,7],2), ")")
disease_duration10<-paste(round(SMSCjagsResults10$BUGSoutput$summary[3,1],2), "(", round(SMSCjagsResults10$BUGSoutput$summary[3,3],2), ",", round(SMSCjagsResults10$BUGSoutput$summary[3,7],2), ")")
Edss10<-paste(round(SMSCjagsResults10$BUGSoutput$summary[4,1],2), "(", round(SMSCjagsResults10$BUGSoutput$summary[4,3],2), ",", round(SMSCjagsResults10$BUGSoutput$summary[4,7],2), ")")
nrGd_lesions10<-paste(round(SMSCjagsResults10$BUGSoutput$summary[5,1],2), "(", round(SMSCjagsResults10$BUGSoutput$summary[5,3],2), ",", round(SMSCjagsResults10$BUGSoutput$summary[5,7],2), ")")
PriorRelapses_110<-paste(round(SMSCjagsResults10$BUGSoutput$summary[6,1],2), "(", round(SMSCjagsResults10$BUGSoutput$summary[6,3],2), ",", round(SMSCjagsResults10$BUGSoutput$summary[6,7],2), ")")
PriorRelapses_210<-paste(round(SMSCjagsResults10$BUGSoutput$summary[7,1],2), "(", round(SMSCjagsResults10$BUGSoutput$summary[7,3],2), ",", round(SMSCjagsResults10$BUGSoutput$summary[7,7],2), ")")
MonthsLastRelapse10<-paste(round(SMSCjagsResults10$BUGSoutput$summary[8,1],2), "(", round(SMSCjagsResults10$BUGSoutput$summary[8,3],2), ",", round(SMSCjagsResults10$BUGSoutput$summary[8,7],2), ")")
TreatmentNaive10<-paste(round(SMSCjagsResults10$BUGSoutput$summary[9,1],2), "(", round(SMSCjagsResults10$BUGSoutput$summary[9,3],2), ",", round(SMSCjagsResults10$BUGSoutput$summary[9,7],2), ")")
Gender10<-paste(round(SMSCjagsResults10$BUGSoutput$summary[10,1],2), "(", round(SMSCjagsResults10$BUGSoutput$summary[10,3],2), ",", round(SMSCjagsResults10$BUGSoutput$summary[10,7],2), ")")
Treatment10<-paste(round(SMSCjagsResults10$BUGSoutput$summary[11,1],2), "(", round(SMSCjagsResults10$BUGSoutput$summary[11,3],2), ",", round(SMSCjagsResults10$BUGSoutput$summary[11,7],2), ")")
Rho10<-paste(round(SMSCjagsResults10$BUGSoutput$summary[13,1],2), "(", round(SMSCjagsResults10$BUGSoutput$summary[13,3],2), ",", round(SMSCjagsResults10$BUGSoutput$summary[13,7],2), ")")
Sigma10<-paste(round(SMSCjagsResults10$BUGSoutput$summary[14,1],2), "(", round(SMSCjagsResults10$BUGSoutput$summary[14,3],2), ",", round(SMSCjagsResults10$BUGSoutput$summary[14,7],2), ")")
ImputedDataset10<-as.data.frame(rbind(int10,age10,disease_duration10,Edss10,nrGd_lesions10,PriorRelapses_110,PriorRelapses_210,MonthsLastRelapse10,
                                      TreatmentNaive10,Gender10,Treatment10,Rho10,Sigma10))

tablewithresults<-as.data.frame(cbind(complete_cases, ImputedDataset1,ImputedDataset2,ImputedDataset3,ImputedDataset4,ImputedDataset5,ImputedDataset6,ImputedDataset7,ImputedDataset8,ImputedDataset9,ImputedDataset10))
#write.xlsx(tablewithresults, "C:/Users/kc19o338/Desktop/Real world predictions project/NMAPedictionsRiskModel/ImputedResults2.xlsx")


############################# Pooled estimates via the Rubin's rules   #################################

#estimates

intercept1<-SMSCjagsResults1$BUGSoutput$summary[1,1]
intercept2<-SMSCjagsResults2$BUGSoutput$summary[1,1]
intercept3<-SMSCjagsResults3$BUGSoutput$summary[1,1]
intercept4<-SMSCjagsResults4$BUGSoutput$summary[1,1]
intercept5<-SMSCjagsResults5$BUGSoutput$summary[1,1]
intercept6<-SMSCjagsResults6$BUGSoutput$summary[1,1]
intercept7<-SMSCjagsResults7$BUGSoutput$summary[1,1]
intercept8<-SMSCjagsResults8$BUGSoutput$summary[1,1]
intercept9<-SMSCjagsResults9$BUGSoutput$summary[1,1]
intercept10<-SMSCjagsResults10$BUGSoutput$summary[1,1]
x<-as.matrix(cbind(intercept1,intercept2,intercept3,intercept4,intercept5,intercept6,intercept7,intercept8,intercept9, intercept10),colnames=NA)
colnames(x)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(x)<-c("Intercept")


age1<-SMSCjagsResults1$BUGSoutput$summary[2,1]
age2<-SMSCjagsResults2$BUGSoutput$summary[2,1]
age3<-SMSCjagsResults3$BUGSoutput$summary[2,1]
age4<-SMSCjagsResults4$BUGSoutput$summary[2,1]
age5<-SMSCjagsResults5$BUGSoutput$summary[2,1]
age6<-SMSCjagsResults6$BUGSoutput$summary[2,1]
age7<-SMSCjagsResults7$BUGSoutput$summary[2,1]
age8<-SMSCjagsResults8$BUGSoutput$summary[2,1]
age9<-SMSCjagsResults9$BUGSoutput$summary[2,1]
age10<-SMSCjagsResults10$BUGSoutput$summary[2,1]
age<-as.matrix(cbind(age1,age2,age3,age4,age5,age6,age7,age8,age9, age10),colnames=NA)
colnames(age)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(age)<-c("age")

DiseaseDuration1<-SMSCjagsResults1$BUGSoutput$summary[3,1]
DiseaseDuration2<-SMSCjagsResults2$BUGSoutput$summary[3,1]
DiseaseDuration3<-SMSCjagsResults3$BUGSoutput$summary[3,1]
DiseaseDuration4<-SMSCjagsResults4$BUGSoutput$summary[3,1]
DiseaseDuration5<-SMSCjagsResults5$BUGSoutput$summary[3,1]
DiseaseDuration6<-SMSCjagsResults6$BUGSoutput$summary[3,1]
DiseaseDuration7<-SMSCjagsResults7$BUGSoutput$summary[3,1]
DiseaseDuration8<-SMSCjagsResults8$BUGSoutput$summary[3,1]
DiseaseDuration9<-SMSCjagsResults9$BUGSoutput$summary[3,1]
DiseaseDuration10<-SMSCjagsResults10$BUGSoutput$summary[3,1]
DiseaseDuration<-as.matrix(cbind(DiseaseDuration1,DiseaseDuration2,DiseaseDuration3,DiseaseDuration4,DiseaseDuration5,DiseaseDuration6,DiseaseDuration7,DiseaseDuration8,DiseaseDuration9, DiseaseDuration10),colnames=NA)
colnames(DiseaseDuration)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(DiseaseDuration)<-c("DiseaseDuration")

Edss1<-SMSCjagsResults1$BUGSoutput$summary[4,1]
Edss2<-SMSCjagsResults2$BUGSoutput$summary[4,1]
Edss3<-SMSCjagsResults3$BUGSoutput$summary[4,1]
Edss4<-SMSCjagsResults4$BUGSoutput$summary[4,1]
Edss5<-SMSCjagsResults5$BUGSoutput$summary[4,1]
Edss6<-SMSCjagsResults6$BUGSoutput$summary[4,1]
Edss7<-SMSCjagsResults7$BUGSoutput$summary[4,1]
Edss8<-SMSCjagsResults8$BUGSoutput$summary[4,1]
Edss9<-SMSCjagsResults9$BUGSoutput$summary[4,1]
Edss10<-SMSCjagsResults10$BUGSoutput$summary[4,1]
Edss<-as.matrix(cbind(Edss1,Edss2,Edss3,Edss4,Edss5,Edss6,Edss7,Edss8,Edss9, Edss10),colnames=NA)
colnames(Edss)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(Edss)<-c("Edss")

GdLesions1<-SMSCjagsResults1$BUGSoutput$summary[5,1]
GdLesions2<-SMSCjagsResults2$BUGSoutput$summary[5,1]
GdLesions3<-SMSCjagsResults3$BUGSoutput$summary[5,1]
GdLesions4<-SMSCjagsResults4$BUGSoutput$summary[5,1]
GdLesions5<-SMSCjagsResults5$BUGSoutput$summary[5,1]
GdLesions6<-SMSCjagsResults6$BUGSoutput$summary[5,1]
GdLesions7<-SMSCjagsResults7$BUGSoutput$summary[5,1]
GdLesions8<-SMSCjagsResults8$BUGSoutput$summary[5,1]
GdLesions9<-SMSCjagsResults9$BUGSoutput$summary[5,1]
GdLesions10<-SMSCjagsResults10$BUGSoutput$summary[5,1]
GdLesions<-as.matrix(cbind(GdLesions1,GdLesions2,GdLesions3,GdLesions4,GdLesions5,GdLesions6,GdLesions7,GdLesions8,GdLesions9, GdLesions10),colnames=NA)
colnames(GdLesions)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(GdLesions)<-c("GdLesions")

NrRelapses11<-SMSCjagsResults1$BUGSoutput$summary[6,1]
NrRelapses12<-SMSCjagsResults2$BUGSoutput$summary[6,1]
NrRelapses13<-SMSCjagsResults3$BUGSoutput$summary[6,1]
NrRelapses14<-SMSCjagsResults4$BUGSoutput$summary[6,1]
NrRelapses15<-SMSCjagsResults5$BUGSoutput$summary[6,1]
NrRelapses16<-SMSCjagsResults6$BUGSoutput$summary[6,1]
NrRelapses17<-SMSCjagsResults7$BUGSoutput$summary[6,1]
NrRelapses18<-SMSCjagsResults8$BUGSoutput$summary[6,1]
NrRelapses19<-SMSCjagsResults9$BUGSoutput$summary[6,1]
NrRelapses110<-SMSCjagsResults10$BUGSoutput$summary[6,1]
NrRelapses1<-as.matrix(cbind(NrRelapses11,NrRelapses12,NrRelapses13,NrRelapses14,NrRelapses15,NrRelapses16,NrRelapses17,NrRelapses18,NrRelapses19, NrRelapses110),colnames=NA)
colnames(NrRelapses1)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(NrRelapses1)<-c("NrRelapses1")

NrRelapses21<-SMSCjagsResults1$BUGSoutput$summary[7,1]
NrRelapses22<-SMSCjagsResults2$BUGSoutput$summary[7,1]
NrRelapses23<-SMSCjagsResults3$BUGSoutput$summary[7,1]
NrRelapses24<-SMSCjagsResults4$BUGSoutput$summary[7,1]
NrRelapses25<-SMSCjagsResults5$BUGSoutput$summary[7,1]
NrRelapses26<-SMSCjagsResults6$BUGSoutput$summary[7,1]
NrRelapses27<-SMSCjagsResults7$BUGSoutput$summary[7,1]
NrRelapses28<-SMSCjagsResults8$BUGSoutput$summary[7,1]
NrRelapses29<-SMSCjagsResults9$BUGSoutput$summary[7,1]
NrRelapses210<-SMSCjagsResults10$BUGSoutput$summary[7,1]
NrRelapses2<-as.matrix(cbind(NrRelapses21,NrRelapses22,NrRelapses23,NrRelapses24,NrRelapses25,NrRelapses26,NrRelapses27,NrRelapses28,NrRelapses29, NrRelapses210),colnames=NA)
colnames(NrRelapses2)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(NrRelapses2)<-c("NrRelapses2")

MonthsSinceRelapse1<-SMSCjagsResults1$BUGSoutput$summary[8,1]
MonthsSinceRelapse2<-SMSCjagsResults2$BUGSoutput$summary[8,1]
MonthsSinceRelapse3<-SMSCjagsResults3$BUGSoutput$summary[8,1]
MonthsSinceRelapse4<-SMSCjagsResults4$BUGSoutput$summary[8,1]
MonthsSinceRelapse5<-SMSCjagsResults5$BUGSoutput$summary[8,1]
MonthsSinceRelapse6<-SMSCjagsResults6$BUGSoutput$summary[8,1]
MonthsSinceRelapse7<-SMSCjagsResults7$BUGSoutput$summary[8,1]
MonthsSinceRelapse8<-SMSCjagsResults8$BUGSoutput$summary[8,1]
MonthsSinceRelapse9<-SMSCjagsResults9$BUGSoutput$summary[8,1]
MonthsSinceRelapse10<-SMSCjagsResults10$BUGSoutput$summary[8,1]
MonthsSinceRelapse<-as.matrix(cbind(MonthsSinceRelapse1,MonthsSinceRelapse2,MonthsSinceRelapse3,MonthsSinceRelapse4,MonthsSinceRelapse5,MonthsSinceRelapse6,MonthsSinceRelapse7,MonthsSinceRelapse8,MonthsSinceRelapse9, MonthsSinceRelapse10),colnames=NA)
colnames(MonthsSinceRelapse)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(MonthsSinceRelapse)<-c("MonthsSinceRelapse")

TrNaive1<-SMSCjagsResults1$BUGSoutput$summary[9,1]
TrNaive2<-SMSCjagsResults2$BUGSoutput$summary[9,1]
TrNaive3<-SMSCjagsResults3$BUGSoutput$summary[9,1]
TrNaive4<-SMSCjagsResults4$BUGSoutput$summary[9,1]
TrNaive5<-SMSCjagsResults5$BUGSoutput$summary[9,1]
TrNaive6<-SMSCjagsResults6$BUGSoutput$summary[9,1]
TrNaive7<-SMSCjagsResults7$BUGSoutput$summary[9,1]
TrNaive8<-SMSCjagsResults8$BUGSoutput$summary[9,1]
TrNaive9<-SMSCjagsResults9$BUGSoutput$summary[9,1]
TrNaive10<-SMSCjagsResults10$BUGSoutput$summary[9,1]
TrNaive<-as.matrix(cbind(TrNaive1,TrNaive2,TrNaive3,TrNaive4,TrNaive5,TrNaive6,TrNaive7,TrNaive8,TrNaive9, TrNaive10),colnames=NA)
colnames(TrNaive)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(TrNaive)<-c("TrNaive")

Gender1<-SMSCjagsResults1$BUGSoutput$summary[10,1]
Gender2<-SMSCjagsResults2$BUGSoutput$summary[10,1]
Gender3<-SMSCjagsResults3$BUGSoutput$summary[10,1]
Gender4<-SMSCjagsResults4$BUGSoutput$summary[10,1]
Gender5<-SMSCjagsResults5$BUGSoutput$summary[10,1]
Gender6<-SMSCjagsResults6$BUGSoutput$summary[10,1]
Gender7<-SMSCjagsResults7$BUGSoutput$summary[10,1]
Gender8<-SMSCjagsResults8$BUGSoutput$summary[10,1]
Gender9<-SMSCjagsResults9$BUGSoutput$summary[10,1]
Gender10<-SMSCjagsResults10$BUGSoutput$summary[10,1]
Gender<-as.matrix(cbind(Gender1,Gender2,Gender3,Gender4,Gender5,Gender6,Gender7,Gender8,Gender9, Gender10),colnames=NA)
colnames(Gender)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(Gender)<-c("Gender")

TreatmentCycle1<-SMSCjagsResults1$BUGSoutput$summary[11,1]
TreatmentCycle2<-SMSCjagsResults2$BUGSoutput$summary[11,1]
TreatmentCycle3<-SMSCjagsResults3$BUGSoutput$summary[11,1]
TreatmentCycle4<-SMSCjagsResults4$BUGSoutput$summary[11,1]
TreatmentCycle5<-SMSCjagsResults5$BUGSoutput$summary[11,1]
TreatmentCycle6<-SMSCjagsResults6$BUGSoutput$summary[11,1]
TreatmentCycle7<-SMSCjagsResults7$BUGSoutput$summary[11,1]
TreatmentCycle8<-SMSCjagsResults8$BUGSoutput$summary[11,1]
TreatmentCycle9<-SMSCjagsResults9$BUGSoutput$summary[11,1]
TreatmentCycle10<-SMSCjagsResults10$BUGSoutput$summary[11,1]
TreatmentCycle<-as.matrix(cbind(TreatmentCycle1,TreatmentCycle2,TreatmentCycle3,TreatmentCycle4,TreatmentCycle5,TreatmentCycle6,TreatmentCycle7,TreatmentCycle8,TreatmentCycle9, TreatmentCycle10),colnames=NA)
colnames(TreatmentCycle)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(TreatmentCycle)<-c("TreatmentCycle")


sigma1<-SMSCjagsResults1$BUGSoutput$summary[14,1]
sigma2<-SMSCjagsResults2$BUGSoutput$summary[14,1]
sigma3<-SMSCjagsResults3$BUGSoutput$summary[14,1]
sigma4<-SMSCjagsResults4$BUGSoutput$summary[14,1]
sigma5<-SMSCjagsResults5$BUGSoutput$summary[14,1]
sigma6<-SMSCjagsResults6$BUGSoutput$summary[14,1]
sigma7<-SMSCjagsResults7$BUGSoutput$summary[14,1]
sigma8<-SMSCjagsResults8$BUGSoutput$summary[14,1]
sigma9<-SMSCjagsResults9$BUGSoutput$summary[14,1]
sigma10<-SMSCjagsResults10$BUGSoutput$summary[14,1]
sigma<-as.matrix(cbind(sigma1,sigma2,sigma3,sigma4,sigma5,sigma6,sigma7,sigma8,sigma9, sigma10),colnames=NA)
colnames(sigma)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(sigma)<-c("sigma")

rho1<-SMSCjagsResults1$BUGSoutput$summary[13,1]
rho2<-SMSCjagsResults2$BUGSoutput$summary[13,1]
rho3<-SMSCjagsResults3$BUGSoutput$summary[13,1]
rho4<-SMSCjagsResults4$BUGSoutput$summary[13,1]
rho5<-SMSCjagsResults5$BUGSoutput$summary[13,1]
rho6<-SMSCjagsResults6$BUGSoutput$summary[13,1]
rho7<-SMSCjagsResults7$BUGSoutput$summary[13,1]
rho8<-SMSCjagsResults8$BUGSoutput$summary[13,1]
rho9<-SMSCjagsResults9$BUGSoutput$summary[13,1]
rho10<-SMSCjagsResults10$BUGSoutput$summary[13,1]
rho<-as.matrix(cbind(rho1,rho2,rho3,rho4,rho5,rho6,rho7,rho8,rho9, rho10),colnames=NA)
colnames(rho)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(rho)<-c("rho")

#matrix of estimates
qhat1<-rbind(x,age,DiseaseDuration,Edss,GdLesions,NrRelapses1,NrRelapses2,MonthsSinceRelapse,TrNaive,Gender,TreatmentCycle,rho,sigma)

#for variance-covariance matrix
sint1<-SMSCjagsResults1$BUGSoutput$summary[1,2]^2
sint2<-SMSCjagsResults2$BUGSoutput$summary[1,2]^2
sint3<-SMSCjagsResults3$BUGSoutput$summary[1,2]^2
sint4<-SMSCjagsResults4$BUGSoutput$summary[1,2]^2
sint5<-SMSCjagsResults5$BUGSoutput$summary[1,2]^2
sint6<-SMSCjagsResults6$BUGSoutput$summary[1,2]^2
sint7<-SMSCjagsResults7$BUGSoutput$summary[1,2]^2
sint8<-SMSCjagsResults8$BUGSoutput$summary[1,2]^2
sint9<-SMSCjagsResults9$BUGSoutput$summary[1,2]^2
sint10<-SMSCjagsResults10$BUGSoutput$summary[1,2]^2
sint<-as.matrix(cbind(sint1,sint2,sint3,sint4,sint5,sint6,sint7,sint8,sint9, sint10),colnames=NA)
colnames(sint)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(sint)<-c("sint")

sage1<-SMSCjagsResults1$BUGSoutput$summary[2,2]^2
sage2<-SMSCjagsResults2$BUGSoutput$summary[2,2]^2
sage3<-SMSCjagsResults3$BUGSoutput$summary[2,2]^2
sage4<-SMSCjagsResults4$BUGSoutput$summary[2,2]^2
sage5<-SMSCjagsResults5$BUGSoutput$summary[2,2]^2
sage6<-SMSCjagsResults6$BUGSoutput$summary[2,2]^2
sage7<-SMSCjagsResults7$BUGSoutput$summary[2,2]^2
sage8<-SMSCjagsResults8$BUGSoutput$summary[2,2]^2
sage9<-SMSCjagsResults9$BUGSoutput$summary[2,2]^2
sage10<-SMSCjagsResults10$BUGSoutput$summary[2,2]^2
sage<-as.matrix(cbind(sage1,sage2,sage3,sage4,sage5,sage6,sage7,sage8,sage9, sage10),colnames=NA)
colnames(sage)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(sage)<-c("sage")

sDiseaseDuration1<-SMSCjagsResults1$BUGSoutput$summary[3,2]^2
sDiseaseDuration2<-SMSCjagsResults2$BUGSoutput$summary[3,2]^2
sDiseaseDuration3<-SMSCjagsResults3$BUGSoutput$summary[3,2]^2
sDiseaseDuration4<-SMSCjagsResults4$BUGSoutput$summary[3,2]^2
sDiseaseDuration5<-SMSCjagsResults5$BUGSoutput$summary[3,2]^2
sDiseaseDuration6<-SMSCjagsResults6$BUGSoutput$summary[3,2]^2
sDiseaseDuration7<-SMSCjagsResults7$BUGSoutput$summary[3,2]^2
sDiseaseDuration8<-SMSCjagsResults8$BUGSoutput$summary[3,2]^2
sDiseaseDuration9<-SMSCjagsResults9$BUGSoutput$summary[3,2]^2
sDiseaseDuration10<-SMSCjagsResults10$BUGSoutput$summary[3,2]^2
sDiseaseDuration<-as.matrix(cbind(sDiseaseDuration1,sDiseaseDuration2,sDiseaseDuration3,sDiseaseDuration4,sDiseaseDuration5,sDiseaseDuration6,sDiseaseDuration7,sDiseaseDuration8,sDiseaseDuration9, sDiseaseDuration10),colnames=NA)
colnames(sDiseaseDuration)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(sDiseaseDuration)<-c("sDiseaseDuration")

sEdss1<-SMSCjagsResults1$BUGSoutput$summary[4,2]^2
sEdss2<-SMSCjagsResults2$BUGSoutput$summary[4,2]^2
sEdss3<-SMSCjagsResults3$BUGSoutput$summary[4,2]^2
sEdss4<-SMSCjagsResults4$BUGSoutput$summary[4,2]^2
sEdss5<-SMSCjagsResults5$BUGSoutput$summary[4,2]^2
sEdss6<-SMSCjagsResults6$BUGSoutput$summary[4,2]^2
sEdss7<-SMSCjagsResults7$BUGSoutput$summary[4,2]^2
sEdss8<-SMSCjagsResults8$BUGSoutput$summary[4,2]^2
sEdss9<-SMSCjagsResults9$BUGSoutput$summary[4,2]^2
sEdss10<-SMSCjagsResults10$BUGSoutput$summary[4,2]^2
sEdss<-as.matrix(cbind(sEdss1,sEdss2,sEdss3,sEdss4,sEdss5,sEdss6,sEdss7,sEdss8,sEdss9, sEdss10),colnames=NA)
colnames(sEdss)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(sEdss)<-c("sEdss")

sGdLesions1<-SMSCjagsResults1$BUGSoutput$summary[5,2]^2
sGdLesions2<-SMSCjagsResults2$BUGSoutput$summary[5,2]^2
sGdLesions3<-SMSCjagsResults3$BUGSoutput$summary[5,2]^2
sGdLesions4<-SMSCjagsResults4$BUGSoutput$summary[5,2]^2
sGdLesions5<-SMSCjagsResults5$BUGSoutput$summary[5,2]^2
sGdLesions6<-SMSCjagsResults6$BUGSoutput$summary[5,2]^2
sGdLesions7<-SMSCjagsResults7$BUGSoutput$summary[5,2]^2
sGdLesions8<-SMSCjagsResults8$BUGSoutput$summary[5,2]^2
sGdLesions9<-SMSCjagsResults9$BUGSoutput$summary[5,2]^2
sGdLesions10<-SMSCjagsResults10$BUGSoutput$summary[5,2]^2
sGdLesions<-as.matrix(cbind(sGdLesions1,sGdLesions2,sGdLesions3,sGdLesions4,sGdLesions5,sGdLesions6,sGdLesions7,sGdLesions8,sGdLesions9, sGdLesions10),colnames=NA)
colnames(sGdLesions)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(sGdLesions)<-c("sGdLesions")

sNrRelapses11<-SMSCjagsResults1$BUGSoutput$summary[6,2]^2
sNrRelapses12<-SMSCjagsResults2$BUGSoutput$summary[6,2]^2
sNrRelapses13<-SMSCjagsResults3$BUGSoutput$summary[6,2]^2
sNrRelapses14<-SMSCjagsResults4$BUGSoutput$summary[6,2]^2
sNrRelapses15<-SMSCjagsResults5$BUGSoutput$summary[6,2]^2
sNrRelapses16<-SMSCjagsResults6$BUGSoutput$summary[6,2]^2
sNrRelapses17<-SMSCjagsResults7$BUGSoutput$summary[6,2]^2
sNrRelapses18<-SMSCjagsResults8$BUGSoutput$summary[6,2]^2
sNrRelapses19<-SMSCjagsResults9$BUGSoutput$summary[6,2]^2
sNrRelapses110<-SMSCjagsResults10$BUGSoutput$summary[6,2]^2
sNrRelapses1<-as.matrix(cbind(sNrRelapses11,sNrRelapses12,sNrRelapses13,sNrRelapses14,sNrRelapses15,sNrRelapses16,sNrRelapses17,sNrRelapses18,sNrRelapses19, sNrRelapses110),colnames=NA)
colnames(sNrRelapses1)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(sNrRelapses1)<-c("sNrRelapses1")

sNrRelapses21<-SMSCjagsResults1$BUGSoutput$summary[7,2]^2
sNrRelapses22<-SMSCjagsResults2$BUGSoutput$summary[7,2]^2
sNrRelapses23<-SMSCjagsResults3$BUGSoutput$summary[7,2]^2
sNrRelapses24<-SMSCjagsResults4$BUGSoutput$summary[7,2]^2
sNrRelapses25<-SMSCjagsResults5$BUGSoutput$summary[7,2]^2
sNrRelapses26<-SMSCjagsResults6$BUGSoutput$summary[7,2]^2
sNrRelapses27<-SMSCjagsResults7$BUGSoutput$summary[7,2]^2
sNrRelapses28<-SMSCjagsResults8$BUGSoutput$summary[7,2]^2
sNrRelapses29<-SMSCjagsResults9$BUGSoutput$summary[7,2]^2
sNrRelapses210<-SMSCjagsResults10$BUGSoutput$summary[7,2]^2
sNrRelapses2<-as.matrix(cbind(sNrRelapses21,sNrRelapses22,sNrRelapses23,sNrRelapses24,sNrRelapses25,sNrRelapses26,sNrRelapses27,sNrRelapses28,sNrRelapses29, sNrRelapses210),colnames=NA)
colnames(sNrRelapses2)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(sNrRelapses2)<-c("sNrRelapses2")

sMonthsRelapse1<-SMSCjagsResults1$BUGSoutput$summary[8,2]^2
sMonthsRelapse2<-SMSCjagsResults2$BUGSoutput$summary[8,2]^2
sMonthsRelapse3<-SMSCjagsResults3$BUGSoutput$summary[8,2]^2
sMonthsRelapse4<-SMSCjagsResults4$BUGSoutput$summary[8,2]^2
sMonthsRelapse5<-SMSCjagsResults5$BUGSoutput$summary[8,2]^2
sMonthsRelapse6<-SMSCjagsResults6$BUGSoutput$summary[8,2]^2
sMonthsRelapse7<-SMSCjagsResults7$BUGSoutput$summary[8,2]^2
sMonthsRelapse8<-SMSCjagsResults8$BUGSoutput$summary[8,2]^2
sMonthsRelapse9<-SMSCjagsResults9$BUGSoutput$summary[8,2]^2
sMonthsRelapse10<-SMSCjagsResults10$BUGSoutput$summary[8,2]^2
sMonthsRelapse<-as.matrix(cbind(sMonthsRelapse1,sMonthsRelapse2,sMonthsRelapse3,sMonthsRelapse4,sMonthsRelapse5,sMonthsRelapse6,sMonthsRelapse7,sMonthsRelapse8,sMonthsRelapse9, sMonthsRelapse10),colnames=NA)
colnames(sMonthsRelapse)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(sMonthsRelapse)<-c("sMonthsRelapse")

sTrNaive1<-SMSCjagsResults1$BUGSoutput$summary[9,2]^2
sTrNaive2<-SMSCjagsResults2$BUGSoutput$summary[9,2]^2
sTrNaive3<-SMSCjagsResults3$BUGSoutput$summary[9,2]^2
sTrNaive4<-SMSCjagsResults4$BUGSoutput$summary[9,2]^2
sTrNaive5<-SMSCjagsResults5$BUGSoutput$summary[9,2]^2
sTrNaive6<-SMSCjagsResults6$BUGSoutput$summary[9,2]^2
sTrNaive7<-SMSCjagsResults7$BUGSoutput$summary[9,2]^2
sTrNaive8<-SMSCjagsResults8$BUGSoutput$summary[9,2]^2
sTrNaive9<-SMSCjagsResults9$BUGSoutput$summary[9,2]^2
sTrNaive10<-SMSCjagsResults10$BUGSoutput$summary[9,2]^2
sTrNaive<-as.matrix(cbind(sTrNaive1,sTrNaive2,sTrNaive3,sTrNaive4,sTrNaive5,sTrNaive6,sTrNaive7,sTrNaive8,sTrNaive9, sTrNaive10),colnames=NA)
colnames(sTrNaive)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(sTrNaive)<-c("sTrNaive")

sGender1<-SMSCjagsResults1$BUGSoutput$summary[10,2]^2
sGender2<-SMSCjagsResults2$BUGSoutput$summary[10,2]^2
sGender3<-SMSCjagsResults3$BUGSoutput$summary[10,2]^2
sGender4<-SMSCjagsResults4$BUGSoutput$summary[10,2]^2
sGender5<-SMSCjagsResults5$BUGSoutput$summary[10,2]^2
sGender6<-SMSCjagsResults6$BUGSoutput$summary[10,2]^2
sGender7<-SMSCjagsResults7$BUGSoutput$summary[10,2]^2
sGender8<-SMSCjagsResults8$BUGSoutput$summary[10,2]^2
sGender9<-SMSCjagsResults9$BUGSoutput$summary[10,2]^2
sGender10<-SMSCjagsResults10$BUGSoutput$summary[10,2]^2
sGender<-as.matrix(cbind(sGender1,sGender2,sGender3,sGender4,sGender5,sGender6,sGender7,sGender8,sGender9, sGender10),colnames=NA)
colnames(sGender)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(sGender)<-c("sGender")

sTreatmentCycle1<-SMSCjagsResults1$BUGSoutput$summary[11,2]^2
sTreatmentCycle2<-SMSCjagsResults2$BUGSoutput$summary[11,2]^2
sTreatmentCycle3<-SMSCjagsResults3$BUGSoutput$summary[11,2]^2
sTreatmentCycle4<-SMSCjagsResults4$BUGSoutput$summary[11,2]^2
sTreatmentCycle5<-SMSCjagsResults5$BUGSoutput$summary[11,2]^2
sTreatmentCycle6<-SMSCjagsResults6$BUGSoutput$summary[11,2]^2
sTreatmentCycle7<-SMSCjagsResults7$BUGSoutput$summary[11,2]^2
sTreatmentCycle8<-SMSCjagsResults8$BUGSoutput$summary[11,2]^2
sTreatmentCycle9<-SMSCjagsResults9$BUGSoutput$summary[11,2]^2
sTreatmentCycle10<-SMSCjagsResults10$BUGSoutput$summary[11,2]^2
sTreatmentCycle<-as.matrix(cbind(sTreatmentCycle1,sTreatmentCycle2,sTreatmentCycle3,sTreatmentCycle4,sTreatmentCycle5,sTreatmentCycle6,sTreatmentCycle7,sTreatmentCycle8,sTreatmentCycle9, sTreatmentCycle10),colnames=NA)
colnames(sTreatmentCycle)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(sTreatmentCycle)<-c("sTreatmentCycle")

srho1<-SMSCjagsResults1$BUGSoutput$summary[13,2]^2
srho2<-SMSCjagsResults2$BUGSoutput$summary[13,2]^2
srho3<-SMSCjagsResults3$BUGSoutput$summary[13,2]^2
srho4<-SMSCjagsResults4$BUGSoutput$summary[13,2]^2
srho5<-SMSCjagsResults5$BUGSoutput$summary[13,2]^2
srho6<-SMSCjagsResults6$BUGSoutput$summary[13,2]^2
srho7<-SMSCjagsResults7$BUGSoutput$summary[13,2]^2
srho8<-SMSCjagsResults8$BUGSoutput$summary[13,2]^2
srho9<-SMSCjagsResults9$BUGSoutput$summary[13,2]^2
srho10<-SMSCjagsResults10$BUGSoutput$summary[13,2]^2
srho<-as.matrix(cbind(srho1,srho2,srho3,srho4,srho5,srho6,srho7,srho8,srho9, srho10),colnames=NA)
colnames(srho)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(srho)<-c("srho")

ssigma1<-SMSCjagsResults1$BUGSoutput$summary[14,2]^2
ssigma2<-SMSCjagsResults2$BUGSoutput$summary[14,2]^2
ssigma3<-SMSCjagsResults3$BUGSoutput$summary[14,2]^2
ssigma4<-SMSCjagsResults4$BUGSoutput$summary[14,2]^2
ssigma5<-SMSCjagsResults5$BUGSoutput$summary[14,2]^2
ssigma6<-SMSCjagsResults6$BUGSoutput$summary[14,2]^2
ssigma7<-SMSCjagsResults7$BUGSoutput$summary[14,2]^2
ssigma8<-SMSCjagsResults8$BUGSoutput$summary[14,2]^2
ssigma9<-SMSCjagsResults9$BUGSoutput$summary[14,2]^2
ssigma10<-SMSCjagsResults10$BUGSoutput$summary[14,2]^2
ssigma<-as.matrix(cbind(ssigma1,ssigma2,ssigma3,ssigma4,ssigma5,ssigma6,ssigma7,ssigma8,ssigma9, ssigma10),colnames=NA)
colnames(ssigma)<-c("1","2","3","4","5","6","7","8","9","10")
rownames(ssigma)<-c("ssigma")

#matrix of variances
uhat1<-rbind(sint,sage,sDiseaseDuration,sEdss,sGdLesions,sNrRelapses1,sNrRelapses2,sMonthsRelapse,sTrNaive,sGender, sTreatmentCycle,srho,ssigma)


Pooled<-testEstimates(qhat = qhat1, uhat = uhat1, var.comp = T)
pooled_table<-round(Pooled$estimates[,1],2)
#write.xlsx(pooled_table, "C:/Users/kc19o338/Desktop/Real world predictions project/NMAPedictionsRiskModel/pooled_table2.xlsx")
