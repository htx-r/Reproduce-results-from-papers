########################################################################
#################     Script for estimating the NB  ####################
#######                       Under strategies:          ######################
#####   Treat none, treat all with N, treat all with DF, ############
#####     Treat all with GA, and treat based on the model  ##########
################ for one set of threshold values:    #######################
##################     T_N=20%, T_DF=T_GA=10%      ###################3
##############################################################



#insert the threshold values needed for each variable
T_DF=0.10
T_GA=0.10
T_N=0.20

##Rule for recommending treatments based on hte prediction model:
#For a patient i the recommended treatment j is the one that satisfies max {RD_ij-T_j},
#between those that 〖RD〗_ij≥T_j.

RiskData$maxRDTh<-NA
RiskData$RecommendedTreatmentThreshold<-NA
for (i in 1:nrow(RiskData)){
  RiskData$maxRDTh[i]<-max(RiskData$RDDF[i]-T_DF,RiskData$RDGA[i]-T_GA,RiskData$RDN[i]-T_N,0)
  RiskData$RecommendedTreatmentThreshold[i]<-which.max(c(RiskData$RDDF[i]-T_DF,RiskData$RDGA[i]-T_GA,RiskData$RDN[i]-T_N,0))
}

#The congruent dataset with the recommended treatment equal to the actual one
RiskDataCongruent<-RiskData[which(RiskData$Treatment==RiskData$RecommendedTreatmentThreshold),]


###find the rate of relapse in placebo arm in the big dataset

#ADdataset
RiskData$outcome<-RiskData$RELAPSE2year
n_studies<-c(nrow(RiskData[which(RiskData$STUDYID==1 & RiskData$TRT01A==1),]),nrow(RiskData[which(RiskData$STUDYID==1 & RiskData$TRT01A==4),]),
             nrow(RiskData[which(RiskData$STUDYID==2 & RiskData$TRT01A==1),]),nrow(RiskData[which(RiskData$STUDYID==2 & RiskData$TRT01A==2),]),
             nrow(RiskData[which(RiskData$STUDYID==2 & RiskData$TRT01A==4),]),nrow(RiskData[which(RiskData$STUDYID==3 & RiskData$TRT01A==3),]),
             nrow(RiskData[which(RiskData$STUDYID==3 & RiskData$TRT01A==4),]))

n_events<-c(nrow(RiskData[which(RiskData$STUDYID==1 & RiskData$TRT01A==1 & RiskData$outcome==1),]), nrow(RiskData[which(RiskData$STUDYID==1 & RiskData$TRT01A==4 & RiskData$outcome==1),]),
            nrow(RiskData[which(RiskData$STUDYID==2 & RiskData$TRT01A==1 & RiskData$outcome==1),]), nrow(RiskData[which(RiskData$STUDYID==2 & RiskData$TRT01A==2 & RiskData$outcome==1),]),
            nrow(RiskData[which(RiskData$STUDYID==2 & RiskData$TRT01A==4 & RiskData$outcome==1),]), nrow(RiskData[which(RiskData$STUDYID==3 & RiskData$TRT01A==3 & RiskData$outcome==1),]),
            nrow(RiskData[which(RiskData$STUDYID==3 & RiskData$TRT01A==4 & RiskData$outcome==1),]))


dataAD_Whole<-cbind(c(1,1,2,2,2,3,3),c(1,4,1,2,4,3,4),n_studies, n_events)
dataAD_Whole<-as.data.frame(dataAD_Whole)
colnames(dataAD_Whole)<-c("St","Dr","N_ran","N_rel")
dataAD_Whole_Pl<-dataAD_Whole[which(dataAD_Whole$Dr==4),]
## event rate in placebo arm in the whole dataset
m1<-metaprop(N_rel,N_ran,data=dataAD_Whole_Pl,sm="PLOGIT" )

#network meta-analysis in the whole dataset
TestPair <- pairwise(treat=Dr, event=N_rel, n=N_ran, data=dataAD_Whole, sm="RR", studlab=St, allstudies = TRUE)

net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = TestPair, sm = "RR", comb.random=F, comb.fixed=T, prediction=TRUE, ref=4)

summary(net1, digits = 2)

forest(net1,ref=4,fontsize=10)


###The same for the congruent dataset

n_study1T1<-nrow(RiskDataCongruent[which(RiskDataCongruent$STUDYID==1 & RiskDataCongruent$TRT01A==1),])
n_study1T4<-nrow(RiskDataCongruent[which(RiskDataCongruent$STUDYID==1 & RiskDataCongruent$TRT01A==4),])
n_study2T1<-nrow(RiskDataCongruent[which(RiskDataCongruent$STUDYID==2 & RiskDataCongruent$TRT01A==1),])
n_study2T2<-nrow(RiskDataCongruent[which(RiskDataCongruent$STUDYID==2 & RiskDataCongruent$TRT01A==2),])
n_study2T4<-nrow(RiskDataCongruent[which(RiskDataCongruent$STUDYID==2 & RiskDataCongruent$TRT01A==4),])
n_study3T3<-nrow(RiskDataCongruent[which(RiskDataCongruent$STUDYID==3 & RiskDataCongruent$TRT01A==3),])
n_study3T4<-nrow(RiskDataCongruent[which(RiskDataCongruent$STUDYID==3 & RiskDataCongruent$TRT01A==4),])
r_study1T1<-nrow(RiskDataCongruent[which(RiskDataCongruent$STUDYID==1 & RiskDataCongruent$TRT01A==1 & RiskDataCongruent$outcome==1),])
r_study1T4<-nrow(RiskDataCongruent[which(RiskDataCongruent$STUDYID==1 & RiskDataCongruent$TRT01A==4 & RiskDataCongruent$outcome==1),])
r_study2T1<-nrow(RiskDataCongruent[which(RiskDataCongruent$STUDYID==2 & RiskDataCongruent$TRT01A==1 & RiskDataCongruent$outcome==1),])
r_study2T2<-nrow(RiskDataCongruent[which(RiskDataCongruent$STUDYID==2 & RiskDataCongruent$TRT01A==2 & RiskDataCongruent$outcome==1),])
r_study2T4<-nrow(RiskDataCongruent[which(RiskDataCongruent$STUDYID==2 & RiskDataCongruent$TRT01A==4 & RiskDataCongruent$outcome==1),])
r_study3T3<-nrow(RiskDataCongruent[which(RiskDataCongruent$STUDYID==3 & RiskDataCongruent$TRT01A==3 & RiskDataCongruent$outcome==1),])
r_study3T4<-nrow(RiskDataCongruent[which(RiskDataCongruent$STUDYID==3 & RiskDataCongruent$TRT01A==4 & RiskDataCongruent$outcome==1),])
dataAD_Congruent<-cbind(c(1,1,2,2,2,3,3),c(1,4,1,2,4,3,4),c(n_study1T1,n_study1T4,n_study2T1,n_study2T2,n_study2T4,n_study3T3,n_study3T4),
                        c(r_study1T1,r_study1T4,r_study2T1,r_study2T2,r_study2T4,r_study3T3,r_study3T4))

dataAD_Congruent<-as.data.frame(dataAD_Congruent)
colnames(dataAD_Congruent)<-c("St","Dr","N_ran","N_rel")

dataAD_Congruent_Pl<-dataAD_Congruent[which(dataAD_Congruent$Dr==4),]
## event rate in placebo arm in the congruent dataset
m2<-metaprop(N_rel,N_ran,data=dataAD_Congruent_Pl,sm="PLOGIT" )

#network meta-analysis in the congruent dataset
TestPair2 <- pairwise(treat=Dr, event=N_rel, n=N_ran, data=dataAD_Congruent, sm="RR", studlab=St, allstudies = TRUE)

net2 <- netmeta(TE, seTE, treat1, treat2, studlab, data = TestPair2, sm = "RR", comb.random=F, comb.fixed=T, prediction=TRUE, ref=4)
summary(net2, digits = 2)



###Net benefit calculation for each strategy

#NB for strategy treat nobody will be always equal to 0
NetBenefit_Placebo<-0.000

#NB for "treat all with Natalizumab" will be
NetBenefit_Natalizumab<-(expit(m1$TE.fixed)-expit(m1$TE.fixed)*exp(net1$TE.fixed)[3,4])-T_N
#NB for "treat all with Dimethyl Fumarate" will be
NetBenefit_DF<-(expit(m1$TE.fixed)-expit(m1$TE.fixed)*exp(net1$TE.fixed)[1,4])-T_DF
#NB for "treat all with Glatiramer acetate" will be
NetBenefit_GA<-(expit(m1$TE.fixed)-expit(m1$TE.fixed)*exp(net1$TE.fixed)[2,4])-T_GA


#Calculations needed for NB estimation for "treat based on the model" strategy:
n_total<-nrow(RiskDataCongruent)
n_pl<-nrow(RiskDataCongruent[which(RiskDataCongruent$Treatment==4),])
n_N<-nrow(RiskDataCongruent[which(RiskDataCongruent$Treatment==3),])
n_DF<-nrow(RiskDataCongruent[which(RiskDataCongruent$Treatment==1),])
n_GA<-nrow(RiskDataCongruent[which(RiskDataCongruent$Treatment==2),])
e_total<-(n_pl*expit(m2$TE.fixed)+n_DF*expit(m2$TE.fixed)*exp(net2$TE.fixed)[1,3]+n_N*expit(m2$TE.fixed)*exp(net2$TE.fixed)[2,3])/n_total
#NB for "treat based on the model" strategy will be
NetBenefit_Model<-(expit(m1$TE.fixed)-e_total)-((n_N/n_total)*T_N+(n_DF/n_total)*T_DF+(n_GA/n_total)*T_GA)

#Creation of a table with the results
Net_Benefit<-as.data.frame(round(rbind(NetBenefit_Placebo,NetBenefit_Natalizumab,NetBenefit_DF,NetBenefit_GA,NetBenefit_Model),3))
colnames(Net_Benefit)<-c("Net Benefit")
rownames(Net_Benefit)<-c("Treat all with Placebo","Treat all with Natalizumab", "Treat all with Dimethyl Fumerate","Treat all with Glatiramere Acetate","Treat based on the model")

