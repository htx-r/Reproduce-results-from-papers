########################################################################
#################     Script for estimating the NB  ####################
#######                       Under strategies:          ######################
#####   Treat none, treat all with N, treat all with DF, ############
#####     Treat all with GA, and treat based on the model  ##########
################ for one set of threshold values:    #######################
##################     T_DF=T_GA=10%    ###################3
###### and a range between 19% - 40% for Natalizumab thresholds  #################
######################### & Creation of relevant Plot  ################################
##################################################################################3





#Entire ADdataset
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

# CREATING DATAFRAME THAT IS ONE LINE PER THRESHOLD PER all default STRATEGIES
nb=data.frame(seq(from=0.01, to=1, by=0.01))
names(nb)="threshold"
interv=nb

###Set threshold values for DF and Glatiramer
T_DF=0.10
T_GA=0.10

## MB for default strategies
##Estimation of the NB under "treat none" strategy
nb["Placebo"]=0
##Estimation of the NB under "treat all with Natalizumab" strategy using the corresponding threshold values
nb["Natalizumab"]<-(expit(m1$TE.fixed)-expit(m1$TE.fixed)*exp(net1$TE.fixed)[3,4])-nb$threshold
##Estimation of the NB under "treat all with DF" strategy using the corresponding threshold values
nb["Dimethyl FUmerate"]<-(expit(m1$TE.fixed)-expit(m1$TE.fixed)*exp(net1$TE.fixed)[1,4])-T_DF
##Estimation of the NB under "treat all with GA" strategy using the corresponding threshold values
nb["Glatiramer Acetate"]<-(expit(m1$TE.fixed)-expit(m1$TE.fixed)*exp(net1$TE.fixed)[2,4])-T_GA


###Start estimating net benefit for strategy "treat based on the model"
nb["Model"]<-NA
nb["OptimalNB"]<-NA
#The rule of deciding
for (t in 19:40){
  RiskData$maxRDTh<-NA
  RiskData$RecommendedTreatmentThreshold<-NA
  for (i in 1:nrow(RiskData)){
    RiskData$maxRDTh[i]<-max(RiskData$RDDF[i]-T_DF,RiskData$RDGA[i]-T_GA,RiskData$RDN[i]-nb$threshold[t],0)
    RiskData$RecommendedTreatmentThreshold[i]<-which.max(c(RiskData$RDDF[i]-T_DF,RiskData$RDGA[i]-T_GA,RiskData$RDN[i]-nb$threshold[t],0))
  }


## Creation of the congruent dataset, where the recommended treatment
  #via the model is the actual given one
  RiskDataCongruent<-RiskData[which(RiskData$Treatment==RiskData$RecommendedTreatmentThreshold),]
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

  if (nrow(dataAD_Congruent[which(dataAD_Congruent$Dr==4 & dataAD_Congruent$N_ran!=0 ),])!=0) {
      dataAD_Congruent_Pl<-dataAD_Congruent[which(dataAD_Congruent$Dr==4 & dataAD_Congruent$N_ran!=0 ),]
  ## event rate in placebo arm in the congruent dataset
  m2<-metaprop(N_rel,N_ran,data=dataAD_Congruent_Pl,sm="PLOGIT" )

  }

  if (nrow(dataAD_Congruent[which(dataAD_Congruent$Dr==4 & dataAD_Congruent$N_ran!=0 ),])==0) {
    dataAD_Congruent_Pl<-dataAD_Congruent[which(dataAD_Congruent$Dr==1 & dataAD_Congruent$N_ran!=0 ),]
    ## event rate in placebo arm in the congruent dataset
    m2<-metaprop(N_rel,N_ran,data=dataAD_Congruent_Pl,sm="PLOGIT" )

  }

  #network meta-analysis in the congruent dataset
  if (nrow(dataAD_Congruent[which(dataAD_Congruent$Dr==4 & dataAD_Congruent$N_ran!=0 ),])!=0) {
  TestPair <- pairwise(treat=Dr, event=N_rel, n=N_ran, data=dataAD_Congruent, sm="RR", studlab=St, allstudies = TRUE)
  net2 <- netmeta(TE, seTE, treat1, treat2, studlab, data = TestPair, sm = "RR", comb.random=F, comb.fixed=T, prediction=TRUE, ref=4)
  }

  if (nrow(dataAD_Congruent[which(dataAD_Congruent$Dr==4 & dataAD_Congruent$N_ran!=0 ),])==0) {
    TestPair <- pairwise(treat=Dr, event=N_rel, n=N_ran, data=dataAD_Congruent, sm="RR", studlab=St, allstudies = TRUE)
    net2 <- netmeta(TE, seTE, treat1, treat2, studlab, data = TestPair, sm = "RR", comb.random=F, comb.fixed=T, prediction=TRUE, ref=1)
  }

  n_total<-nrow(RiskDataCongruent)
  n_pl<-nrow(RiskDataCongruent[which(RiskDataCongruent$Treatment==4),])
  n_N<-nrow(RiskDataCongruent[which(RiskDataCongruent$Treatment==3),])
  n_DF<-nrow(RiskDataCongruent[which(RiskDataCongruent$Treatment==1),])
  n_GA<-nrow(RiskDataCongruent[which(RiskDataCongruent$Treatment==2),])

  if (n_N!=0) {
    e_N<-n_N*expit(m2$TE.fixed)* exp(net2$TE.nma.fixed[which(net2$treat1==3)])
  }
  if (n_N==0) {
    e_N<-0
  }
  if (n_DF!=0 && nrow(dataAD_Congruent[which(dataAD_Congruent$Dr==4 & dataAD_Congruent$N_ran!=0 ),])!=0  ) {
    e_DF<-n_DF*expit(m2$TE.fixed)* exp(net2$TE.nma.fixed[which(net2$treat1==1)])
    e_DF<-unique(e_DF)
  }

  if (n_DF!=0 && nrow(dataAD_Congruent[which(dataAD_Congruent$Dr==4 & dataAD_Congruent$N_ran==0 ),])!=0  ) {
    e_DF<-n_DF*expit(m2$TE.fixed)
  }
  if (n_DF==0) {
    e_DF<-0
  }
  if (n_GA!=0) {
    e_GA<-n_GA*expit(m2$TE.fixed)* exp(net2$TE.nma.fixed[which(net2$treat1==2)])
  }
  if (n_GA==0) {
    e_GA<-0
  }

  if (n_pl!=0) {
    e_pl<-n_pl*expit(m2$TE.fixed)
  }
  if (n_pl==0) {
    e_pl<-0
  }

  e_total<- ( e_pl + e_N + e_DF + e_GA)/n_total
  nb[t,6]<-(expit(m1$TE.fixed)-e_total)-((n_N*nb$threshold[t]+n_DF*T_DF+n_GA*T_GA)/n_total)
  nb[t,7]<-max(nb[t,2],nb[t,3],nb[t,4], nb[t,5], nb[t,6])
}



#getting maximum number of avoided interventions
ymax=max(nb[11:30,3:6],na.rm = TRUE)
ymin=min(nb[11:30,3:6],na.rm = TRUE)
# inializing new benfit plot with treat all with Natalizumab option
plot(x=nb$threshold[19:40], y=nb$Natalizumab[19:40], type="l",lwd=3,col="green",xlim=c(0.19, 0.40), ylim=c(ymin, ymax+0.2), xlab="Threshold probability", ylab=paste("Net Benefit"))
# adding treat none option
lines(x=nb$threshold[19:40], y=nb$Placebo[19:40],col="blue",lwd=3)
lines(nb$threshold[19:40],y=nb$`Dimethyl FUmerate`[19:40],col="yellow",lwd=3)
lines(nb$threshold[19:40],y=nb$`Glatiramer Acetate`[19:40],col="orange",lwd=3)
lines(nb$threshold[19:40],y=nb$Model[19:40],lwd=3, col="red")
#lines(nb$threshold[20:40], y=c(nb$Natalizumab[20],nb$Model[21:28],nb$`Dimethyl FUmerate`[29:40]) , type="l",col=c("light blue", "red", "orange"),lwd=2)
lines(nb$threshold[19:20], y=nb$OptimalNB[19:20]+0.005, type="l",lty = "dashed",col="black",lwd=3)
lines(nb$threshold[20:21], y=nb$OptimalNB[20:21]+0.005, type="l",lty = "dashed",col="black",lwd=3)
lines(nb$threshold[21:22], y=nb$OptimalNB[21:22]+0.005, type="l",lty = "dashed",col="black",lwd=3)
lines(nb$threshold[22:24], y=nb$OptimalNB[22:24]+0.005, type="l",lty = "dashed",col="black",lwd=3)
lines(nb$threshold[24:25], y=nb$OptimalNB[24:25]+0.005, type="l",lty = "dashed",col="black",lwd=3)
lines(nb$threshold[25:40], y=nb$OptimalNB[25:40]+0.005, type="l",lty = "dashed",col="black",lwd=3)



#then add the legends
#legend("topright", c("Treat none","Treat based on the model","Treat all with Natalizumab","Treat all with Dimethyl Fumerate","Highest Net Benefit"), cex=0.8, col=c(),text.font=4, lwd=2)



