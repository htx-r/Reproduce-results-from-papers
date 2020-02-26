############################ Script for reproduce paper's figures and tables ######################################################



############################   TABLES   ##############################################################################

#### Table 1

#For AFFIRM study:

AFFIRM_T<-as.data.frame(cbind(nrow(MSrelapse[which(MSrelapse$STUDYID=="AFFIRM"),]),
                              paste(nrow(MSrelapse[which(MSrelapse$STUDYID=="AFFIRM" & MSrelapse$RELAPSE2year==1),]), "(",
                                    round((nrow(MSrelapse[which(MSrelapse$STUDYID=="AFFIRM" & MSrelapse$RELAPSE2year==1),])/nrow(MSrelapse[which(MSrelapse$STUDYID=="AFFIRM"),]))*100,1), ")" )
                              , paste(round(mean(MSrelapse$AGE[which(MSrelapse$STUDYID=="AFFIRM")]),1), "(", round(sd((MSrelapse$AGE[which(MSrelapse$STUDYID=="AFFIRM")])),1), ")")
                              , paste(table(MSrelapse$SEX[which(MSrelapse$STUDYID=="AFFIRM")])[1], "(", round(
                                (table(MSrelapse$SEX[which(MSrelapse$STUDYID=="AFFIRM")])[1]/nrow(MSrelapse[which(MSrelapse$STUDYID=="AFFIRM"),]))*100,
                                1),
                                ")"),  paste(table(MSrelapse$SEX[which(MSrelapse$STUDYID=="AFFIRM")])[2], "(", round(
                                  (table(MSrelapse$SEX[which(MSrelapse$STUDYID=="AFFIRM")])[2]/nrow(MSrelapse[which(MSrelapse$STUDYID=="AFFIRM"),]))*100,
                                  1),
                                  ")"), paste(round(mean(MSrelapse$EDSSBL[which(MSrelapse$STUDYID=="AFFIRM")]),1), "(", round(sd(MSrelapse$EDSSBL[which(MSrelapse$STUDYID=="AFFIRM")]),1), ")"),
                              paste(round(median(MSrelapse$RLPS1YR[which(MSrelapse$STUDYID=="AFFIRM")]),0), "(",
                                    exp(min(MSrelapse$RLPS1YR[which(MSrelapse$STUDYID=="AFFIRM")]))-1, ", ",
                                    exp(max(MSrelapse$RLPS1YR[which(MSrelapse$STUDYID=="AFFIRM")]))-1, ")")
))


AFFIRM_N<-cbind(nrow(MSrelapse[which(MSrelapse$STUDYID=="AFFIRM" & MSrelapse$TRT01A=="Natalizumab"),]),
                paste(nrow(MSrelapse[which(MSrelapse$STUDYID=="AFFIRM" & MSrelapse$RELAPSE2year==1 & MSrelapse$TRT01A=="Natalizumab"),]), "(",round((nrow(MSrelapse[which(MSrelapse$STUDYID=="AFFIRM" & MSrelapse$RELAPSE2year==1 & MSrelapse$TRT01A=="Natalizumab"),])/nrow(MSrelapse[which(MSrelapse$STUDYID=="AFFIRM"  & MSrelapse$TRT01A=="Natalizumab"),]))*100,1) ,")")
                ,NA,NA,NA,NA,NA)


AFFIRM_P<-cbind(nrow(MSrelapse[which(MSrelapse$STUDYID=="AFFIRM" & MSrelapse$TRT01A=="Placebo"),]),
                paste(nrow(MSrelapse[which(MSrelapse$STUDYID=="AFFIRM" & MSrelapse$RELAPSE2year==1 & MSrelapse$TRT01A=="Placebo"),]), "(",round((nrow(MSrelapse[which(MSrelapse$STUDYID=="AFFIRM" & MSrelapse$RELAPSE2year==1 & MSrelapse$TRT01A=="Placebo"),])/nrow(MSrelapse[which(MSrelapse$STUDYID=="AFFIRM"  & MSrelapse$TRT01A=="Placebo"),]))*100,1) ,")")
                ,NA,NA,NA,NA,NA)

AFFIRM<-rbind(AFFIRM_T,AFFIRM_N,AFFIRM_P)

colnames(AFFIRM)<-c("Number of randomized patients", "Number of patients with relapse of MS in two years, (%) ", "Mean Age (sd)", "Female N (%)", "Male N (%)", "Mean EDSS (sd)", "Median number of relapses one year prior to study (min,max)"  )

#For CONFIRM study:

CONFIRM_T<-as.data.frame(cbind(nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM"),]),
                               paste(nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM" & MSrelapse$RELAPSE2year==1),]), "(",
                                     round((nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM" & MSrelapse$RELAPSE2year==1),])/nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM"),]))*100,1), ")" )
                               , paste(round(mean(MSrelapse$AGE[which(MSrelapse$STUDYID=="CONFIRM")]),1), "(", round(sd((MSrelapse$AGE[which(MSrelapse$STUDYID=="CONFIRM")])),1), ")")
                               , paste(table(MSrelapse$SEX[which(MSrelapse$STUDYID=="CONFIRM")])[1], "(", round(
                                 (table(MSrelapse$SEX[which(MSrelapse$STUDYID=="CONFIRM")])[1]/nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM"),]))*100,
                                 1),
                                 ")"),  paste(table(MSrelapse$SEX[which(MSrelapse$STUDYID=="CONFIRM")])[2], "(", round(
                                   (table(MSrelapse$SEX[which(MSrelapse$STUDYID=="CONFIRM")])[2]/nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM"),]))*100,
                                   1),
                                   ")"), paste(round(mean(MSrelapse$EDSSBL[which(MSrelapse$STUDYID=="CONFIRM")]),1), "(", round(sd(MSrelapse$EDSSBL[which(MSrelapse$STUDYID=="CONFIRM")]),1), ")"),
                               paste(round(median(MSrelapse$RLPS1YR[which(MSrelapse$STUDYID=="CONFIRM")], na.rm = T),0), "(",
                                     exp(min(MSrelapse$RLPS1YR[which(MSrelapse$STUDYID=="CONFIRM")], na.rm=T))-1, ", ",
                                     exp(max(MSrelapse$RLPS1YR[which(MSrelapse$STUDYID=="CONFIRM")], na.rm=T))-1, ")")
))
CONFIRM_GA<-cbind(nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM" & MSrelapse$TRT01A=="Glatiramer acetate"),]),
                  paste(nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM" & MSrelapse$RELAPSE2year==1 & MSrelapse$TRT01A=="Glatiramer acetate"),]), "(",round((nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM" & MSrelapse$RELAPSE2year==1 & MSrelapse$TRT01A=="Glatiramer acetate"),])/nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM"  & MSrelapse$TRT01A=="Glatiramer acetate"),]))*100,1) ,")")
                  ,NA,NA,NA,NA,NA)

CONFIRM_DF<-cbind(nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM" & MSrelapse$TRT01A=="Dimethyl fumarate"),]),
                  paste(nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM" & MSrelapse$RELAPSE2year==1 & MSrelapse$TRT01A=="Dimethyl fumarate"),]), "(",round((nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM" & MSrelapse$RELAPSE2year==1 & MSrelapse$TRT01A=="Dimethyl fumarate"),])/nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM"  & MSrelapse$TRT01A=="Dimethyl fumarate"),]))*100,1) ,")")
                  ,NA,NA,NA,NA,NA)


CONFIRM_P<-cbind(nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM" & MSrelapse$TRT01A=="Placebo"),]),
                 paste(nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM" & MSrelapse$RELAPSE2year==1 & MSrelapse$TRT01A=="Placebo"),]), "(",round((nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM" & MSrelapse$RELAPSE2year==1 & MSrelapse$TRT01A=="Placebo"),])/nrow(MSrelapse[which(MSrelapse$STUDYID=="CONFIRM"  & MSrelapse$TRT01A=="Placebo"),]))*100,1) ,")")
                 ,NA,NA,NA,NA,NA)

CONFIRM<-as.data.frame(rbind(CONFIRM_T,CONFIRM_DF,CONFIRM_GA,CONFIRM_P))

colnames(CONFIRM)<-c("Number of randomized patients", "Number of patients with relapse of MS in two years, (%) ", "Mean Age (sd)", "Female N (%)", "Male N (%)", "Mean EDSS (sd)", "Median number of relapses one year prior to study (min,max)"  )

#For DEFINE study:

DEFINE_T<-as.data.frame(cbind(nrow(MSrelapse[which(MSrelapse$STUDYID=="DEFINE"),]),
                              paste(nrow(MSrelapse[which(MSrelapse$STUDYID=="DEFINE" & MSrelapse$RELAPSE2year==1),]), "(",
                                    round((nrow(MSrelapse[which(MSrelapse$STUDYID=="DEFINE" & MSrelapse$RELAPSE2year==1),])/nrow(MSrelapse[which(MSrelapse$STUDYID=="DEFINE"),]))*100,1), ")" )
                              , paste(round(mean(MSrelapse$AGE[which(MSrelapse$STUDYID=="DEFINE")]),1), "(", round(sd((MSrelapse$AGE[which(MSrelapse$STUDYID=="DEFINE")])),1), ")")
                              , paste(table(MSrelapse$SEX[which(MSrelapse$STUDYID=="DEFINE")])[1], "(", round(
                                (table(MSrelapse$SEX[which(MSrelapse$STUDYID=="DEFINE")])[1]/nrow(MSrelapse[which(MSrelapse$STUDYID=="DEFINE"),]))*100,
                                1),
                                ")"),  paste(table(MSrelapse$SEX[which(MSrelapse$STUDYID=="DEFINE")])[2], "(", round(
                                  (table(MSrelapse$SEX[which(MSrelapse$STUDYID=="DEFINE")])[2]/nrow(MSrelapse[which(MSrelapse$STUDYID=="DEFINE"),]))*100,
                                  1),
                                  ")"), paste(round(mean(MSrelapse$EDSSBL[which(MSrelapse$STUDYID=="DEFINE")], na.rm=T),1), "(", round(sd(MSrelapse$EDSSBL[which(MSrelapse$STUDYID=="DEFINE")], na.rm = T),1), ")"),
                              paste(round(median(MSrelapse$RLPS1YR[which(MSrelapse$STUDYID=="DEFINE")]),0), "(",
                                    exp(min(MSrelapse$RLPS1YR[which(MSrelapse$STUDYID=="DEFINE")]))-1, ", ",
                                    exp(max(MSrelapse$RLPS1YR[which(MSrelapse$STUDYID=="DEFINE")]))-1, ")")
))


DEFINE_DF<-cbind(nrow(MSrelapse[which(MSrelapse$STUDYID=="DEFINE" & MSrelapse$TRT01A=="Dimethyl fumarate"),]),
                 paste(nrow(MSrelapse[which(MSrelapse$STUDYID=="DEFINE" & MSrelapse$RELAPSE2year==1 & MSrelapse$TRT01A=="Dimethyl fumarate"),]), "(",round((nrow(MSrelapse[which(MSrelapse$STUDYID=="DEFINE" & MSrelapse$RELAPSE2year==1 & MSrelapse$TRT01A=="Dimethyl fumarate"),])/nrow(MSrelapse[which(MSrelapse$STUDYID=="DEFINE"  & MSrelapse$TRT01A=="Dimethyl fumarate"),]))*100,1) ,")")
                 ,NA,NA,NA,NA,NA)


DEFINE_P<-cbind(nrow(MSrelapse[which(MSrelapse$STUDYID=="DEFINE" & MSrelapse$TRT01A=="Placebo"),]),
                paste(nrow(MSrelapse[which(MSrelapse$STUDYID=="DEFINE" & MSrelapse$RELAPSE2year==1 & MSrelapse$TRT01A=="Placebo"),]), "(",round((nrow(MSrelapse[which(MSrelapse$STUDYID=="DEFINE" & MSrelapse$RELAPSE2year==1 & MSrelapse$TRT01A=="Placebo"),])/nrow(MSrelapse[which(MSrelapse$STUDYID=="DEFINE"  & MSrelapse$TRT01A=="Placebo"),]))*100,1) ,")")
                ,NA,NA,NA,NA,NA)

DEFINE<-rbind(DEFINE_T,DEFINE_DF,DEFINE_P)

colnames(DEFINE)<-c("Number of randomized patients", "Number of patients with relapse of MS in two years, (%) ", "Mean Age (sd)", "Female N (%)", "Male N (%)", "Mean EDSS (sd)", "Median number of relapses one year prior to study (min,max)"  )

#For Placebo study

Placebo<-cbind(nrow(PlaceboArms[which(!is.na(PlaceboArms$Relapse2year)),]),
               paste(nrow(PlaceboArms[which(!is.na(PlaceboArms$Relapse2year) & PlaceboArms$Relapse2year==1),]), "(",
                     round((nrow(PlaceboArms[which(!is.na(PlaceboArms$Relapse2year) & PlaceboArms$Relapse2year==1),])/nrow(PlaceboArms[which(!is.na(PlaceboArms$Relapse2year)),]))*100,1), ")"
               ),
               paste(round(mean(PlaceboArms$AGE[which(!is.na(PlaceboArms$Relapse2year))], na.rm=T),1), "(", round(sd((PlaceboArms$AGE[which(!is.na(PlaceboArms$Relapse2year))]), na.rm = T),1), ")"),
               paste(table(PlaceboArms$SEX[which(!is.na(PlaceboArms$Relapse2year))])[1], "(", round(
                 (table(PlaceboArms$SEX[which(!is.na(PlaceboArms$Relapse2year))])[1]/nrow(PlaceboArms[which(!is.na(PlaceboArms$Relapse2year)),]))*100,
                 1),
                 ")"),

               paste(table(PlaceboArms$SEX[which(!is.na(PlaceboArms$Relapse2year))])[2], "(", round(
                 (table(PlaceboArms$SEX[which(!is.na(PlaceboArms$Relapse2year))])[2]/nrow(PlaceboArms[which(!is.na(PlaceboArms$Relapse2year)),]))*100,
                 1),
                 ")"), NA, NA
)


colnames(Placebo)<-c("Number of randomized patients", "Number of patients with relapse of MS in two years, (%) ", "Mean Age (sd)", "Female N (%)", "Male N (%)", "Mean EDSS (sd)", "Median number of relapses one year prior to study (min,max)"  )


#results
AFFIRM
CONFIRM
DEFINE
Placebo

#### Table 2

#coefficients for LASSO model
x<-LASSOModel$lassomodel$coefficients
y<-PreSpecifiedModel$PreSpecifiedmodel$coefficients
x<-as.data.frame(x)
colnames(x)<-c("Coefficients")
rownames(x)<-c("Intercept", "Age", "Baseline Weight", "Baseline EDSS","No. of relapses 1 year prior to study", "Prior MS Treatment group", "REGION: India","REGION: North America","REGION: ROW", "REGION: Western Europe", "Baseline Gd volume", "Baseline SF-36 PCS", "Baseline Actual Distance Walked >500")
x<-round(x,4)
y<-c("Intercept", "Age", "Baseline Weight", "Baseline EDSS","No. of relapses 1 year prior to study", "Prior MS Treatment group", "REGION: India","REGION: North America","REGION: ROW", "REGION: Western Europe", "Baseline Gd volume", "Baseline SF-36 PCS", "Baseline Actual Distance Walked >500")
y<-as.data.frame(y)
Table_LASSOModel<-cbind(y,x)
rownames(Table_LASSOModel) <- c()
colnames(Table_LASSOModel)<-c("Variables","Coefficients")
Table_LASSOModel

#coefficients for pre-specified model
y<-PreSpecifiedModel$PreSpecifiedmodel$coefficients
y<-as.data.frame(y)
colnames(y)<-c("Coefficients")
y<-round(y,4)
z<-c("Intercept", "Age", "Sex (male vs female)", "Baseline EDSS","Years Since Onset of Symptoms","Ethnicity (white vs other)","No. of relapses 1 year prior to study","Months since pre-study relapse", "Prior MS Treatment group (yes vs no)","T25FW","9HPT","PASAT-3","VFT 2.5%", "Baseline SF-36 MCS", "Baseline SF-36 PCS")
z<-as.data.frame(z)
Table_PreSpecifiedModel<-cbind(z,y)
colnames(Table_PreSpecifiedModel)<-c("Variables","Coefficients")
rownames(Table_PreSpecifiedModel) <- c()
Table_PreSpecifiedModel


#### Table 3
gamma_LASSO<-IPDNMRJAGSresultsLASSO$BUGSoutput$summary[9,]
deltaDF_LASSO<-IPDNMRJAGSresultsLASSO$BUGSoutput$summary[5,]
deltaGA_LASSO<-IPDNMRJAGSresultsLASSO$BUGSoutput$summary[6,]
deltaN_LASSO<-IPDNMRJAGSresultsLASSO$BUGSoutput$summary[7,]
gammaDF_LASSO<-IPDNMRJAGSresultsLASSO$BUGSoutput$summary[10,]
gammaGA_LASSO<-IPDNMRJAGSresultsLASSO$BUGSoutput$summary[11,]
gammaN_LASSO<-IPDNMRJAGSresultsLASSO$BUGSoutput$summary[12,]


LASSOIPDNMR_Table<-rbind(gamma_LASSO,deltaDF_LASSO,deltaGA_LASSO,deltaN_LASSO,gammaDF_LASSO,gammaGA_LASSO,gammaN_LASSO)
LASSOIPDNMR_Table<-as.data.frame(LASSOIPDNMR_Table)
todrop<-c(2,4,5,6,8,9)
LASSOIPDNMR_Table<-LASSOIPDNMR_Table[,-todrop]
LASSOIPDNMR_Table<-round(LASSOIPDNMR_Table,2)


LASSOIPDNMR_Table$CredibleIntervals<-NA
for(i in 1:7){
  LASSOIPDNMR_Table[i,4]<-paste(LASSOIPDNMR_Table[i,1],"(",LASSOIPDNMR_Table[i,2],",", LASSOIPDNMR_Table[i,3], ")")
}
todrop<-c(1,2,3)
LASSOIPDNMR_Table<-LASSOIPDNMR_Table[,-todrop]
LASSOIPDNMR_Table<-as.data.frame(LASSOIPDNMR_Table)

gamma_Prespecified<-IPDNMRJAGSresultsPreSpecified$BUGSoutput$summary[9,]
deltaDF_Prespecified<-IPDNMRJAGSresultsPreSpecified$BUGSoutput$summary[5,]
deltaGA_Prespecified<-IPDNMRJAGSresultsPreSpecified$BUGSoutput$summary[6,]
deltaN_Prespecified<-IPDNMRJAGSresultsPreSpecified$BUGSoutput$summary[7,]
gammaDF_Prespecified<-IPDNMRJAGSresultsPreSpecified$BUGSoutput$summary[10,]
gammaGA_Prespecified<-IPDNMRJAGSresultsPreSpecified$BUGSoutput$summary[11,]
gammaN_Prespecified<-IPDNMRJAGSresultsPreSpecified$BUGSoutput$summary[12,]


PrespecifiedIPDNMR_Table<-rbind(gamma_Prespecified,deltaDF_Prespecified,deltaGA_Prespecified,deltaN_Prespecified,gammaDF_Prespecified,gammaGA_Prespecified,gammaN_Prespecified)
PrespecifiedIPDNMR_Table<-as.data.frame(PrespecifiedIPDNMR_Table)
todrop<-c(2,4,5,6,8,9)
PrespecifiedIPDNMR_Table<-PrespecifiedIPDNMR_Table[,-todrop]

PrespecifiedIPDNMR_Table<-round(PrespecifiedIPDNMR_Table,2)
PrespecifiedIPDNMR_Table$CredibleIntervals<-NA
for(i in 1:7){
  PrespecifiedIPDNMR_Table[i,4]<-paste(PrespecifiedIPDNMR_Table[i,1],"(",PrespecifiedIPDNMR_Table[i,2],",", PrespecifiedIPDNMR_Table[i,3], ")")
}

todrop<-c(1,2,3)
PrespecifiedIPDNMR_Table<-PrespecifiedIPDNMR_Table[,-todrop]
PrespecifiedIPDNMR_Table<-as.data.frame(PrespecifiedIPDNMR_Table)

IPDNMR_Table<-cbind(LASSOIPDNMR_Table,PrespecifiedIPDNMR_Table)
colnames(IPDNMR_Table)<-c("LASSO model Mean (95% Cr. Interval)", "Pre-specified model Mean (95% Cr. Interval)" )
rownames(IPDNMR_Table)<-c("g0","delta_DF","delta_GA","delta_N", "gamma_DF","gamma_GA","gamma_N")

#### Table 4
###absolute benefits
#all these tables have been created in the GraphForPredictedRisk.R script
LASSOtable
PreSpecifiedtable
###ORs
LASSOtableOR
PreSpecifiedtableOR


############################   FIGURES   ##############################################################################


#### Figure 1

#all these figures have been created in the GraphForPredictedRisk.R script
ggarrange(PrognosticRiskLASSO,PrognosticRiskPreSpecified,ncol = 1,nrow = 2,labels = c("A  LASSO model","B  Pre-specified model"), hjust=-0.2,font.label = list(size = 11))


#### Figure 2

#all these figures have been created in the GraphForPredictedRisk.R script
ggarrange(IPDplotLASSO,IPDplotPreSpecified,ncol = 1,nrow = 2,labels = c("A","B"),font.label = list(size = 11))

################################## Appendix figures ############################################


### ORs plot

#all these figures have been created in the GraphForPredictedRisk.R script
ggarrange(IPDplotLASSO_OR,IPDplotPreSpecified_OR,ncol = 1,nrow = 2,labels = c("A","B"),font.label = list(size = 11))

### Flow-chart Appendix

library(DiagrammeR)


Flowchart<-grViz("digraph flowchart {
                 # node definitions with substituted label text
                 node [fontname = Helvetica, shape = rectangle]
                 tab1 [label = '@@1']
                 tab2 [label = '@@2']
                 tab3 [label = '@@3']
                 tab4 [label = '@@4']
                 tab5 [label = '@@5']
                 # edge definitions with the node IDs
                 tab1 -> tab2 -> tab3 ;
                 tab3->tab4;
                 tab3->tab5;

                 }

                 [1]: 'Number of prognostic factors, np=57'
                 [2]: 'Number of prognostic factors with missing data less than 50%, np=53'
                 [3]: 'Number of prognostic factors correlated less than 70%, np=31 '
                 [4]: 'Number of prognostic factors in LASSO model, np=9 '
                 [5]: 'Number of prognostic factors in pre-specified model, np=14 '

                 ")


