################# Script for Plots of predicted Risks and ORs#########################

######################################################################################################
###################           Predicted risk           ################################################
##########################################################################################################

########################################## FOR LASSO MODEL   ######################################
###predicted risk= expit(logit p risk)
expit<-function(x) {exp(x)/(1+exp(x))}
p<-IPDNMRJAGSmodelLASSO$BUGSoutput$mean$logitp
p<-expit(IPDNMRJAGSmodelLASSO$BUGSoutput$mean$logitp)

####preperation for the graph
###For Dymethyl fumarate- Risk & propability to relapse
DFL<-cbind(Risknew,p[,1])
DFL$Treatment<-"Dimethyl Fumarate"
colnames(DFL)<-c("Risknew","prelapse","Treatment")
##For Glatimarer acetate  - Risk & propability to relapse
GAL<-cbind(Risknew,p[,2])
GAL$Treatment<-"Glatiramer Acetate"
colnames(GAL)<-c("Risknew","prelapse","Treatment")
##For Natalizumab acetate - Risk & propability to relapse
NL<-cbind(Risknew,p[,3])
NL$Treatment<-"Natalizumab"
colnames(NL)<-c("Risknew","prelapse","Treatment")
##For Placebo - Risk & propability to relapse
PlL<-cbind(Risknew,p[,4])
PlL$Treatment<-"Placebo"
colnames(PlL)<-c("Risknew","prelapse","Treatment")

##merge data for all the treatments
GraphdataL<-rbind(DFL,GAL,NL,PlL)

###Graph for IPD
ggplot(data=GraphdataL, aes(x=Risknew, y=prelapse, group=Treatment)) +
  geom_line()+
  geom_point()+
  geom_vline(xintercept=0.2197, linetype="dashed", color = "red")+
  geom_vline(xintercept=0.6162, linetype="dashed", color = "red")

IPDplotLASSO<-ggplot(GraphdataL, aes(x=Risknew, y=prelapse, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))+geom_vline(xintercept=0.2197, linetype="dashed", color = "red")+
  geom_vline(xintercept=0.6162, linetype="dashed", color = "red")
IPDplotLASSO<-IPDplotLASSO+ xlab("Baseline risk") + ylab("Predicted probability of relapse")

IPDplotLASSO

######################################################################################################

################################# For PreSpecified model #################################################

###predicted risk= expit(logit p risk)
expit<-function(x) {exp(x)/(1+exp(x))}
p<-IPDNMRJAGSmodelPreSpecified$BUGSoutput$mean$logitp
p<-expit(IPDNMRJAGSmodelPreSpecified$BUGSoutput$mean$logitp)

####preperation for the graph
###For Dymethyl fumarate- Risk & propability to relapse
DFF<-cbind(Risknew,p[,1])
DFF$Treatment<-"Dimethyl Fumarate"
colnames(DFF)<-c("Risknew","prelapse","Treatment")
##For Glatimarer acetate  - Risk & propability to relapse
GAF<-cbind(Risknew,p[,2])
GAF$Treatment<-"Glatiramer Acetate"
colnames(GAF)<-c("Risknew","prelapse","Treatment")
##For Natalizumab acetate - Risk & propability to relapse
NF<-cbind(Risknew,p[,3])
NF$Treatment<-"Natalizumab"
colnames(NF)<-c("Risknew","prelapse","Treatment")
##For Placebo - Risk & propability to relapse
PlF<-cbind(Risknew,p[,4])
PlF$Treatment<-"Placebo"
colnames(PlF)<-c("Risknew","prelapse","Treatment")

##merge data for all the treatments
GraphdataF<-rbind(DFF,GAF,NF,PlF)

###Graph for IPD
ggplot(data=GraphdataF, aes(x=Risknew, y=prelapse, group=Treatment)) +
  geom_line()+
  geom_point()+geom_vline(xintercept=0.05113, linetype="dashed", color = "red")+
  geom_vline(xintercept=0.72461, linetype="dashed", color = "red")

IPDplotPreSpecified<-ggplot(GraphdataF, aes(x=Risknew, y=prelapse, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))+geom_vline(xintercept=0.05113, linetype="dashed", color = "red")+
  geom_vline(xintercept=0.72461, linetype="dashed", color = "red")
IPDplotPreSpecified<-IPDplotPreSpecified+ xlab("Baseline risk") + ylab("Predicted probability of relapse")


######################################################################################################
###################           OR          ################################################
##########################################################################################################

##################### LASSO model ################################

pl<-IPDNMRJAGSmodelLASSO$BUGSoutput$mean$logitp

####preperation for the graph
###For Dymethyl fumarate- Risk & propability to relapse
DFL_OR<-cbind(Risknew,exp(pl[,1]-pl[,4]))
DFL_OR$Treatment<-"Dimethyl Fumarate vs Placebo"
colnames(DFL_OR)<-c("Risknew","OR","Comparison")
##For Glatimarer acetate  - Risk & propability to relapse
GAL_OR<-cbind(Risknew,exp(pl[,2]-pl[,4]))
GAL_OR$Treatment<-"Glatiramer Acetate vs Placebo"
colnames(GAL_OR)<-c("Risknew","OR","Comparison")
##For Natalizumab acetate - Risk & propability to relapse
NL_OR<-cbind(Risknew,exp(pl[,3]-pl[,4]))
NL_OR$Treatment<-"Natalizumab vs Placebo"
colnames(NL_OR)<-c("Risknew","OR","Comparison")
##For Placebo - Risk & propability to relapse
#PlL_OR<-cbind(Risknew,exp(pl[,4]-pl[,4]))
#PlL_OR$Treatment<-"Placebo"
#colnames(PlL_OR)<-c("Risknew","OR","Treatment")

##merge data for all the treatments
GraphdataL_OR<-rbind(DFL_OR,GAL_OR,NL_OR)

###Graph for IPD
ggplot(data=GraphdataL_OR, aes(x=Risknew, y=OR, group=Comparison)) +
  geom_line()+
  geom_point() + ylim(0,2) + geom_vline(xintercept=0.2197, linetype="dashed", color = "red") + geom_hline(yintercept=1, color="black") +
  geom_vline(xintercept=0.6162, linetype="dashed", color = "red")


IPDplotLASSO_OR<-ggplot(GraphdataL_OR, aes(x=Risknew, y=OR, group=Comparison)) +
  geom_line(aes(color=Comparison))+
  geom_point(aes(color=Comparison))+ ylim(0,2) +geom_vline(xintercept= 0.2197, linetype="dashed", color = "red")+ geom_hline(yintercept=1, color="black") +
  geom_vline(xintercept=0.6162, linetype="dashed", color = "red")
IPDplotLASSO_OR<-IPDplotLASSO_OR+ xlab("Baseline risk") + ylab("OR")

IPDplotLASSO_OR

######################################################################################################

################################# For PreSpecified model #################################################

###predicted risk= expit(logit p risk)
pf<-IPDNMRJAGSmodelPreSpecified$BUGSoutput$mean$logitp

####preperation for the graph
###For Dymethyl fumarate- Risk & propability to relapse
DF_OR<-cbind(Risknew,exp(pf[,1]-pf[,4]))
DF_OR$Treatment<-"Dimethyl Fumarate vs Placebo"
colnames(DF_OR)<-c("Risknew","OR","Comparison")
##For Glatimarer acetate  - Risk & propability to relapse
GA_OR<-cbind(Risknew,exp(pf[,2]-pf[,4]))
GA_OR$Treatment<-"Glatiramer Acetate vs Placebo"
colnames(GA_OR)<-c("Risknew","OR","Comparison")
##For Natalizumab acetate - Risk & propability to relapse
N_OR<-cbind(Risknew,exp(pf[,3]-pf[,4]))
N_OR$Treatment<-"Natalizumab vs Placebo"
colnames(N_OR)<-c("Risknew","OR","Comparison")
##For Placebo - Risk & propability to relapse
#Pl_OR<-cbind(Risknew,exp(pf[,4]-pf[,4]))
#Pl_OR$Treatment<-"Placebo"
#colnames(Pl_OR)<-c("Risknew","OR","Treatment")

##merge data for all the treatments
GraphdataF_OR<-rbind(DF_OR,GA_OR,N_OR)

###Graph for IPD
ggplot(data=GraphdataF_OR, aes(x=Risknew, y=OR, group=Comparison)) +
  geom_line()+
  geom_point()+ ylim(0,2)+geom_vline(xintercept=0.05113, linetype="dashed", color = "red") + geom_hline(yintercept=1, color="black") +
  geom_vline(xintercept=0.72461, linetype="dashed", color = "red")

IPDplotPreSpecified_OR<-ggplot(GraphdataF_OR, aes(x=Risknew, y=OR, group=Comparison)) +
  geom_line(aes(color=Comparison))+
  geom_point(aes(color=Comparison))+ ylim(0,2)+geom_vline(xintercept=0.05113, linetype="dashed", color = "red")+ geom_hline(yintercept=1, color="black") +
  geom_vline(xintercept=0.72461, linetype="dashed", color = "red")
IPDplotPreSpecified_OR<-IPDplotPreSpecified_OR+ xlab("Baseline risk") + ylab("OR")

IPDplotPreSpecified_OR


######################   TABLES OF predicted risks by risk-group ###################################

mean1<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.22 & GraphdataL$Risknew<=0.616 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.22 & GraphdataL$Risknew<=0.616 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.22 & GraphdataL$Risknew<=0.616 & GraphdataL$Treatment=="Natalizumab")])[4]*100

mean1low<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.22 & GraphdataL$Risknew<0.3 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2low<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.22 & GraphdataL$Risknew<0.3 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3low<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.22 & GraphdataL$Risknew<0.3 & GraphdataL$Treatment=="Natalizumab")])[4]*100

mean1high<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.616 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2high<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.616 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3high<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.616 & GraphdataL$Treatment=="Natalizumab")])[4]*100

LASSOtable<-data.frame("Average predicted relapse "=c(mean1, mean2, mean3), "Low-isk patients' (<30%) predicted relapse"=c(mean1low,mean2low,mean3low)
                       , "High-isk patients' (>50%) predicted relapse "=c(mean1high,mean2high,mean3high))
names(LASSOtable)<-c("Average predicted relapse %","Low-isk patients' (<30%) predicted relapse %","High-isk patients' (>50%) predicted relapse %")
rownames(LASSOtable)<-c("Dymethyl fumarate","Glatiramer acetate","Natalizumab")


mean1f<- summary(GraphdataF$prelapse[which(GraphdataF$Risknew>=0.05 & GraphdataF$Risknew<=0.725 & GraphdataF$Treatment=="Dimethyl Fumarate")])[4]*100
mean2f<- summary(GraphdataF$prelapse[which(GraphdataF$Risknew>=0.05 & GraphdataF$Risknew<=0.725 & GraphdataF$Treatment=="Glatiramer Acetate")])[4]*100
mean3f<- summary(GraphdataF$prelapse[which(GraphdataF$Risknew>=0.05 & GraphdataF$Risknew<=0.725 & GraphdataF$Treatment=="Natalizumab")])[4]*100

mean1lowf<- summary(GraphdataF$prelapse[which(GraphdataF$Risknew>=0.05 & GraphdataF$Risknew<0.3 & GraphdataF$Treatment=="Dimethyl Fumarate")])[4]*100
mean2lowf<- summary(GraphdataF$prelapse[which(GraphdataF$Risknew>=0.05 & GraphdataF$Risknew<0.3 & GraphdataF$Treatment=="Glatiramer Acetate")])[4]*100
mean3lowf<- summary(GraphdataF$prelapse[which(GraphdataF$Risknew>=0.05 & GraphdataF$Risknew<0.3 & GraphdataF$Treatment=="Natalizumab")])[4]*100

mean1highf<- summary(GraphdataF$prelapse[which(GraphdataF$Risknew>0.5 & GraphdataF$Risknew<=0.725  & GraphdataF$Treatment=="Dimethyl Fumarate")])[4]*100
mean2highf<- summary(GraphdataF$prelapse[which(GraphdataF$Risknew>0.5 & GraphdataF$Risknew<=0.725  & GraphdataF$Treatment=="Glatiramer Acetate")])[4]*100
mean3highf<- summary(GraphdataF$prelapse[which(GraphdataF$Risknew>0.5 & GraphdataF$Risknew<=0.725  & GraphdataF$Treatment=="Natalizumab")])[4]*100

PreSpecifiedtable<-data.frame("Average predicted relapse"=c(mean1f, mean2f, mean3f), "Low-isk patients' (<30%) predicted relapse"=c(mean1lowf,mean2lowf,mean3lowf)
                       , "High-isk patients' (>50%) predicted relapse"=c(mean1highf,mean2highf,mean3highf))
names(PreSpecifiedtable)<-c("Average predicted relapse %","Low-isk patients' (<30%) predicted relapse %","High-isk patients' (>50%) predicted relapse %")
rownames(PreSpecifiedtable)<-c("Dymethyl fumarate","Glatiramer acetate","Natalizumab")




######################   TABLES OF ORs by risk-group ###################################

mean1OR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.22 & GraphdataL_OR$Risknew<=0.616 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2OR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.22 & GraphdataL_OR$Risknew<=0.616 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3OR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.22 & GraphdataL_OR$Risknew<=0.616 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1lowOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.22 & GraphdataL_OR$Risknew<0.3 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2lowOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.22 & GraphdataL_OR$Risknew<0.3 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3lowOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.22 & GraphdataL_OR$Risknew<0.3 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1highOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.616 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2highOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.616 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3highOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.616 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

LASSOtableOR<-data.frame("Average OR "=c(mean1OR, mean2OR, mean3OR), "Low-isk patients' (<30%) mean OR"=c(mean1lowOR,mean2lowOR,mean3lowOR)
                       , "High-isk patients' (>50%) predicted relapse "=c(mean1highOR,mean2highOR,mean3highOR))
names(LASSOtableOR)<-c("Average OR","Low-isk patients' (<30%) OR","High-isk patients' (>50%) OR ")
rownames(LASSOtableOR)<-c("Dymethyl fumarate","Glatiramer acetate","Natalizumab")


mean1ORF<- summary(GraphdataF_OR$OR[which(GraphdataF_OR$Risknew>=0.05 & GraphdataF_OR$Risknew<=0.725 & GraphdataF_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2ORF<- summary(GraphdataF_OR$OR[which(GraphdataF_OR$Risknew>=0.05 & GraphdataF_OR$Risknew<=0.725 & GraphdataF_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3ORF<- summary(GraphdataF_OR$OR[which(GraphdataF_OR$Risknew>=0.05 & GraphdataF_OR$Risknew<=0.725 & GraphdataF_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1lowORF<- summary(GraphdataF_OR$OR[which(GraphdataF_OR$Risknew>=0.05 & GraphdataF_OR$Risknew<0.3 & GraphdataF_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2lowORF<- summary(GraphdataF_OR$OR[which(GraphdataF_OR$Risknew>=0.05 & GraphdataF_OR$Risknew<0.3 & GraphdataF_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3lowORF<- summary(GraphdataF_OR$OR[which(GraphdataF_OR$Risknew>=0.05 & GraphdataF_OR$Risknew<0.3 & GraphdataF_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1highORF<- summary(GraphdataF_OR$OR[which(GraphdataF_OR$Risknew>0.5 & GraphdataF_OR$Risknew<=0.725 & GraphdataF_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2highORF<- summary(GraphdataF_OR$OR[which(GraphdataF_OR$Risknew>0.5 & GraphdataF_OR$Risknew<=0.725 & GraphdataF_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3highORF<- summary(GraphdataF_OR$OR[which(GraphdataF_OR$Risknew>0.5 & GraphdataF_OR$Risknew<=0.725 & GraphdataF_OR$Comparison=="Natalizumab vs Placebo")])[4]

PreSpecifiedtableOR<-data.frame("Average OR "=c(mean1ORF, mean2ORF, mean3ORF), "Low-isk patients' (<30%) mean OR"=c(mean1lowORF,mean2lowORF,mean3lowORF)
                         , "High-isk patients' (>50%) predicted relapse "=c(mean1highORF,mean2highORF,mean3highORF))
names(PreSpecifiedtableOR)<-c("Average OR","Low-isk patients' (<30%) OR","High-isk patients' (>50%) OR ")
rownames(PreSpecifiedtableOR)<-c("Dymethyl fumarate","Glatiramer acetate","Natalizumab")


