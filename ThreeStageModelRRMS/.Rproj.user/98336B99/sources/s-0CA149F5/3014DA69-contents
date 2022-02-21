######################################################################################
###  GRAPH with the predicted probabilities (&ORs)   ###########
# to relapse under each available treatment option   ###################
###############################################################################



expit<-function(x) {exp(x)/(1+exp(x))}
p<-IPDADNMRJAGSresultsSMSC$BUGSoutput$mean$logitp
p<-expit(IPDADNMRJAGSresultsSMSC$BUGSoutput$mean$logitp)

####preperation for the graph
###For Dymethyl fumarate- Risk & propability to relapse
DFL<-cbind(Risknew1,p[,1])
DFL$Treatment<-"Dimethyl Fumarate"
colnames(DFL)<-c("Risknew","prelapse","Treatment")
##For Glatimarer acetate  - Risk & propability to relapse
GAL<-cbind(Risknew1,p[,2])
GAL$Treatment<-"Glatiramer Acetate"
colnames(GAL)<-c("Risknew","prelapse","Treatment")
##For Natalizumab acetate - Risk & propability to relapse
NL<-cbind(Risknew1,p[,3])
NL$Treatment<-"Natalizumab"
colnames(NL)<-c("Risknew","prelapse","Treatment")
##For Placebo - Risk & propability to relapse
PlL<-cbind(Risknew1,p[,4])
PlL$Treatment<-"Placebo"
colnames(PlL)<-c("Risknew","prelapse","Treatment")

##merge data for all the treatments
GraphdataL<-rbind(DFL,GAL,NL,PlL)


###Graph for IPD (predicted probabilities)

Graph<-ggplot(GraphdataL, aes(x=Risknew, y=prelapse, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))+geom_vline(xintercept=0.1265625, linetype="dashed", color = "red")+
  geom_vline(xintercept=0.7040377, linetype="dashed", color = "red")+xlab("Baseline risk") + ylab("Predicted probability of relapse")

######################   TABLES OF predicted risks by risk-group ###################################

mean1<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Natalizumab")])[4]*100

mean1low<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<0.25 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2low<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<0.25 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3low<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<0.25 & GraphdataL$Treatment=="Natalizumab")])[4]*100

mean1high<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2high<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3high<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Natalizumab")])[4]*100

Absolutetable<-data.frame("Average predicted relapse "=c(mean1, mean2, mean3), "Low-isk patients' (<25%) predicted relapse"=c(mean1low,mean2low,mean3low)
                       , "High-isk patients' (>50%) predicted relapse "=c(mean1high,mean2high,mean3high))
names(Absolutetable)<-c("Average predicted relapse %","Low-isk patients' (<25%) predicted relapse %","High-isk patients' (>50%) predicted relapse %")
rownames(Absolutetable)<-c("Dymethyl fumarate","Glatiramer acetate","Natalizumab")

######################   Graph OF ORs###################################


pl<-IPDADNMRJAGSresultsSMSC$BUGSoutput$mean$logitp

####preperation for the graph
###For Dymethyl fumarate- Risk & propability to relapse
DFL_OR<-cbind(Risknew1,exp(pl[,1]-pl[,4]))
DFL_OR$Treatment<-"Dimethyl Fumarate vs Placebo"
colnames(DFL_OR)<-c("Risknew","OR","Comparison")
##For Glatimarer acetate  - Risk & propability to relapse
GAL_OR<-cbind(Risknew1,exp(pl[,2]-pl[,4]))
GAL_OR$Treatment<-"Glatiramer Acetate vs Placebo"
colnames(GAL_OR)<-c("Risknew","OR","Comparison")
##For Natalizumab acetate - Risk & propability to relapse
NL_OR<-cbind(Risknew1,exp(pl[,3]-pl[,4]))
NL_OR$Treatment<-"Natalizumab vs Placebo"
colnames(NL_OR)<-c("Risknew","OR","Comparison")
##For Placebo - Risk & propability to relapse
#PlL_OR<-cbind(Risknew,exp(pl[,4]-pl[,4]))
#PlL_OR$Treatment<-"Placebo"
#colnames(PlL_OR)<-c("Risknew","OR","Treatment")

##merge data for all the treatments
GraphdataL_OR<-rbind(DFL_OR,GAL_OR,NL_OR)

###plot for ORs

Plot_OR<-ggplot(GraphdataL_OR, aes(x=Risknew, y=OR, group=Comparison)) +
  geom_line(aes(color=Comparison))+
  geom_point(aes(color=Comparison))+ ylim(0,2) +geom_vline(xintercept= 0.1265625, linetype="dashed", color = "red")+ geom_hline(yintercept=1, color="black") +
  geom_vline(xintercept=0.7040377, linetype="dashed", color = "red")
Plot_OR<-Plot_OR+ xlab("Baseline risk") + ylab("OR")

Plot_OR

###Table with the ORs
### in different risk groups

mean1OR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2OR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3OR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1lowOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<0.25 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2lowOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<0.25 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3lowOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<0.25 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1highOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2highOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3highOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

ORtable<-data.frame("Average OR "=c(mean1OR, mean2OR, mean3OR), "Low-isk patients' (<25%) mean OR"=c(mean1lowOR,mean2lowOR,mean3lowOR)
                         , "High-isk patients' (>50%) predicted relapse "=c(mean1highOR,mean2highOR,mean3highOR))
names(ORtable)<-c("Average OR","Low-isk patients' (<25%) OR","High-isk patients' (>50%) OR ")
rownames(ORtable)<-c("Dymethyl fumarate","Glatiramer acetate","Natalizumab")

