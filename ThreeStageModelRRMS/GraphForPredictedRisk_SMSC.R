######################################################################################
###  Swiss cohort setting: GRAPH with the predicted probabilities (&ORs)   ###########
# to relapse under each available treatment option   ###################
###############################################################################



expit<-function(x) {exp(x)/(1+exp(x))}
p2<-IPDADNMRJAGSresultsSMSC$BUGSoutput$mean$logitp2
p2<-expit(IPDADNMRJAGSresultsSMSC$BUGSoutput$mean$logitp2)
y<-IPDADNMRJAGSresultsSMSC$BUGSoutput$summary[408:803,]

low<-y[,3]
low1<-low[1:99]
low2<-low[100:198]
low3<-low[199:297]
low4<-low[298:396]
high<-y[,7]
high1<-high[1:99]
high2<-high[100:198]
high3<-high[199:297]
high4<-high[298:396]

low1<-expit(low1)
low2<-expit(low2)
low3<-expit(low3)
low4<-expit(low4)

high1<-expit(high1)
high2<-expit(high2)
high3<-expit(high3)
high4<-expit(high4)

####preperation for the graph
###For Dymethyl fumarate- Risk & propability to relapse
DFL<-cbind(Risknew1,p2[,1], low1, high1)
DFL$Treatment<-"Dimethyl Fumarate"
colnames(DFL)<-c("Risknew","prelapse","lowCI", "highCI","Treatment")
##For Glatimarer acetate  - Risk & propability to relapse
GAL<-cbind(Risknew1,p2[,2], low2, high2)
GAL$Treatment<-"Glatiramer Acetate"
colnames(GAL)<-c("Risknew","prelapse","lowCI", "highCI","Treatment")
##For Natalizumab acetate - Risk & propability to relapse
NL<-cbind(Risknew1,p2[,3], low3, high3)
NL$Treatment<-"Natalizumab"
colnames(NL)<-c("Risknew","prelapse","lowCI", "highCI","Treatment")
##For Placebo - Risk & propability to relapse
PlL<-cbind(Risknew1,p2[,4], low4, high4)
PlL$Treatment<-"Placebo"
colnames(PlL)<-c("Risknew","prelapse","lowCI", "highCI","Treatment")

##merge data for all the treatments
GraphdataL<-rbind(DFL,GAL,NL,PlL)

#Graphdata_SMSC<-GraphdataL[,c(1,2,5)]
#colnames(Graphdata_SMSC)<-c( "Baseline risk","Predicted probability of relapsing within the next two years %","Treatment")
#Graphdata_SMSC$Treatment<-as.factor(Graphdata_SMSC$Treatment)
#write.csv(Graphdata_SMSC,"C:/Users/kc19o338/Documents/GitHub/shinies/apps/test/GraphData_SMSC.csv")
###Graph for IPD (predicted probabilities)

Graph_SMSC<-ggplot(GraphdataL, aes(x=Risknew, y=prelapse, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))+geom_vline(xintercept=0.03925, linetype="dashed", color = "red")+
  geom_vline(xintercept=0.66126 , linetype="dashed", color = "red")+xlab("Baseline risk") + ylab("Predicted probability of relapse")

######################   TABLES OF predicted risks by risk-group ###################################

mean1<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Natalizumab")])[4]*100

mean1_lowCI<- summary(GraphdataL$lowCI[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2_lowCI<- summary(GraphdataL$lowCI[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3_lowCI<- summary(GraphdataL$lowCI[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Natalizumab")])[4]*100

mean1_highCI<- summary(GraphdataL$highCI[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2_highCI<- summary(GraphdataL$highCI[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3_highCI<- summary(GraphdataL$highCI[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Natalizumab")])[4]*100


mean1low<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<0.25 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2low<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<0.25 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3low<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<0.25 & GraphdataL$Treatment=="Natalizumab")])[4]*100


mean1low_lowCI<- summary(GraphdataL$lowCI[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<0.25 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2low_lowCI<- summary(GraphdataL$lowCI[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<0.25 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3low_lowCI<- summary(GraphdataL$lowCI[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<0.25 & GraphdataL$Treatment=="Natalizumab")])[4]*100


mean1low_highCI<- summary(GraphdataL$highCI[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<0.25 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2low_highCI<- summary(GraphdataL$highCI[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<0.25 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3low_highCI<- summary(GraphdataL$highCI[which(GraphdataL$Risknew>=0.03925 & GraphdataL$Risknew<0.25 & GraphdataL$Treatment=="Natalizumab")])[4]*100

mean1high<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2high<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3high<- summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Natalizumab")])[4]*100

mean1high_lowCI<- summary(GraphdataL$lowCI[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2high_lowCI<- summary(GraphdataL$lowCI[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3high_lowCI<- summary(GraphdataL$lowCI[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Natalizumab")])[4]*100

mean1high_highCI<- summary(GraphdataL$highCI[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100
mean2high_highCI<- summary(GraphdataL$highCI[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100
mean3high_highCI<- summary(GraphdataL$highCI[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.66126 & GraphdataL$Treatment=="Natalizumab")])[4]*100

Absolutetable<-data.frame("Average predicted relapse "=c(mean1, mean2, mean3), "Low-isk patients' (<25%) predicted relapse"=c(mean1low,mean2low,mean3low)
                          , "High-isk patients' (>50%) predicted relapse "=c(mean1high,mean2high,mean3high))
names(Absolutetable)<-c("Average predicted relapse %","Low-isk patients' (<25%) predicted relapse %","High-isk patients' (>50%) predicted relapse %")
rownames(Absolutetable)<-c("Dymethyl fumarate","Glatiramer acetate","Natalizumab")

x<-SMSCdataC[SMSCdataC$Risknew<0.25,]
nrow(x) ##77,3 low-risk patient

y<-SMSCdataC[SMSCdataC$Risknew>0.50,]
nrow(y) ##1% high risk patients

######################   Graph OF ORs###################################


pl2<-IPDADNMRJAGSresultsSMSC$BUGSoutput$mean$logitp2

high1<-logit(high1)
high2<-logit(high2)
high3<-logit(high3)
high4<-logit(high4)

low1<-logit(low1)
low2<-logit(low2)
low3<-logit(low3)
low4<-logit(low4)
####preperation for the graph
###For Dymethyl fumarate- Risk & propability to relapse
DFL_OR<-cbind(Risknew1,exp(pl2[,1]-pl2[,4]), exp(low1-low4), exp(high1-high4))
DFL_OR$Treatment<-"Dimethyl Fumarate vs Placebo"
colnames(DFL_OR)<-c("Risknew","OR","lowCI","highCI","Comparison")
##For Glatimarer acetate  - Risk & propability to relapse
GAL_OR<-cbind(Risknew1,exp(pl2[,2]-pl2[,4]), exp(low2-low4), exp(high2-high4))
GAL_OR$Treatment<-"Glatiramer Acetate vs Placebo"
colnames(GAL_OR)<-c("Risknew","OR","lowCI","highCI","Comparison")
##For Natalizumab acetate - Risk & propability to relapse
NL_OR<-cbind(Risknew1,exp(pl2[,3]-pl2[,4]), exp(low3-low4), exp(high3-high4))
NL_OR$Treatment<-"Natalizumab vs Placebo"
colnames(NL_OR)<-c("Risknew","OR","lowCI","highCI","Comparison")
##For Placebo - Risk & propability to relapse
#PlL_OR<-cbind(Risknew,exp(pl[,4]-pl[,4]))
#PlL_OR$Treatment<-"Placebo"
#colnames(PlL_OR)<-c("Risknew","OR","Treatment")

##merge data for all the treatments
GraphdataL_OR<-rbind(DFL_OR,GAL_OR,NL_OR)

###plot for ORs

Plot_OR<-ggplot(GraphdataL_OR, aes(x=Risknew, y=OR, group=Comparison)) +
  geom_line(aes(color=Comparison))+
  geom_point(aes(color=Comparison))+ ylim(0,2) +geom_vline(xintercept= 0.03925, linetype="dashed", color = "red")+ geom_hline(yintercept=1, color="black") +
  geom_vline(xintercept=0.66126, linetype="dashed", color = "red")
Plot_OR<-Plot_OR+ xlab("Baseline risk") + ylab("OR")

Plot_OR

###Table with the ORs
### in different risk groups

mean1OR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2OR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3OR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1OR_lowCI<- summary(GraphdataL_OR$lowCI[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2OR_lowCI<- summary(GraphdataL_OR$lowCI[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3OR_lowCI<- summary(GraphdataL_OR$lowCI[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1OR_highCI<- summary(GraphdataL_OR$highCI[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2OR_highCI<- summary(GraphdataL_OR$highCI[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3OR_highCI<- summary(GraphdataL_OR$highCI[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]


mean1lowOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<0.25 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2lowOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<0.25 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3lowOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<0.25 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]


mean1lowOR_lowCI<- summary(GraphdataL_OR$lowCI[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<0.25 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2lowOR_lowCI<- summary(GraphdataL_OR$lowCI[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<0.25 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3lowOR_lowCI<- summary(GraphdataL_OR$lowCI[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<0.25 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]


mean1lowOR_highCI<- summary(GraphdataL_OR$highCI[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<0.25 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2lowOR_highCI<- summary(GraphdataL_OR$highCI[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<0.25 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3lowOR_highCI<- summary(GraphdataL_OR$highCI[which(GraphdataL_OR$Risknew>=0.03925 & GraphdataL_OR$Risknew<0.25 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1highOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2highOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3highOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1highOR_lowCI<- summary(GraphdataL_OR$lowCI[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2highOR_lowCI<- summary(GraphdataL_OR$lowCI[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3highOR_lowCI<- summary(GraphdataL_OR$lowCI[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1highOR_highCI<- summary(GraphdataL_OR$highCI[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2highOR_highCI<- summary(GraphdataL_OR$highCI[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3highOR_highCI<- summary(GraphdataL_OR$highCI[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.66126 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

ORtable<-data.frame("Average OR "=c(mean1OR, mean2OR, mean3OR), "Low-isk patients' (<25%) mean OR"=c(mean1lowOR,mean2lowOR,mean3lowOR)
                    , "High-isk patients' (>50%) predicted relapse "=c(mean1highOR,mean2highOR,mean3highOR))
names(ORtable)<-c("Average OR","Low-isk patients' (<25%) OR","High-isk patients' (>50%) OR ")
rownames(ORtable)<-c("Dymethyl fumarate","Glatiramer acetate","Natalizumab")

###Distribution of risk score in the entire dataset of SMSC
RiskDistribution<-ggdensity(SMSCdataC, x = "Risknew",
                            fill = "blue", color = "blue",
                            add = "mean", rug = TRUE, xlim=c(0,1.15))
ggarrange(Graph,RiskDistribution,ncol=1,nrow=2)
