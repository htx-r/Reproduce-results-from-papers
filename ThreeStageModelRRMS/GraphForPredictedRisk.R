######################################################################################
###  RCTs setting: GRAPH with the predicted probabilities (&ORs)   ###########
# to relapse under each available treatment option   ###################
###############################################################################



expit<-function(x) {exp(x)/(1+exp(x))}
p<-IPDADNMRJAGSresultsSMSC$BUGSoutput$mean$logitp
p<-expit(IPDADNMRJAGSresultsSMSC$BUGSoutput$mean$logitp)

p_lower<-expit(IPDADNMRJAGSresultsSMSC$BUGSoutput$summary[12:407,3])
p_lower1<-p_lower[1:99]
p_lower2<-p_lower[100:198]
p_lower3<-p_lower[199:297]
p_lower4<-p_lower[298:396]

p_upper<-expit(IPDADNMRJAGSresultsSMSC$BUGSoutput$summary[12:407,7])
p_upper1<-p_upper[1:99]
p_upper2<-p_upper[100:198]
p_upper3<-p_upper[199:297]
p_upper4<-p_upper[298:396]
####preperation for the graph
###For Dymethyl fumarate- Risk & propability to relapse
DFL<-cbind(Risknew1,p[,1], p_lower1, p_upper1)
DFL$Treatment<-"Dimethyl Fumarate"
colnames(DFL)<-c("Risknew","prelapse","lower","upper","Treatment")
##For Glatimarer acetate  - Risk & propability to relapse
GAL<-cbind(Risknew1,p[,2], p_lower2, p_upper2)
GAL$Treatment<-"Glatiramer Acetate"
colnames(GAL)<-c("Risknew","prelapse","lower","upper","Treatment")
##For Natalizumab acetate - Risk & propability to relapse
NL<-cbind(Risknew1,p[,3], p_lower3, p_upper3)
NL$Treatment<-"Natalizumab"
colnames(NL)<-c("Risknew","prelapse","lower","upper","Treatment")
##For Placebo - Risk & propability to relapse
PlL<-cbind(Risknew1,p[,4], p_lower4, p_upper4)
PlL$Treatment<-"Placebo"
colnames(PlL)<-c("Risknew","prelapse","lower","upper","Treatment")

##merge data for all the treatments
GraphdataL<-rbind(DFL,GAL,NL,PlL)

write.xlsx(GraphdataL,"C:/Users/kc19o338/Documents/GitHub/Reproduce-results-from-papers/ThreeStageModelRRMS/Results/Data for the Graph_IPDAD.xls",row.names = F)
GraphdataF<-read_excel("C:/Users/kc19o338/Documents/GitHub/Reproduce-results-from-papers/ThreeStageModelRRMS/Results/Data for the Graph_IPDAD.xls")
Graphdata1<-GraphdataF[,5]
Graphdata2<-GraphdataF[,2]
Graphdata3<-GraphdataF[,1]
Graphdata<-cbind(Graphdata1,Graphdata2,Graphdata3)
colnames(Graphdata)<-c("Treatment", "Predicted probability to relapse within the next 2 years %", "Baseline risk score")
write.csv(Graphdata,"Data for the Graph_IPDAD.csv",row.names = F)

###Graph for IPD (predicted probabilities)

Graph<-ggplot(GraphdataL, aes(x=Risknew, y=prelapse, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))+geom_vline(xintercept=0.1265625, linetype="dashed", color = "red")+
 # geom_ribbon(aes(ymin=lower, ymax=upper, color=Treatment,fill=Treatment), alpha=0.2)+
   geom_vline(xintercept=0.7040377, linetype="dashed", color = "red")+xlab("Baseline risk") + ylab("Predicted probability of relapse")

######################   TABLES OF predicted risks by risk-group ###################################

mean1<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1_lower<-round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1_upper<-round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)


mean1low<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<0.3 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2low<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<0.3 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3low<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<0.3 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1low_lower<-round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2low_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3low_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1low_upper<-round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2low_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3low_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.1198 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)


mean1high<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2high<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3high<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1high_lower<-round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2high_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3high_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1high_upper<-round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2high_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3high_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.7066 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)




Absolutetable<-data.frame("Average predicted relapse "=c(paste(mean1, "(", mean1_lower, ",",mean1_upper, ")"), paste(mean2, "(", mean2_lower,",",mean2_upper,")"), paste(mean3, "(", mean3_lower,",",mean3_upper,")")),
                          "Low-isk patients' (<25%) predicted relapse"=c(paste(mean1low, "(", mean1low_lower, ",",mean1low_upper, ")"), paste(mean2low, "(", mean2low_lower,",",mean2low_upper,")"), paste(mean3low, "(", mean3low_lower,",",mean3low_upper,")")),
                          "High-isk patients' (>50%) predicted relapse"=c(paste(mean1high, "(", mean1high_lower, ",",mean1high_upper, ")"), paste(mean2high, "(", mean2high_lower,",",mean2high_upper,")"), paste(mean3high, "(", mean3high_lower,",",mean3high_upper,")")))
names(Absolutetable)<-c("Average predicted relapse %","Low-isk patients' (<25%) predicted relapse %","High-isk patients' (>50%) predicted relapse %")
rownames(Absolutetable)<-c("Dymethyl fumarate","Glatiramer acetate","Natalizumab")

######################   Graph OF ORs###################################


pl<-IPDADNMRJAGSresultsSMSC$BUGSoutput$mean$logitp
pl_lower<-(IPDADNMRJAGSresultsSMSC$BUGSoutput$summary[12:407,3])
pl_lower1<-pl_lower[1:99]
pl_lower2<-pl_lower[100:198]
pl_lower3<-pl_lower[199:297]
pl_lower4<-pl_lower[298:396]

pl_upper<-(IPDADNMRJAGSresultsSMSC$BUGSoutput$summary[12:407,7])
pl_upper1<-pl_upper[1:99]
pl_upper2<-pl_upper[100:198]
pl_upper3<-pl_upper[199:297]
pl_upper4<-pl_upper[298:396]

####preperation for the graph
###For Dymethyl fumarate- Risk & propability to relapse
DFL_OR<-cbind(Risknew1,exp(pl[,1]-pl[,4]) ,round(exp(pl_lower1-pl_lower4),2),round(exp(pl_upper1-pl_upper4),2))
DFL_OR$Treatment<-"Dimethyl Fumarate vs Placebo"
colnames(DFL_OR)<-c("Risknew","OR","2.5%","97.5%","Comparison")
##For Glatimarer acetate  - Risk & propability to relapse
GAL_OR<-cbind(Risknew1,exp(pl[,2]-pl[,4]),round(exp(pl_lower2-pl_lower4),2),round(exp(pl_upper2-pl_upper4),2))
GAL_OR$Treatment<-"Glatiramer Acetate vs Placebo"
colnames(GAL_OR)<-c("Risknew","OR","2.5%","97.5%","Comparison")
##For Natalizumab acetate - Risk & propability to relapse
NL_OR<-cbind(Risknew1,exp(pl[,3]-pl[,4]),round(exp(pl_lower3-pl_lower4),2),round(exp(pl_upper3-pl_upper4),2))
NL_OR$Treatment<-"Natalizumab vs Placebo"
colnames(NL_OR)<-c("Risknew","OR","2.5%","97.5%","Comparison")
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

mean1OR_lowCI<- summary(GraphdataL_OR$`2.5%`[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2OR_lowCI<- summary(GraphdataL_OR$`2.5%`[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3OR_lowCI<- summary(GraphdataL_OR$`2.5%`[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1OR_upperCI<- summary(GraphdataL_OR$`97.5%`[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2OR_upperCI<- summary(GraphdataL_OR$`97.5%`[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3OR_upperCI<- summary(GraphdataL_OR$`97.5%`[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1lowOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<0.3 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2lowOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<0.3 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3lowOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<0.3 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1lowOR_lowCI<- summary(GraphdataL_OR$`2.5%`[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<0.3 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2lowOR_lowCI<- summary(GraphdataL_OR$`2.5%`[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<0.3 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3lowOR_lowCI<- summary(GraphdataL_OR$`2.5%`[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<0.3 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1lowOR_upperCI<- summary(GraphdataL_OR$`97.5%`[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<0.3 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2lowOR_upperCI<- summary(GraphdataL_OR$`97.5%`[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<0.3 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3lowOR_upperCI<- summary(GraphdataL_OR$`97.5%`[which(GraphdataL_OR$Risknew>=0.1198 & GraphdataL_OR$Risknew<0.3 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]



mean1highOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2highOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3highOR<- summary(GraphdataL_OR$OR[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]


mean1highOR_lowCI<- summary(GraphdataL_OR$`2.5%`[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2highOR_lowCI<- summary(GraphdataL_OR$`2.5%`[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3highOR_lowCI<- summary(GraphdataL_OR$`2.5%`[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

mean1highOR_upCI<- summary(GraphdataL_OR$`97.5%`[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Dimethyl Fumarate vs Placebo")])[4]
mean2highOR_upCI<- summary(GraphdataL_OR$`97.5%`[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Glatiramer Acetate vs Placebo")])[4]
mean3highOR_upCI<- summary(GraphdataL_OR$`97.5%`[which(GraphdataL_OR$Risknew>0.5 & GraphdataL_OR$Risknew<=0.7066 & GraphdataL_OR$Comparison=="Natalizumab vs Placebo")])[4]

ORtable<-data.frame("Average OR "=c(mean1OR, mean2OR, mean3OR), "Low-isk patients' (<25%) mean OR"=c(mean1lowOR,mean2lowOR,mean3lowOR)
                    , "High-isk patients' (>50%) predicted relapse "=c(mean1highOR,mean2highOR,mean3highOR))
names(ORtable)<-c("Average OR","Low-isk patients' (<25%) OR","High-isk patients' (>50%) OR ")
rownames(ORtable)<-c("Dymethyl fumarate","Glatiramer acetate","Natalizumab")



expit<-function(x) {exp(x)/(1+exp(x))}
p<-IPDADNMRJAGSresultsSMSC2$BUGSoutput$mean$logitp
p<-expit(IPDADNMRJAGSresultsSMSC2$BUGSoutput$mean$logitp)

p_lower<-expit(IPDADNMRJAGSresultsSMSC2$BUGSoutput$summary[18:413,3])
p_lower1<-p_lower[1:99]
p_lower2<-p_lower[100:198]
p_lower3<-p_lower[199:297]
p_lower4<-p_lower[298:396]

p_upper<-expit(IPDADNMRJAGSresultsSMSC2$BUGSoutput$summary[18:413,7])
p_upper1<-p_upper[1:99]
p_upper2<-p_upper[100:198]
p_upper3<-p_upper[199:297]
p_upper4<-p_upper[298:396]
####preperation for the graph

####preperation for the graph
###For Dymethyl fumarate- Risk & propability to relapse
###For Dymethyl fumarate- Risk & propability to relapse
DFL<-cbind(Risknew1,p[,1], p_lower1, p_upper1)
DFL$Treatment<-"Dimethyl Fumarate"
colnames(DFL)<-c("Risknew","prelapse","lower","upper","Treatment")
##For Glatimarer acetate  - Risk & propability to relapse
GAL<-cbind(Risknew1,p[,2], p_lower2, p_upper2)
GAL$Treatment<-"Glatiramer Acetate"
colnames(GAL)<-c("Risknew","prelapse","lower","upper","Treatment")
##For Natalizumab acetate - Risk & propability to relapse
NL<-cbind(Risknew1,p[,3], p_lower3, p_upper3)
NL$Treatment<-"Natalizumab"
colnames(NL)<-c("Risknew","prelapse","lower","upper","Treatment")
##For Placebo - Risk & propability to relapse
PlL<-cbind(Risknew1,p[,4], p_lower4, p_upper4)
PlL$Treatment<-"Placebo"
colnames(PlL)<-c("Risknew","prelapse","lower","upper","Treatment")

##merge data for all the treatments
GraphdataL<-rbind(DFL,GAL,NL,PlL)


###Graph for IPD (predicted probabilities)

Graph2<-ggplot(GraphdataL, aes(x=Risknew, y=prelapse, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))+geom_vline(xintercept=0.0961, linetype="dashed", color = "red")+
  #geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2)+
  geom_vline(xintercept=0.6264, linetype="dashed", color = "red")+xlab("Baseline risk") + ylab("Predicted probability of relapse")

######################   TABLES OF predicted risks by risk-group ###################################

mean1<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1_lower<-round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1_upper<-round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)


mean1low<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<0.3 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2low<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<0.3 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3low<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<0.3 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1low_lower<-round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2low_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3low_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1low_upper<-round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2low_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3low_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.0961 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)


mean1high<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2high<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3high<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1high_lower<-round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2high_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3high_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1high_upper<-round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2high_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3high_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.6264 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)




Absolutetable2<-data.frame("Average predicted relapse "=c(paste(mean1, "(", mean1_lower, ",",mean1_upper, ")"), paste(mean2, "(", mean2_lower,",",mean2_upper,")"), paste(mean3, "(", mean3_lower,",",mean3_upper,")")),
                          "Low-isk patients' (<25%) predicted relapse"=c(paste(mean1low, "(", mean1low_lower, ",",mean1low_upper, ")"), paste(mean2low, "(", mean2low_lower,",",mean2low_upper,")"), paste(mean3low, "(", mean3low_lower,",",mean3low_upper,")")),
                          "High-isk patients' (>50%) predicted relapse"=c(paste(mean1high, "(", mean1high_lower, ",",mean1high_upper, ")"), paste(mean2high, "(", mean2high_lower,",",mean2high_upper,")"), paste(mean3high, "(", mean3high_lower,",",mean3high_upper,")")))
names(Absolutetable2)<-c("Average predicted relapse %","Low-isk patients' (<25%) predicted relapse %","High-isk patients' (>50%) predicted relapse %")
rownames(Absolutetable2)<-c("Dymethyl fumarate","Glatiramer acetate","Natalizumab")



#model 3

expit<-function(x) {exp(x)/(1+exp(x))}
p<-IPDADNMRJAGSresultsSMSC3$BUGSoutput$mean$logitp
p<-expit(IPDADNMRJAGSresultsSMSC3$BUGSoutput$mean$logitp)

p_lower<-expit(IPDADNMRJAGSresultsSMSC3$BUGSoutput$summary[18:413,3])
p_lower1<-p_lower[1:99]
p_lower2<-p_lower[100:198]
p_lower3<-p_lower[199:297]
p_lower4<-p_lower[298:396]

p_upper<-expit(IPDADNMRJAGSresultsSMSC3$BUGSoutput$summary[38:433,7])
p_upper1<-p_upper[1:99]
p_upper2<-p_upper[100:198]
p_upper3<-p_upper[199:297]
p_upper4<-p_upper[298:396]
####preperation for the graph

####preperation for the graph
###For Dymethyl fumarate- Risk & propability to relapse
###For Dymethyl fumarate- Risk & propability to relapse
DFL<-cbind(Risknew1,p[,1], p_lower1, p_upper1)
DFL$Treatment<-"Dimethyl Fumarate"
colnames(DFL)<-c("Risknew","prelapse","lower","upper","Treatment")
##For Glatimarer acetate  - Risk & propability to relapse
GAL<-cbind(Risknew1,p[,2], p_lower2, p_upper2)
GAL$Treatment<-"Glatiramer Acetate"
colnames(GAL)<-c("Risknew","prelapse","lower","upper","Treatment")
##For Natalizumab acetate - Risk & propability to relapse
NL<-cbind(Risknew1,p[,3], p_lower3, p_upper3)
NL$Treatment<-"Natalizumab"
colnames(NL)<-c("Risknew","prelapse","lower","upper","Treatment")
##For Placebo - Risk & propability to relapse
PlL<-cbind(Risknew1,p[,4], p_lower4, p_upper4)
PlL$Treatment<-"Placebo"
colnames(PlL)<-c("Risknew","prelapse","lower","upper","Treatment")

##merge data for all the treatments
GraphdataL<-rbind(DFL,GAL,NL,PlL)


###Graph for IPD (predicted probabilities)

Graph3<-ggplot(GraphdataL, aes(x=Risknew, y=prelapse, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))+geom_vline(xintercept=0.1201, linetype="dashed", color = "red")+
 # geom_ribbon(aes(ymin=lower, ymax=upper, color=Treatment,fill=Treatment), alpha=0.1)+
  geom_vline(xintercept=0.5902, linetype="dashed", color = "red")+xlab("Baseline risk") + ylab("Predicted probability of relapse")

######################   TABLES OF predicted risks by risk-group ###################################
######################   TABLES OF predicted risks by risk-group ###################################

mean1<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1_lower<-round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1_upper<-round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)


mean1low<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<0.3 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2low<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<0.3 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3low<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<0.3 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1low_lower<-round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2low_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3low_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1low_upper<-round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2low_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3low_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.1201 & GraphdataL$Risknew<=0.3 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)


mean1high<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2high<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3high<- round(summary(GraphdataL$prelapse[which(GraphdataL$Risknew>0.5 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1high_lower<-round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2high_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3high_lower<- round(summary(GraphdataL$lower[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)
mean1high_upper<-round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Dimethyl Fumarate")])[4]*100,2)
mean2high_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Glatiramer Acetate")])[4]*100,2)
mean3high_upper<- round(summary(GraphdataL$upper[which(GraphdataL$Risknew>=0.5 & GraphdataL$Risknew<=0.5902 & GraphdataL$Treatment=="Natalizumab")])[4]*100,2)




Absolutetable3<-data.frame("Average predicted relapse "=c(paste(mean1, "(", mean1_lower, ",",mean1_upper, ")"), paste(mean2, "(", mean2_lower,",",mean2_upper,")"), paste(mean3, "(", mean3_lower,",",mean3_upper,")")),
                           "Low-isk patients' (<25%) predicted relapse"=c(paste(mean1low, "(", mean1low_lower, ",",mean1low_upper, ")"), paste(mean2low, "(", mean2low_lower,",",mean2low_upper,")"), paste(mean3low, "(", mean3low_lower,",",mean3low_upper,")")),
                           "High-isk patients' (>50%) predicted relapse"=c(paste(mean1high, "(", mean1high_lower, ",",mean1high_upper, ")"), paste(mean2high, "(", mean2high_lower,",",mean2high_upper,")"), paste(mean3high, "(", mean3high_lower,",",mean3high_upper,")")))
names(Absolutetable3)<-c("Average predicted relapse %","Low-isk patients' (<25%) predicted relapse %","High-isk patients' (>50%) predicted relapse %")
rownames(Absolutetable3)<-c("Dymethyl fumarate","Glatiramer acetate","Natalizumab")


