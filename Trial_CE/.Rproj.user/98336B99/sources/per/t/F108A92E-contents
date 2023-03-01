
p<-IPDADNMRJAGSresults_CDP2$BUGSoutput$mean$logh
p<-exp(IPDADNMRJAGSresults_CDP2$BUGSoutput$mean$logh)



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
GraphdataL<-rbind(DFL,GAL,PlL)

summary(GraphdataL$prelapse[GraphdataL$Treatment=="Dimethyl Fumarate" & GraphdataL$Risknew>=0.1261 & GraphdataL$Risknew<=0.7606 ])
summary(GraphdataL$prelapse[GraphdataL$Treatment=="Glatiramer Acetate" & GraphdataL$Risknew>=0.1261 & GraphdataL$Risknew<=0.7606 ])


###Graph for IPD

IPDplot_CDP2<-ggplot(GraphdataL, aes(x=Risknew, y=prelapse, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))+geom_vline(xintercept=0.126, linetype="dashed", color = "red")+
  geom_vline(xintercept=0.76, linetype="dashed", color = "red")
IPDplot_CDP2<-IPDplot_CDP2+ xlab("Baseline risk") + ylab("Predicted Hazards of Confirmed Progression")

IPDplot_CDP2



p<-IPDADNMRJAGSresults_Relapse2$BUGSoutput$mean$annualized



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
GraphdataL<-rbind(DFL,GAL,PlL)

###Graph for IPD

IPDplot_Relapse<-ggplot(GraphdataL, aes(x=Risknew, y=prelapse, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))+geom_vline(xintercept=0.126, linetype="dashed", color = "red")+
  geom_vline(xintercept=0.76, linetype="dashed", color = "red")
IPDplot_Relapse<-IPDplot_Relapse+ xlab("Baseline risk") + ylab("Predicted Hazards of Confirmed Progression")

IPDplot_Relapse
