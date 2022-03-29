p<-IPDADNMRJAGSresults_CDP$BUGSoutput$mean$logh
p<-exp(IPDADNMRJAGSresults_CDP$BUGSoutput$mean$logh)



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

IPDplot_CDP<-ggplot(GraphdataL, aes(x=Risknew, y=prelapse, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))+geom_vline(xintercept=0.126, linetype="dashed", color = "red")+
  geom_vline(xintercept=0.76, linetype="dashed", color = "red")
IPDplot_CDP<-IPDplot_CDP+ xlab("Baseline risk") + ylab("Predicted Hazards of Confirmed Progression")

IPDplot_CDP


p<-IPDADNMRJAGSresults_Relapse$BUGSoutput$mean$logitp
p<-expit(IPDADNMRJAGSresults_Relapse$BUGSoutput$mean$logitp)



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

IPDplot_CDP<-ggplot(GraphdataL, aes(x=Risknew, y=prelapse, group=Treatment)) +
  geom_line(aes(color=Treatment))+
  geom_point(aes(color=Treatment))+geom_vline(xintercept=0.126, linetype="dashed", color = "red")+
  geom_vline(xintercept=0.76, linetype="dashed", color = "red")
IPDplot_CDP<-IPDplot_CDP+ xlab("Baseline risk") + ylab("Predicted Hazards of Confirmed Progression")

IPDplot_CDP
