

p<-IPDADNMRJAGSresults_Relapse2$BUGSoutput$summary[1:396,]

sd<-IPDADNMRJAGSresults_Relapse2$BUGSoutput$summary[1:396,2]
sd1<-sd[1:99]
sd2<-sd[100:198]
sd3<-sd[199:297]
sd4<-sd[298:396]

p_lower<-IPDADNMRJAGSresults_Relapse2$BUGSoutput$summary[1:396,3]
p_lower1<-p_lower[1:99]
p_lower2<-p_lower[100:198]
p_lower3<-p_lower[199:297]
p_lower4<-p_lower[298:396]

p_upper<-IPDADNMRJAGSresults_Relapse2$BUGSoutput$summary[1:396,7]
p_upper1<-p_upper[1:99]
p_upper2<-p_upper[100:198]
p_upper3<-p_upper[199:297]
p_upper4<-p_upper[298:396]
####preperation for the graph
###For Dymethyl fumarate- Risk & propability to relapse
DFL<-cbind(Risknew,p[,1],sd1, p_lower1, p_upper1)
DFL$Treatment<-"Dimethyl Fumarate"
colnames(DFL)<-c("Risknew","annualized_relapse_rate","sd","lower","upper","Treatment")
##For Glatimarer acetate  - Risk & propability to relapse
GAL<-cbind(Risknew,p[,2],sd2, p_lower2, p_upper2)
GAL$Treatment<-"Glatiramer Acetate"
colnames(GAL)<-c("Risknew","annualized_relapse_rate","sd","lower","upper","Treatment")
##For Natalizumab acetate - Risk & propability to relapse
NL<-cbind(Risknew,p[,3], sd3,p_lower3, p_upper3)
NL$Treatment<-"Natalizumab"
colnames(NL)<-c("Risknew","annualized_relapse_rate","sd","lower","upper","Treatment")
##For Placebo - Risk & propability to relapse
PlL<-cbind(Risknew,p[,4],sd4, p_lower4, p_upper4)
PlL$Treatment<-"Placebo"
colnames(PlL)<-c("Risknew","annualized_relapse_rate","sd","lower","upper","Treatment")

##merge data for all the treatments
annualized_relapse_rate_data<-rbind(DFL,GAL,NL,PlL)
write.csv(annualized_relapse_rate_data, "C:/Users/kc19o338/Documents/GitHub/Reproduce-results-from-papers/Trial_CE/annualized relapse rate per risk.csv")

