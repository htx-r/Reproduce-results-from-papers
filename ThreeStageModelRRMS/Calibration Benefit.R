

###Dimethyl fumerate vs Placebo
set.seed(2000)
DF<-RiskData[which(RiskData$Treatment=="Dimethyl fumarate"),]
DF_samp<-sample (c(1:nrow(DF)), size=nrow(RiskData[which(RiskData$Treatment=="Placebo"),]), replace=F)
DF<-DF[DF_samp,]
lin_pred_DF <- DF$logitp_internal
prob_DF <- expit(DF$logitp_internal)

lin_pred_Pl <- RiskData$logitp_internal[which(RiskData$Treatment=="Placebo")]
prob_Pl <- expit(RiskData$logitp_internal[which(RiskData$Treatment=="Placebo")])

pred_benefit_DFPl<-prob_Pl-prob_DF
observed_benefit_DFPl<-as.numeric(RiskData$outcome[which(RiskData$Treatment=="Placebo")])-as.numeric(DF$outcome)

# create 10 risk groups
groups_pt8 <- cut(pred_benefit_DFPl,breaks=quantile(pred_benefit_DFPl, prob = c(0,0.2,0.4,0.6,0.8,1.0)),labels=c(1:5),include.lowest=TRUE)
DATADFPl<-rbind(DF,RiskData[which(RiskData$Treatment=="Placebo"),])
# average the observed and expected probabilities of patients in each risk group
gpdata_pt8 <- cbind(as.data.frame(observed_benefit_DFPl),groups_pt8,pred_benefit_DFPl)
obs_pt8 <- as.data.frame(ddply(gpdata_pt8,~groups_pt8,summarise,mean=mean(observed_benefit_DFPl))[,2])
exp_pt8 <- as.data.frame(ddply(gpdata_pt8,~groups_pt8,summarise,mean=mean(pred_benefit_DFPl))[,2])
colnames(obs_pt8)<-c("Probability")
colnames(exp_pt8)<-c("Probability")
df2<-rbind(round(obs_pt8,2),round(exp_pt8,2))
df2<-cbind(df2,as.data.frame(c("Observed","Observed","Observed","Observed","Observed","Expected","Expected","Expected","Expected","Expected")))
colnames(df2)<-c("Probability","Category")

df2<-cbind(df2,as.data.frame(c(1,2,3,4,5,1,2,3,4,5)))
colnames(df2)<-c("Probability","Category","Quartiles")
df2$Category<-as.factor(df2$Category)
df2$Quartiles<-as.factor(df2$Quartiles)

a<-ggplot(data=df2, aes(x=Quartiles, y=Probability, fill=Category)) +
  geom_bar(stat="identity", position=position_dodge())+
  xlab("Quintiles of Treatment Benefit") + ylab("Treatment Benefit")+
  geom_text(aes(label=Probability), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)+
  scale_fill_brewer(palette="Paired")+ylim(0,0.8)+
  theme(axis.text.x = element_text( size = 12, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        axis.title.x = element_text( size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title = element_text(size = 30) )


###Glatiramer Acetate vs Placebo
set.seed(10)
Pl<-RiskData[which(RiskData$Treatment=="Placebo"),]
Pl_samp<-sample (c(1:nrow(Pl)), size=nrow(RiskData[which(RiskData$Treatment=="Glatiramer acetate"),]), replace=F)
Pl<-Pl[Pl_samp,]
lin_pred_GA <- RiskData$logitp_internal[which(RiskData$Treatment=="Glatiramer acetate")]
prob_GA <-  expit(RiskData$logitp_internal[which(RiskData$Treatment=="Glatiramer acetate")])

lin_pred_Pl <- Pl$logitp_internal
prob_Pl <- expit(Pl$logitp_internal)

pred_benefit_GAPl<-prob_Pl-prob_GA
observed_benefit_GAPl<-as.numeric(Pl$outcome)-as.numeric(RiskData$outcome[which(RiskData$Treatment=="Glatiramer acetate")])

# create 10 risk groups
groups_pt8 <- cut(pred_benefit_GAPl,breaks=quantile(pred_benefit_GAPl, prob = c(0,0.2,0.4,0.6,0.8,1.0)),labels=c(1:5),include.lowest=TRUE)
gpdata_pt8 <- cbind(as.data.frame(observed_benefit_GAPl),groups_pt8,pred_benefit_GAPl)
DATAGAPl<-rbind(Pl,RiskData[which(RiskData$Treatment=="Placebo"),])
# average the observed and expected probabilities of patients in each risk group
gpdata_pt8 <- cbind(as.data.frame(observed_benefit_GAPl),groups_pt8,pred_benefit_GAPl)
obs_pt8 <- as.data.frame(ddply(gpdata_pt8,~groups_pt8,summarise,mean=mean(observed_benefit_GAPl))[,2])
exp_pt8 <- as.data.frame(ddply(gpdata_pt8,~groups_pt8,summarise,mean=mean(pred_benefit_GAPl))[,2])
colnames(obs_pt8)<-c("Probability")
colnames(exp_pt8)<-c("Probability")
GA2<-rbind(round(obs_pt8,2),round(exp_pt8,2))
GA2<-cbind(GA2,as.data.frame(c("Observed","Observed","Observed","Observed","Observed","Expected","Expected","Expected","Expected","Expected")))
colnames(GA2)<-c("Probability","Category")

GA2<-cbind(GA2,as.data.frame(c(1,2,3,4,5,1,2,3,4,5)))
colnames(GA2)<-c("Probability","Category","Quartiles")
GA2$Category<-as.factor(GA2$Category)
GA2$Quartiles<-as.factor(GA2$Quartiles)

b<-ggplot(data=GA2, aes(x=Quartiles, y=Probability, fill=Category)) +
  geom_bar(stat="identity", position=position_dodge())+
  xlab("Quartiles of Treatment Benefit") + ylab("Treatment Benefit")+
  geom_text(aes(label=Probability), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)+ylim(0,0.8)+
  scale_fill_brewer(palette="Paired")+
  theme(axis.text.x = element_text( size = 12, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        axis.title.x = element_text( size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title = element_text(size = 30) )


###Natalizumab vs Placebo
set.seed(12345)
Pl<-RiskData[which(RiskData$Treatment=="Placebo"),]
Pl_samp<-sample (c(1:nrow(Pl)), size=nrow(RiskData[which(RiskData$Treatment=="Natalizumab"),]), replace=F)
Pl<-Pl[Pl_samp,]
lin_pred_N <- RiskData$logitp_internal[which(RiskData$Treatment=="Natalizumab")]
prob_N <- expit(RiskData$logitp_internal[which(RiskData$Treatment=="Natalizumab")])
lin_pred_Pl <- Pl$logitp_internal
prob_Pl <- expit(Pl$logitp_internal)

pred_benefit_NPl<-prob_Pl-prob_N
observed_benefit_NPl<-as.numeric(Pl$outcome)-as.numeric(RiskData$outcome[which(RiskData$Treatment=="Natalizumab")])

# create 10 risk groups
groups_pt8 <- cut(pred_benefit_NPl,breaks=quantile(pred_benefit_NPl, prob = c(0,0.2,0.4,0.6,0.8,1.0)),labels=c(1:5),include.lowest=TRUE)
gpdata_pt8 <- cbind(as.data.frame(observed_benefit_NPl),groups_pt8,pred_benefit_NPl)
# average the observed and expected probabilities of patients in each risk group
gpdata_pt8 <- cbind(as.data.frame(observed_benefit_NPl),groups_pt8,pred_benefit_NPl)
obs_pt8 <- as.data.frame(ddply(gpdata_pt8,~groups_pt8,summarise,mean=mean(observed_benefit_NPl))[,2])
exp_pt8 <- as.data.frame(ddply(gpdata_pt8,~groups_pt8,summarise,mean=mean(pred_benefit_NPl))[,2])
colnames(obs_pt8)<-c("Probability")
colnames(exp_pt8)<-c("Probability")
NA2<-rbind(round(obs_pt8,2),round(exp_pt8,2))
NA2<-cbind(NA2,as.data.frame(c("Observed","Observed","Observed","Observed","Observed","Expected","Expected","Expected","Expected","Expected")))
colnames(NA2)<-c("Probability","Category")

NA2<-cbind(NA2,as.data.frame(c(1,2,3,4,5,1,2,3,4,5)))
colnames(NA2)<-c("Probability","Category","Quartiles")
NA2$Category<-as.factor(NA2$Category)
NA2$Quartiles<-as.factor(NA2$Quartiles)

c<-ggplot(data=NA2, aes(x=Quartiles, y=Probability, fill=Category)) +
  geom_bar(stat="identity", position=position_dodge())+
  xlab("Quartiles of Treatment Benefit") + ylab("Treatment Benefit")+
  geom_text(aes(label=Probability), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)+
  scale_fill_brewer(palette="Paired")+ylim(0,0.8)+
  theme(axis.text.x = element_text( size = 12, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),
        axis.title.x = element_text( size = 15, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(size = 15, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        plot.title = element_text(size = 30) )


ggarrange(a,b,c,ncol = 1,nrow = 3,labels = c("A  Dimethyl Fumerate vs Placebo","B  Glatiramer Acetate vs Placebo", "C Natalizumab vs Placebo"), hjust=-0.2,font.label = list(size = 15))

