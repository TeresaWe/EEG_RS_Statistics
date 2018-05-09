library("tidyr", lib.loc="~/R/win-library/3.2")
library("ggplot2", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")

##################
#EO, beta, Clustering (higher in AP, correlated with AQ (higher))

##AP have lower Clustering, higher AQ_score has same relationship towards Clustering in both groups
#! but higher R in AP!

lm<-lm(`Crand_ T10`~AQ_Score+AP.0.RP.1.AP., data=german_beta_EO ) #!!! same with non native excluded (higher!)

german_beta_EO_AP<-subset(german_beta_EO, german_beta_EO$AP.0.RP.1.AP.==1)
german_beta_EO_RP<-subset(german_beta_EO, german_beta_EO$AP.0.RP.1.AP.==0)
lm<-lm(`Crand_ T10`~AQ_Score, data=german_beta_EO_AP )
lm<-lm(`Crand_ T10`~AQ_Score, data=german_beta_EO_RP )

lm<-lm(`Crand_ T10`~AQ_CO_Score+AQ_IM_Score+AQ_SK_Score+AQ_SW_Score+AQ_AD_Score+AP.0.RP.1.AP., data=german_beta_EO )#SW,group, (CO,IM) model sign.
lm<-lm(`Crand_ T10`~AQ_CO_Score+AQ_IM_Score+AQ_SK_Score+AQ_SW_Score+AQ_AD_Score, data=german_beta_EO_RP )#(SW)/model n.s.
lm<-lm(`Crand_ T10`~AQ_CO_Score+AQ_IM_Score+AQ_SK_Score+AQ_SW_Score+AQ_AD_Score, data=german_beta_EO_AP )#IM (+SK)/model sign.
##-> group effect on beta Clustering might be due to hihgher SW AQ in AP! 
#(pos. relationship, group difference and Clustering, 
#but no correlation of SK and C within AP)


ggplot(data=german_beta_EO,aes(x=AQ_Score, y=`Crand_ T10`))+
  geom_point(mapping=aes(x=AQ_Score, y=`Crand_ T10`))+
  geom_abline(aes(intercept=0.401845, slope=0.004963), color="red")+
  geom_abline(aes(intercept= 0.311296, slope=0.006794), color="blue")+
  geom_point(aes(color=AP.0.RP.1.AP.,size=starting_age))+theme_classic()+labs(x="AQ_Score",
                                                                              y= "`Crand_ T10_beta`", 
                                                                              title="Clustering,AQ and AP", 
                                                                              subtitle="")+
  coord_cartesian(ylim=c(0.25,0.8),xlim=c(0,36))+
  theme(axis.text.x=element_text(color="black", size=10),
        axis.text.y=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=10,face="bold"),
        legend.text=element_text(color="black", size=10),
        axis.title=element_text(face="bold",size=10),
        title=element_text(size=12),panel.grid.major.y = element_line(colour = "grey"))

##################################################
#EC, delta, Path Length (higher in AP)
lm<-lm(`Lrand_ T10`~AQ_Score+AP.0.RP.1.AP., data=german_delta_EC )

german_delta_EC_AP<-subset(german_delta_EC, german_delta_EC$AP.0.RP.1.AP.==1)
german_delta_EC_RP<-subset(german_delta_EC, german_delta_EC$AP.0.RP.1.AP.==0)
lm<-lm(`Lrand_ T10`~AQ_Score, data=german_delta_EC_AP )
lm<-lm(`Lrand_ T10`~AQ_Score, data=german_delta_EC_RP )

ggplot(data=german_delta_EC,aes(x=AQ_Score, y=`Lrand_ T10`))+
  geom_point(mapping=aes(x=AQ_Score, y=`Lrand_ T10`))+
  geom_abline(aes(intercept=1.783722, slope=0.001039), color="red")+
  geom_abline(aes(intercept= 1.807387, slope=0.001370), color="blue")+
  geom_point(aes(color=AP.0.RP.1.AP.,size=starting_age))+theme_classic()+labs(x="AQ_Score",
                                                                              y= "`Lrand_ T10_delta`", 
                                                                              title="Path length,AQ and AP", 
                                                                              subtitle="")+
  coord_cartesian(ylim=c(1.65,2),xlim=c(0,36))+
  theme(axis.text.x=element_text(color="black", size=10),
        axis.text.y=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=10,face="bold"),
        legend.text=element_text(color="black", size=10),
        axis.title=element_text(face="bold",size=10),
        title=element_text(size=12),panel.grid.major.y = element_line(colour = "grey"))



