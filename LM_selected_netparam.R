
library("tidyr", lib.loc="~/R/win-library/3.2")
library("ggplot2", lib.loc="~/R/win-library/3.2")
library("dplyr", lib.loc="~/R/win-library/3.2")
# regressions of AP, AQ and significant network parameters

lm<-lm(`E_ T5`~AQ_Score+AP.0.RP.1.AP., data=EO_delta_network )
lm<-lm(`E_ T6`~AQ_Score+AP.0.RP.1.AP., data=EO_delta_network )#!
lm<-lm(`E_ T7`~AQ_Score+AP.0.RP.1.AP., data=EO_delta_network )#!
lm<-lm(`Lrand_ T2`~AQ_Score+AP.0.RP.1.AP., data=EO_delta_network )
lm<-lm(`Lrand_ T1`~AQ_Score+AP.0.RP.1.AP., data=EO_delta_network )
lm<-lm(`CErand_ T2`~AQ_Score+AP.0.RP.1.AP., data=EO_delta_network )
lm<-lm(`CErand_ T7`~AQ_Score+AP.0.RP.1.AP., data=EO_beta_network )
lm<-lm(`Lrand_ T7`~AQ_Score+AP.0.RP.1.AP., data=EO_beta_network )
lm<-lm(`Crand_ T10`~AQ_Score+AP.0.RP.1.AP., data=EO_beta_network ) #!!!
lm<-lm(`Crand_ T10`~AQ_Score+AP.0.RP.1.AP., data=german_beta_EO ) #!!! same with non native excluded (higher!)
lm<-lm(`Crand_ T9`~AQ_Score+AP.0.RP.1.AP., data=EO_beta_network )#!
lm<-lm(`SW_ T4`~AQ_Score+AP.0.RP.1.AP., data=EC_gamma_network )#!
lm<-lm(`Erand_ T10`~AQ_Score+AP.0.RP.1.AP., data=EC_delta_network )#!
lm<-lm(`Lrand_ T1`~AQ_Score+AP.0.RP.1.AP., data=EC_delta_network )
lm<-lm(`Lrand_ T5`~AQ_Score+AP.0.RP.1.AP., data=EC_delta_network )
lm<-lm(`Lrand_ T9`~AQ_Score+AP.0.RP.1.AP., data=EC_delta_network )
lm<-lm(`Lrand_ T10`~AQ_Score+AP.0.RP.1.AP., data=german_delta_EC ) # non native excluded
lm<-lm(AP.0.RP.1.AP.~AQ_Score+`Lrand_ T10`, data=EC_delta_network )#!!!
lm<-lm(SDfoM~AQ_Score+`Lrand_ T10`, data=EC_delta_network )
lm<-lm(`Lrand_ T10`~AQ_Score+MAD, data=german_delta_EC )#...

lm<-lm(MAD~AQ_Score+`Crand_ T10`+early,EO_beta_network )
summary(lm)

## beta relationship
ggplot(data=EO_beta_network,aes(x=AQ_Score, y=`Crand_ T10`))+
     geom_point(mapping=aes(x=AQ_Score, y=`Crand_ T10`))+
     geom_abline(aes(intercept=0.415439, slope=0.004158))+
     geom_point(aes(color=AP.0.RP.1.AP.))+theme_classic()+labs(x="AQ_Score",
                                                         y= "Crand_T10_beta", 
                                                         title="Clustering,AQ and AP", 
                                                         subtitle="")+
     coord_cartesian(ylim=c(0.2,0.8),xlim=c(5,40))+
     theme(axis.text.x=element_text(color="black", size=10),
                        axis.text.y=element_text(color="black", size=10),
                        legend.title=element_text(color="black", size=10,face="bold"),
                       legend.text=element_text(color="black", size=10),
                        axis.title=element_text(face="bold",size=10),
                        title=element_text(size=12),panel.grid.major.y = element_line(colour = "grey"))
##same with non native excluded
ggplot(data=german_beta_EO,aes(x=AQ_Score, y=`Crand_ T10`))+
  geom_point(mapping=aes(x=AQ_Score, y=`Crand_ T10`))+
  geom_abline(aes(intercept=0.388153, slope=0.005775))+
  geom_point(aes(color=AP.0.RP.1.AP.))+theme_classic()+labs(x="AQ_Score",
                                                    y= "Crand_T10_beta", 
                                                    title="Clustering,AQ and AP", 
                                                    subtitle="")+
  coord_cartesian(ylim=c(0.2,0.8),xlim=c(5,32))+
  theme(axis.text.x=element_text(color="black", size=10),
        axis.text.y=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=10,face="bold"),
        legend.text=element_text(color="black", size=10),
        axis.title=element_text(face="bold",size=10),
        title=element_text(size=12),panel.grid.major.y = element_line(colour = "grey"))


##AQ.Ldelta,AP.0.RP.1.AP. (#non native excluded)
ggplot(data=german_delta_EC,aes(x=AQ_Score, y=`Lrand_ T10`))+
  geom_point(mapping=aes(x=AQ_Score, y=`Lrand_ T10`))+
  geom_abline(aes(intercept= 1.781249, slope=0.001186),color="red")+
  geom_point(aes(color=AP.0.RP.1.AP.))+theme_classic()+labs(x="AQ_Score",
                                                    y= "Lrand_T10_delta", 
                                                    title="Path length,AQ and AP", 
                                                    subtitle="")+
  coord_cartesian(ylim=c(1.7,1.95),xlim=c(5,40))+
  theme(axis.text.x=element_text(color="black", size=10),
        axis.text.y=element_text(color="black", size=10),
        legend.title=element_text(color="black", size=10,face="bold"),
        legend.text=element_text(color="black", size=10),
        axis.title=element_text(face="bold",size=10),
        title=element_text(size=12),panel.grid.major.y = element_line(colour = "grey"))

##AQ,Ldelta,MAD
ggplot(data=german_delta_EC,aes(x=MAD, y=`Lrand_ T10`))+
  geom_point(mapping=aes(x=MAD, y=`Lrand_ T10`))+
  geom_abline(aes(intercept=1.808e+00, slope=-8.781e-05))+
  geom_point(aes(color=AQ_Score))+theme_classic()+labs(x="SDfoM",
                                                                                                                   y= "Lrand_T10_delta", 
                                                                                                                   title="Path length,AQ and AP", 
                                                                                                                   subtitle="")+
  coord_cartesian(ylim=c(1.7,1.95),xlim=c(0,500))+
  theme(axis.text.x=element_text(color="black", size=10),
              axis.text.y=element_text(color="black", size=10),
              legend.title=element_text(color="black", size=10,face="bold"),
              legend.text=element_text(color="black", size=10),
               axis.title=element_text(face="bold",size=10),
              title=element_text(size=12),panel.grid.major.y = element_line(colour = "grey"))