## Density distributions of degree values
#create index of AP vs. RP in EEG order
EEG_order<-cbind(EEG_order, EEG_order)
VPindex<-match(x=EEG_order$VP_Code_EEG, table=EO_delta_network$VP_Code)
VPindex<-rbind(EO_delta_network[VPindex,])
VPindex<-data.frame(VPindex$VP_Code, VPindex$exclude.0.no.1.elec.2.med.3.exp.,VPindex$AP.0.RP.1.AP.,VPindex$AQ_Score)
colnames(VPindex)<-c("VP_Code","excluded","APRP","AQ_Score")
is.AP<-VPindex$APRP
is.excl<-VPindex$excluded
is.AQ<-VPindex$AQ_Score
is.AQ[is.AQ<21]<-0
is.AQ[is.AQ>=26]<-2
is.AQ[is.AQ>2]<-1
is.excl[is.excl==2]<-1
is.excl[is.excl==3]<-1

#############################
#BETA
#############################
degree_beta<-read.csv("beta_locdeg.csv",header=FALSE) #28 electrodes * 10 thresholds!
degree_beta<-as.matrix(degree_beta)
degree_beta_T10<-as.matrix(degree_beta[,c(10,20,30,40,50,60,70,80,90,100)])
plot(density(degree_beta_T10))
AP_degree_beta_T10<-degree_beta_T10[which(is.AP==1&is.excl==0),]
RP_degree_beta_T10<-degree_beta_T10[which(is.AP==0&is.excl==0),]
plot(density(RP_degree_beta_T10),main="Degree Density Beta T10",col="orange")
lines(density(AP_degree_beta_T10),col="black")
text(20, y = 0.08, labels = c("RP"), col = "orange")
text(20, y = 0.07, labels = c("AP"), col = "black")

degree_beta<-read.csv("beta_locdeg.csv",header=FALSE) #28 electrodes * 10 thresholds!
degree_beta<-as.matrix(degree_beta)
degree_beta_T9<-as.matrix(degree_beta[,c(9,19,29,39,49,59,69,79,89,99)])
plot(density(degree_beta_T10))
AP_degree_beta_T9<-degree_beta_T9[which(is.AP==1&is.excl==0),]
RP_degree_beta_T9<-degree_beta_T9[which(is.AP==0&is.excl==0),]
plot(density(RP_degree_beta_T9),main="Degree Density Beta T9",col="orange")
lines(density(AP_degree_beta_T9),col="black")
text(20, y = 0.08, labels = c("RP"), col = "orange")
text(20, y = 0.07, labels = c("AP"), col = "black")

#############################
#Delta
#############################
degree_delta<-read.csv("delta_locdeg.csv",header=FALSE) #28 electrodes * 10 thresholds!
degree_delta<-as.matrix(degree_delta)
degree_delta_T10<-as.matrix(degree_delta[,c(10,20,30,40,50,60,70,80,90,100)])
plot(density(degree_delta_T10))
AP_degree_delta_T10<-degree_delta_T10[which(is.AP==1&is.excl==0),]
RP_degree_delta_T10<-degree_delta_T10[which(is.AP==0&is.excl==0),]
plot(density(AP_degree_delta_T10),main="Degree Density Delta T10",col="black")
lines(density(RP_degree_delta_T10),col="orange")
text(20, y = 0.08, labels = c("RP"), col = "orange")
text(20, y = 0.07, labels = c("AP"), col = "black")

degree_delta<-read.csv("delta_locdeg.csv",header=FALSE) #28 electrodes * 10 thresholds!
degree_delta<-as.matrix(degree_delta)
degree_delta_T9<-as.matrix(degree_delta[,c(9,19,29,39,49,59,69,79,89,99)])
plot(density(degree_delta_T10))
AP_degree_delta_T9<-degree_delta_T9[which(is.AP==1&is.excl==0),]
RP_degree_delta_T9<-degree_delta_T9[which(is.AP==0&is.excl==0),]
plot(density(AP_degree_delta_T9),main="Degree Density Delta T9",col="black")
lines(density(RP_degree_delta_T9),col="orange")
text(20, y = 0.08, labels = c("RP"), col = "orange")
text(20, y = 0.07, labels = c("AP"), col = "black")
