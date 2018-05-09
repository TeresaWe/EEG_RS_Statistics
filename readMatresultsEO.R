#load results from fieldtrip/matlab 
#read order of EEG analysis
EEG_order<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/EEG_order.csv")
#read results table
allresults<-read.csv('~/Backup PC Cambridge/Teresa/files/data_tables/allresults28.03.csv')
library("tidyr", lib.loc="~/R/win-library/3.3")
library("dplyr", lib.loc="~/R/win-library/3.3")
allresults<-mutate(allresults, early=starting_age)
allresults$early[allresults$early<7]<-1
allresults$early[allresults$early>=7]<-0



############################################
#EO 
############################################

######################################
#all freq bands
######################################

# directory
setwd("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO")
#read threshold levels
thresholds<-c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10")
all_cost<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/all_cost.csv", header=FALSE)
colnames(all_cost)<-thresholds
#read Clustering
all_C<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/all_C.csv", header=FALSE)
colnames(all_C)<-paste("C_",thresholds)
#read Clustering rand
all_Crand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/all_Crand.csv", header=FALSE)
colnames(all_Crand)<-paste("Crand_",thresholds)
#read Path length
all_L<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/all_L.csv", header=FALSE)
colnames(all_L)<-paste("L_",thresholds)
#read Path length rand
all_Lrand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/all_Lrand.csv", header=FALSE)
colnames(all_Lrand)<-paste("Lrand_",thresholds)
#read Efficiency
all_E<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/all_E.csv", header=FALSE)
colnames(all_E)<-paste("E_",thresholds)
#read Efficiency rand
all_Erand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/all_Erand.csv", header=FALSE)
colnames(all_Erand)<-paste("Erand_",thresholds)
#read CostEfficiency
all_CE<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/all_CE.csv", header=FALSE)
colnames(all_CE)<-paste("CE_",thresholds)
#read CosteEfficiency rand
all_CErand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/all_CErand.csv", header=FALSE)
colnames(all_CErand)<-paste("CErand_",thresholds)
#read Degree
all_deg<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/all_deg.csv", header=FALSE)
colnames(all_deg)<-paste("deg_",thresholds)
#read Small World
all_SW<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/all_SW.csv", header=FALSE)
colnames(all_SW)<-paste("SW_",thresholds)

#bind to one table per frequency band
EO_all_network<-cbind(EEG_order,all_C,all_Crand, all_L,all_Lrand,
                              all_E, all_Erand,all_CE,all_CErand,all_deg,all_SW)
#match allresults and network
index<-match(x=allresults$VP_Code, table=EO_all_network$VP_Code_EEG)
networkindex<-rbind(EO_all_network[index,])
EO_all_network<-cbind(allresults,networkindex) 

############################################
#alpha
############################################
# directory
setwd("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO")
#read threshold levels
thresholds<-c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10")
alpha_cost<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/alpha_cost.csv", header=FALSE)
colnames(alpha_cost)<-thresholds
#read Clustering
alpha_C<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/alpha_C.csv", header=FALSE)
colnames(alpha_C)<-paste("C_",thresholds)
#read Clustering rand
alpha_Crand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/alpha_Crand.csv", header=FALSE)
colnames(alpha_Crand)<-paste("Crand_",thresholds)
#read Path length
alpha_L<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/alpha_L.csv", header=FALSE)
colnames(alpha_L)<-paste("L_",thresholds)
#read Path length rand
alpha_Lrand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/alpha_Lrand.csv", header=FALSE)
colnames(alpha_Lrand)<-paste("Lrand_",thresholds)
#read Efficiency
alpha_E<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/alpha_E.csv", header=FALSE)
colnames(alpha_E)<-paste("E_",thresholds)
#read Efficiency rand
alpha_Erand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/alpha_Erand.csv", header=FALSE)
colnames(alpha_Erand)<-paste("Erand_",thresholds)
#read CostEfficiency
alpha_CE<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/alpha_CE.csv", header=FALSE)
colnames(alpha_CE)<-paste("CE_",thresholds)
#read CosteEfficiency rand
alpha_CErand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/alpha_CErand.csv", header=FALSE)
colnames(alpha_CErand)<-paste("CErand_",thresholds)
#read Degree
alpha_deg<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/alpha_deg.csv", header=FALSE)
colnames(alpha_deg)<-paste("deg_",thresholds)
#read Small World
alpha_SW<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/alpha_SW.csv", header=FALSE)
colnames(alpha_SW)<-paste("SW_",thresholds)

#bind to one table per frequency band
EO_alpha_network<-cbind(EEG_order,alpha_C,alpha_Crand, alpha_L,alpha_Lrand,
                              alpha_E, alpha_Erand,alpha_CE,alpha_CErand,alpha_deg,alpha_SW)
#match alpharesults and network
index<-match(x=allresults$VP_Code, table=EO_alpha_network$VP_Code_EEG)
networkindex<-rbind(EO_alpha_network[index,])
EO_alpha_network<-cbind(allresults,networkindex)

############################################
#beta
############################################
# directory
setwd("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO")
#read threshold levels
thresholds<-c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10")
beta_cost<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/beta_cost.csv", header=FALSE)
colnames(beta_cost)<-thresholds
#read Clustering
beta_C<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/beta_C.csv", header=FALSE)
colnames(beta_C)<-paste("C_",thresholds)
#read Clustering rand
beta_Crand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/beta_Crand.csv", header=FALSE)
colnames(beta_Crand)<-paste("Crand_",thresholds)
#read Path length
beta_L<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/beta_L.csv", header=FALSE)
colnames(beta_L)<-paste("L_",thresholds)
#read Path length rand
beta_Lrand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/beta_Lrand.csv", header=FALSE)
colnames(beta_Lrand)<-paste("Lrand_",thresholds)
#read Efficiency
beta_E<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/beta_E.csv", header=FALSE)
colnames(beta_E)<-paste("E_",thresholds)
#read Efficiency rand
beta_Erand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/beta_Erand.csv", header=FALSE)
colnames(beta_Erand)<-paste("Erand_",thresholds)
#read CostEfficiency
beta_CE<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/beta_CE.csv", header=FALSE)
colnames(beta_CE)<-paste("CE_",thresholds)
#read CosteEfficiency rand
beta_CErand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/beta_CErand.csv", header=FALSE)
colnames(beta_CErand)<-paste("CErand_",thresholds)
#read Degree
beta_deg<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/beta_deg.csv", header=FALSE)
colnames(beta_deg)<-paste("deg_",thresholds)
#read Small World
beta_SW<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/beta_SW.csv", header=FALSE)
colnames(beta_SW)<-paste("SW_",thresholds)

#bind to one table per frequency band
EO_beta_network<-cbind(EEG_order,beta_C,beta_Crand, beta_L,beta_Lrand,
                                beta_E, beta_Erand,beta_CE,beta_CErand,beta_deg,beta_SW)
#match betaresults and network
index<-match(x=allresults$VP_Code, table=EO_beta_network$VP_Code_EEG)
networkindex<-rbind(EO_beta_network[index,])
EO_beta_network<-cbind(allresults,networkindex)

############################################
#gamma
############################################
# directory
setwd("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO")
#read threshold levels
thresholds<-c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10")
gamma_cost<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/gamma_cost.csv", header=FALSE)
colnames(gamma_cost)<-thresholds
#read Clustering
gamma_C<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/gamma_C.csv", header=FALSE)
colnames(gamma_C)<-paste("C_",thresholds)
#read Clustering rand
gamma_Crand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/gamma_Crand.csv", header=FALSE)
colnames(gamma_Crand)<-paste("Crand_",thresholds)
#read Path length
gamma_L<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/gamma_L.csv", header=FALSE)
colnames(gamma_L)<-paste("L_",thresholds)
#read Path length rand
gamma_Lrand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/gamma_Lrand.csv", header=FALSE)
colnames(gamma_Lrand)<-paste("Lrand_",thresholds)
#read Efficiency
gamma_E<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/gamma_E.csv", header=FALSE)
colnames(gamma_E)<-paste("E_",thresholds)
#read Efficiency rand
gamma_Erand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/gamma_Erand.csv", header=FALSE)
colnames(gamma_Erand)<-paste("Erand_",thresholds)
#read CostEfficiency
gamma_CE<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/gamma_CE.csv", header=FALSE)
colnames(gamma_CE)<-paste("CE_",thresholds)
#read CosteEfficiency rand
gamma_CErand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/gamma_CErand.csv", header=FALSE)
colnames(gamma_CErand)<-paste("CErand_",thresholds)
#read Degree
gamma_deg<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/gamma_deg.csv", header=FALSE)
colnames(gamma_deg)<-paste("deg_",thresholds)
#read Small World
gamma_SW<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/gamma_SW.csv", header=FALSE)
colnames(gamma_SW)<-paste("SW_",thresholds)

#bind to one table per frequency band
EO_gamma_network<-cbind(EEG_order,gamma_C,gamma_Crand, gamma_L,gamma_Lrand,
                       gamma_E, gamma_Erand,gamma_CE,gamma_CErand,gamma_deg,gamma_SW)
#match gammaresults and network
index<-match(x=allresults$VP_Code, table=EO_gamma_network$VP_Code_EEG)
networkindex<-rbind(EO_gamma_network[index,])
EO_gamma_network<-cbind(allresults,networkindex)


############################################
#delta
############################################
# directory
setwd("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO")
#read threshold levels
thresholds<-c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10")
delta_cost<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/delta_cost.csv", header=FALSE)
colnames(delta_cost)<-thresholds
#read Clustering
delta_C<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/delta_C.csv", header=FALSE)
colnames(delta_C)<-paste("C_",thresholds)
#read Clustering rand
delta_Crand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/delta_Crand.csv", header=FALSE)
colnames(delta_Crand)<-paste("Crand_",thresholds)
#read Path length
delta_L<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/delta_L.csv", header=FALSE)
colnames(delta_L)<-paste("L_",thresholds)
#read Path length rand
delta_Lrand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/delta_Lrand.csv", header=FALSE)
colnames(delta_Lrand)<-paste("Lrand_",thresholds)
#read Efficiency
delta_E<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/delta_E.csv", header=FALSE)
colnames(delta_E)<-paste("E_",thresholds)
#read Efficiency rand
delta_Erand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/delta_Erand.csv", header=FALSE)
colnames(delta_Erand)<-paste("Erand_",thresholds)
#read CostEfficiency
delta_CE<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/delta_CE.csv", header=FALSE)
colnames(delta_CE)<-paste("CE_",thresholds)
#read CosteEfficiency rand
delta_CErand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/delta_CErand.csv", header=FALSE)
colnames(delta_CErand)<-paste("CErand_",thresholds)
#read Degree
delta_deg<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/delta_deg.csv", header=FALSE)
colnames(delta_deg)<-paste("deg_",thresholds)
#read Small World
delta_SW<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/delta_SW.csv", header=FALSE)
colnames(delta_SW)<-paste("SW_",thresholds)

#bind to one table per frequency band
EO_delta_network<-cbind(EEG_order,delta_C,delta_Crand, delta_L,delta_Lrand,
                        delta_E, delta_Erand,delta_CE,delta_CErand,delta_deg,delta_SW)
#match deltaresults and network
index<-match(x=allresults$VP_Code, table=EO_delta_network$VP_Code_EEG)
networkindex<-rbind(EO_delta_network[index,])
EO_delta_network<-cbind(allresults,networkindex)

############################################
#theta
############################################
# directory
setwd("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO")
#read threshold levels
thresholds<-c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10")
theta_cost<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/theta_cost.csv", header=FALSE)
colnames(theta_cost)<-thresholds
#read Clustering
theta_C<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/theta_C.csv", header=FALSE)
colnames(theta_C)<-paste("C_",thresholds)
#read Clustering rand
theta_Crand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/theta_Crand.csv", header=FALSE)
colnames(theta_Crand)<-paste("Crand_",thresholds)
#read Path length
theta_L<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/theta_L.csv", header=FALSE)
colnames(theta_L)<-paste("L_",thresholds)
#read Path length rand
theta_Lrand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/theta_Lrand.csv", header=FALSE)
colnames(theta_Lrand)<-paste("Lrand_",thresholds)
#read Efficiency
theta_E<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/theta_E.csv", header=FALSE)
colnames(theta_E)<-paste("E_",thresholds)
#read Efficiency rand
theta_Erand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/theta_Erand.csv", header=FALSE)
colnames(theta_Erand)<-paste("Erand_",thresholds)
#read CostEfficiency
theta_CE<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/theta_CE.csv", header=FALSE)
colnames(theta_CE)<-paste("CE_",thresholds)
#read CosteEfficiency rand
theta_CErand<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/theta_CErand.csv", header=FALSE)
colnames(theta_CErand)<-paste("CErand_",thresholds)
#read Degree
theta_deg<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/theta_deg.csv", header=FALSE)
colnames(theta_deg)<-paste("deg_",thresholds)
#read Small World
theta_SW<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/zGraph Theory/EO/theta_SW.csv", header=FALSE)
colnames(theta_SW)<-paste("SW_",thresholds)

#bind to one table per frequency band
EO_theta_network<-cbind(EEG_order,theta_C,theta_Crand, theta_L,theta_Lrand,
                        theta_E, theta_Erand,theta_CE,theta_CErand,theta_deg,theta_SW)
#match thetaresults and network
index<-match(x=allresults$VP_Code, table=EO_theta_network$VP_Code_EEG)
networkindex<-rbind(EO_theta_network[index,])
EO_theta_network<-cbind(allresults,networkindex)