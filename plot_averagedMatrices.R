#plot averaged binary matrices of subjects per group
#####################electrode positions
electrode_position<-read.csv("~/Backup PC Cambridge/Teresa/files/EEG Statistics/electrode_positions.csv", header=FALSE)
colnames(electrode_position)<-c("x","y","z","name","hemisph")
electrode_position$name<-as.character(electrode_position$name)
elnames<-list(electrode_position$name,electrode_position$name)

new_el_order<-c("Pz","C4","FT8","F8","FC4","F4","FP2","Fz","FP1","F3","FC3","F7","FT7","C3","Cz","CP3","T7","TP7","P3","P7","O1",
                "Oz","O2","P8","P4","TP8","T8","CP4")
new_el_order_heat<-c("FP1","F3","C3","CP3","FC3","F7","FT7","T7","TP7","P3","P7","O1",
                     "Fz","Cz","Pz","Oz",
                     "FP2","F4","C4","CP4","FC4","F8","FT8","T8","TP8","P4","P8","O2")
new_el_order_heat2<-c("FP1","F3","FC3","F7","FT7","C3","CP3","T7","TP7","P3","P7","O1",
                     "Fz","Cz","Pz","Oz","O2","P8","P4","TP8","T8","CP4","C4","FT8","F8","FC4","F4","FP2")

# read and average MAT matrices (delta )

#############################delta EC
setwd("~/Backup PC Cambridge/zRS_EC_all_rejected_fieldtrip/done/networks_delta")
filenames <- list.files(path="~/Backup PC Cambridge/zRS_EC_all_rejected_fieldtrip/done/networks_delta",
                        pattern = "rawMAT_[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_RS_EC.mat.csv") # to list all files in a folder, ggf. define type of the file
#read in tables

for (i in filenames) {  
  name <- gsub("-",".",i) # gsub(pattern, replacement, x)
  name <- gsub(".csv","",name)  
  i <- paste(".\\",i,sep="")
  assign(name,read.csv(i, header=FALSE)) #read in the table and name as "name"
}

#create index of AP vs. RP in EEG order
EEG_order<-cbind(EEG_order, EEG_order)
VPindex<-match(x=EEG_order$VP_Code_EEG, table=EO_delta_network$VP_Code)
VPindex<-rbind(EO_delta_network[VPindex,])
VPindex<-data.frame(VPindex$VP_Code, VPindex$exclude.0.no.1.elec.2.med.3.exp.,VPindex$AP.0.RP.1.AP.,VPindex$AQ_Score)
colnames(VPindex)<-c("VP_Code","excluded","APRP","AQ_Score")

#take only selected threshold
file_list_Matrices<-ls(pattern="rawMAT_[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_RS_EC.mat") 
Matrix_list <- vector("list", "length" = length(file_list_Matrices))
for (i in 1:length(file_list_Matrices)) { 
  Matrices<-get(file_list_Matrices[i])
  delta<-Matrices[,] #change threshold index +freqband
  delta<-as.matrix(delta)
  diag(delta)<-NA  #set diagonale to NA
  mean_M<-mean(delta,na.rm=TRUE)
  sd_M<-sd(delta,na.rm=TRUE)
  delta<-((delta-mean_M)/sd_M)#normalized matrices
  Matrix_list[[i]]<-delta
}
delta_Matrices<-Matrix_list #change threshold name +freqband


#sum over AP vs. RP
is.AP<-VPindex$APRP
is.excl<-VPindex$excluded
AP_MAtrices_excl_delta<-delta_Matrices[which(is.AP==1&is.excl==0)]
RP_MAtrices_excl_delta<-delta_Matrices[which(is.AP==0&is.excl==0)]
#z_AP_MAtrices_excl_delta<-apply(simplify2array(AP_MAtrices_excl_delta),1:2,scale)
#z_RP_MAtrices_excl_delta<-apply(simplify2array(RP_MAtrices_excl_delta),1:2,scale)
#z_grandavg_delta<-apply(simplify2array(delta_Matrices),1:2,scale)

grandavg_delta<-apply(simplify2array(delta_Matrices),1:2,mean)
APavg_delta<-apply(simplify2array(AP_MAtrices_excl_delta),1:2,mean)
RPavg_delta<-apply(simplify2array(RP_MAtrices_excl_delta),1:2,mean)

rownames(APavg_delta)<-electrode_position$name
colnames(APavg_delta)<-c("V253"="FP1","V254"="FP2","V255"= "F3","V256"="F4","V257"="C3","V258"="C4",
                            "V259"="P3","V260"="P4","V261"="O1","V262"="O2","V263"="F7","V264"="F8",
                            "V265"="T7","V266"="T8","V267"="P7","V268"="P8","V269"="Cz","V270"="Fz",
                            "V271"="Pz","V272"="CP3","V273"="CP4","V274"="FC3","V275"="FC4","V276"="TP7", 
                            "V277"="TP8","V278"="FT8","V279"="Oz","V280"="FT7")

APavg_delta<-APavg_delta[new_el_order_heat2,new_el_order_heat2] #reorder matrix

rownames(RPavg_delta)<-electrode_position$name
colnames(RPavg_delta)<-c("V253"="FP1","V254"="FP2","V255"= "F3","V256"="F4","V257"="C3","V258"="C4",
                            "V259"="P3","V260"="P4","V261"="O1","V262"="O2","V263"="F7","V264"="F8",
                            "V265"="T7","V266"="T8","V267"="P7","V268"="P8","V269"="Cz","V270"="Fz",
                            "V271"="Pz","V272"="CP3","V273"="CP4","V274"="FC3","V275"="FC4","V276"="TP7", 
                            "V277"="TP8","V278"="FT8","V279"="Oz","V280"="FT7")
RPavg_delta<-RPavg_delta[new_el_order_heat2,new_el_order_heat2]
#heatmap
heatmap(RPavg_delta,Rowv = NA,Colv =NA) #hemispheres seperate

library("ggplot2", lib.loc="~/R/win-library/3.2")
RPavg_delta_gathered <- gather.matrix(RPavg_delta)
ggplot(data = RPavg_delta_gathered, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Averaged z-normalized matrices (delta, RP)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "z-values")

heatmap(APavg_delta,Rowv = NA,Colv =NA)

APavg_delta_gathered <- gather.matrix(APavg_delta)
ggplot(data = APavg_delta_gathered, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Averaged z-normalized matrices (delta, AP)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "z-values")



heatmap(RPavg_delta,Rowv = NULL,Colv =NULL) #cluster algorithm
heatmap(APavg_delta,Rowv = NULL,Colv =NULL)


#threshold for igraphs
APavg_delta<-APavg_delta[new_el_order,new_el_order] #reorder matrix
RPavg_delta<-RPavg_delta[new_el_order,new_el_order]
APavg_delta[APavg_delta < quantile(APavg_delta,prob=80/100,na.rm=TRUE)]<-0 
#APavg_delta[APavg_delta<1]<-0
APavg_delta[is.na(APavg_delta)]<-0
RPavg_delta[RPavg_delta < quantile(RPavg_delta,prob=80/100,na.rm=TRUE)]<-0 
#RPavg_delta[RPavg_delta<1]<-0
RPavg_delta[is.na(RPavg_delta)]<-0



#igraph
library("igraph", lib.loc="~/R/win-library/3.2")
graph_AP_delta<-graph_from_adjacency_matrix(APavg_delta, mode = c("max"), weighted = TRUE, diag = TRUE)
plot(graph_AP_delta,layout=layout_in_circle(graph_AP_delta))
summary(graph_AP_delta)

graph_RP_delta<-graph_from_adjacency_matrix(RPavg_delta, mode = c("max"), weighted = TRUE, diag = TRUE)
plot(graph_RP_delta,layout=layout_in_circle(graph_RP_delta))
summary(graph_RP_delta)


#############################################
#beta
setwd("~/Backup PC Cambridge/zRS_EO_all_rejected_fieldtrip/done/networks_beta")
filenames <- list.files(path="~/Backup PC Cambridge/zRS_EO_all_rejected_fieldtrip/done/networks_beta",
                        pattern = "rawMAT_[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_RS_EO.mat.csv") # to list all files in a folder, ggf. define type of the file
#read in tables
for (i in filenames) {  
  name <- gsub("-",".",i) # gsub(pattern, replacement, x)
  name <- gsub(".csv","",name)  
  i <- paste(".\\",i,sep="")
  assign(name,read.csv(i, header=FALSE)) #read in the table and name as "name"
}

#create index of AP vs. RP in EEG order
#EEG_order<-cbind(EEG_order, EEG_order)
VPindex<-match(x=EEG_order$VP_Code_EEG, table=EC_beta_network$VP_Code)
VPindex<-rbind(EC_beta_network[VPindex,])
VPindex<-data.frame(VPindex$VP_Code, VPindex$exclude.0.no.1.elec.2.med.3.exp.,VPindex$AP.0.RP.1.AP.)
colnames(VPindex)<-c("VP_Code","excluded","APRP")

#take only selected threshold
file_list_Matrices<-ls(pattern="rawMAT_[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_RS_EO.mat") 
Matrix_list <- vector("list", "length" = length(file_list_Matrices))
for (i in 1:length(file_list_Matrices)) { 
  Matrices<-get(file_list_Matrices[i])
  beta<-Matrices[,] #change threshold index +freqband
  beta<-as.matrix(beta)
  diag(beta)<-NA  #set diagonale to NA
  mean_M<-mean(beta,na.rm=TRUE)
  sd_M<-sd(beta,na.rm=TRUE)
  beta<-((beta-mean_M)/sd_M)#normalized matrices
  Matrix_list[[i]]<-beta
}
beta_Matrices<-Matrix_list #change threshold name +freqband

#sum over AP vs. RP
is.AP<-VPindex$APRP
is.excl<-VPindex$excluded
AP_MAtrices_excl_beta<-beta_Matrices[which(is.AP==1&is.excl==0)]
RP_MAtrices_excl_beta<-beta_Matrices[which(is.AP==0&is.excl==0)]
grandavg_beta<-apply(simplify2array(beta_Matrices),1:2,mean)
APavg_beta<-apply(simplify2array(AP_MAtrices_excl_beta),1:2,mean)#make mean network
RPavg_beta<-apply(simplify2array(RP_MAtrices_excl_beta),1:2,mean)

rownames(APavg_beta)<-electrode_position$name
colnames(APavg_beta)<-c("V253"="FP1","V254"="FP2","V255"= "F3","V256"="F4","V257"="C3","V258"="C4",
                            "V259"="P3","V260"="P4","V261"="O1","V262"="O2","V263"="F7","V264"="F8",
                            "V265"="T7","V266"="T8","V267"="P7","V268"="P8","V269"="Cz","V270"="Fz",
                            "V271"="Pz","V272"="CP3","V273"="CP4","V274"="FC3","V275"="FC4","V276"="TP7", 
                            "V277"="TP8","V278"="FT8","V279"="Oz","V280"="FT7")

APavg_beta<-APavg_beta[new_el_order_heat2,new_el_order_heat2] #reorder matrix

rownames(RPavg_beta)<-electrode_position$name
colnames(RPavg_beta)<-c("V253"="FP1","V254"="FP2","V255"= "F3","V256"="F4","V257"="C3","V258"="C4",
                            "V259"="P3","V260"="P4","V261"="O1","V262"="O2","V263"="F7","V264"="F8",
                            "V265"="T7","V266"="T8","V267"="P7","V268"="P8","V269"="Cz","V270"="Fz",
                            "V271"="Pz","V272"="CP3","V273"="CP4","V274"="FC3","V275"="FC4","V276"="TP7", 
                            "V277"="TP8","V278"="FT8","V279"="Oz","V280"="FT7")
RPavg_beta<-RPavg_beta[new_el_order_heat2,new_el_order_heat2]
#heatmap
heatmap(RPavg_beta,Rowv = NA,Colv =NA) #hemispheres seperate

RPavg_beta_gathered <- gather.matrix(RPavg_beta)
ggplot(data = RPavg_beta_gathered, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Averaged z-normalized matrices (beta, RP)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "z-values")

heatmap(APavg_beta,Rowv = NA,Colv =NA)

APavg_beta_gathered <- gather.matrix(APavg_beta)
ggplot(data = APavg_beta_gathered, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Averaged z-normalized matrices (beta, AP)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "z-values")

heatmap(RPavg_beta,Rowv = NULL,Colv =NULL) #cluster algorithm
heatmap(APavg_beta,Rowv = NULL,Colv =NULL)


#threshold for igraphs
APavg_beta<-APavg_beta[new_el_order,new_el_order] #reorder matrix
RPavg_beta<-RPavg_beta[new_el_order,new_el_order]
APavg_beta[APavg_beta< quantile(APavg_beta,prob=80/100,na.rm=TRUE)]<-0 #binarize highest 20%
#APavg_beta[APavg_beta<1]<-0
APavg_beta[is.na(APavg_beta)]<-0
RPavg_beta[RPavg_beta< quantile(RPavg_beta,prob=80/100,na.rm=TRUE)]<-0
#RPavg_beta[RPavg_beta<1]<-0
RPavg_beta[is.na(RPavg_beta)]<-0


#igraph
library("igraph", lib.loc="~/R/win-library/3.2")
graph_AP_beta<-graph_from_adjacency_matrix(APavg_beta, mode = c("max"), weighted = TRUE, diag = TRUE)
plot(graph_AP_beta,layout=layout_in_circle(graph_AP_beta))
summary(graph_AP_beta)

graph_RP_beta<-graph_from_adjacency_matrix(RPavg_beta, mode = c("max"), weighted = TRUE, diag = TRUE)
plot(graph_RP_beta,layout=layout_in_circle(graph_RP_beta))
summary(graph_RP_beta)
