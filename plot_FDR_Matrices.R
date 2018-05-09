### plot Matric comparison via permutation statistics
library(ggplot2)
electrode_position<-read.csv("~/Teresa/files/EEG Statistics/electrode_positions.csv", header=FALSE)
colnames(electrode_position)<-c("x","y","z","name","hemisph")
electrode_position$name<-as.character(electrode_position$name)

new_el_order_heat<-c("FP1","F3","FC3","F7","FT7","C3","CP3","T7","TP7","P3","P7","O1",
                     "Fz","Cz","Pz","Oz","O2","P8","P4","TP8","T8","CP4","C4","FT8","F8","FC4","F4","FP2")
new_el_order_heat2<-c("FP1","F3","FC3","F7","FT7","C3","CP3","T7","TP7","P3","P7","O1",
                      "Fz","Cz","Pz","Oz","O2","P8","P4","TP8","T8","CP4","C4","FT8","F8","FC4","F4","FP2")


gather.matrix <- function(mat) {
  if (is.null(dimnames(mat))) {
    grid <- expand.grid(seq.int(nrow(mat)), seq.int(ncol(mat)))
  } else {
    grid <- expand.grid(dimnames(mat))
  }
  cbind(grid, value = as.vector(mat))
}
######################
#BETA
#####################



FDR_beta<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/FDR_beta.csv", header=FALSE)
outP_beta<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/outP_beta.csv", header=FALSE)
p_fdr_beta<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/p_fdr_beta.csv", header=FALSE)
outD_beta<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/outD_beta.csv", header=FALSE)
FDR_beta<-as.matrix(FDR_beta)
outP_beta<-as.matrix(outP_beta)
outD_beta<-as.matrix(outD_beta)
rownames(FDR_beta)<-electrode_position$name
colnames(FDR_beta)<-electrode_position$name
rownames(outP_beta)<-electrode_position$name
colnames(outP_beta)<-electrode_position$name
rownames(outD_beta)<-electrode_position$name
colnames(outD_beta)<-electrode_position$name
FDR_beta<-FDR_beta[new_el_order_heat2,new_el_order_heat2]
outP_beta<-outP_beta[new_el_order_heat2,new_el_order_heat2]
outD_beta<-outD_beta[new_el_order_heat2,new_el_order_heat2]

heatmap(outP_beta,Rowv = NA,Colv =NA) #hemispheres seperate
heatmap(FDR_beta,Rowv = NA,Colv =NA)
heatmap(outP_beta,Rowv = NULL,Colv =NULL) #hemispheres seperate
heatmap(FDR_beta,Rowv = NULL,Colv =NULL)



############################
#DELTA
###########################

### plot Matric comparison via permutation statistics
electrode_position<-read.csv("~/Teresa/files/EEG Statistics/electrode_positions.csv", header=FALSE)
colnames(electrode_position)<-c("x","y","z","name","hemisph")
electrode_position$name<-as.character(electrode_position$name)

new_el_order_heat<-c("FP1","F3","C3","CP3","FC3","F7","FT7","T7","TP7","P3","P7","O1",
                     "Fz","Cz","Pz","Oz",
                     "FP2","F4","C4","CP4","FC4","F8","FT8","T8","TP8","P4","P8","O2")



FDR_delta<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/FDR_delta.csv", header=FALSE)
outP_delta<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/outP_delta.csv", header=FALSE)
p_fdr_delta<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/p_fdr_delta.csv", header=FALSE)
outD_delta<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/outD_delta.csv", header=FALSE)
FDR_delta<-as.matrix(FDR_delta)
outP_delta<-as.matrix(outP_delta)
outD_delta<-as.matrix(outD_delta)
rownames(FDR_delta)<-electrode_position$name
colnames(FDR_delta)<-electrode_position$name
rownames(outP_delta)<-electrode_position$name
colnames(outP_delta)<-electrode_position$name
rownames(outD_delta)<-electrode_position$name
colnames(outD_delta)<-electrode_position$name
FDR_delta<-FDR_delta[new_el_order_heat2,new_el_order_heat2]
outP_delta<-outP_delta[new_el_order_heat2,new_el_order_heat2]
outD_delta<-outD_delta[new_el_order_heat2,new_el_order_heat2]

heatmap(outP_delta,Rowv = NA,Colv =NA) #hemispheres seperate
heatmap(FDR_delta,Rowv = NA,Colv =NA)
heatmap(outP_delta,Rowv = NULL,Colv =NULL) #hemispheres seperate
heatmap(FDR_delta,Rowv = NULL,Colv =NULL)




######################
#beta_z
#####################



FDR_beta_z<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/FDR_beta_z.csv", header=FALSE)
outP_beta_z<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/outP_beta_z.csv", header=FALSE)
p_fdr_beta_z<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/p_fdr_beta_z.csv", header=FALSE)
outD_beta_z<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/outD_beta_z.csv", header=FALSE)
FDR_beta_z<-as.matrix(FDR_beta_z)
outP_beta_z<-as.matrix(outP_beta_z)
outD_beta_z<-as.matrix(outD_beta_z)
rownames(FDR_beta_z)<-electrode_position$name
colnames(FDR_beta_z)<-electrode_position$name
rownames(outP_beta_z)<-electrode_position$name
colnames(outP_beta_z)<-electrode_position$name
rownames(outD_beta_z)<-electrode_position$name
colnames(outD_beta_z)<-electrode_position$name
FDR_beta_z<-FDR_beta_z[new_el_order_heat2,new_el_order_heat2]
outP_beta_z<-outP_beta_z[new_el_order_heat2,new_el_order_heat2]
outD_beta_z<-outD_beta_z[new_el_order_heat2,new_el_order_heat2]

heatmap(outP_beta_z,Rowv = NA,Colv =NA) #hemispheres seperate
heatmap(FDR_beta_z,Rowv = NA,Colv =NA)
heatmap(outP_beta_z,Rowv = NULL,Colv =NULL) #hemispheres seperate
heatmap(FDR_beta_z,Rowv = NULL,Colv =NULL)



############################
#delta_z
###########################

### plot Matric comparison via permutation statistics
electrode_position<-read.csv("~/Teresa/files/EEG Statistics/electrode_positions.csv", header=FALSE)
colnames(electrode_position)<-c("x","y","z","name","hemisph")
electrode_position$name<-as.character(electrode_position$name)

new_el_order_heat<-c("FP1","F3","C3","CP3","FC3","F7","FT7","T7","TP7","P3","P7","O1",
                     "Fz","Cz","Pz","Oz",
                     "FP2","F4","C4","CP4","FC4","F8","FT8","T8","TP8","P4","P8","O2")



FDR_delta_z<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/FDR_delta_z.csv", header=FALSE)
outP_delta_z<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/outP_delta_z.csv", header=FALSE)
p_fdr_delta_z<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/p_fdr_delta_z.csv", header=FALSE)
outD_delta_z<-read.csv("~/Teresa/files/EEG Statistics/zGraph Theory/single_connection_test/outD_delta_z.csv", header=FALSE)
FDR_delta_z<-as.matrix(FDR_delta_z)
outP_delta_z<-as.matrix(outP_delta_z)
outD_delta_z<-as.matrix(outD_delta_z)
rownames(FDR_delta_z)<-electrode_position$name
colnames(FDR_delta_z)<-electrode_position$name
rownames(outP_delta_z)<-electrode_position$name
colnames(outP_delta_z)<-electrode_position$name
rownames(outD_delta_z)<-electrode_position$name
colnames(outD_delta_z)<-electrode_position$name
FDR_delta_z<-FDR_delta_z[new_el_order_heat2,new_el_order_heat2]
outP_delta_z<-outP_delta_z[new_el_order_heat2,new_el_order_heat2]
outD_delta_z<-outD_delta_z[new_el_order_heat2,new_el_order_heat2]

heatmap(outP_delta_z,Rowv = NA,Colv =NA) #hemispheres seperate
heatmap(FDR_delta_z,Rowv = NA,Colv =NA)
heatmap(outP_delta_z,Rowv = NULL,Colv =NULL) #hemispheres seperate
heatmap(FDR_delta_z,Rowv = NULL,Colv =NULL)




#################################
#Grafics
#################################

##P
outP_beta_gathered <- gather.matrix(outP_beta)
labs_betaP <- c(apply(round(outP_beta[, ], 2), 2, as.character))
ggplot(data = outP_beta_gathered, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Single Connection p-values AP vs. RP (beta)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "p-values")+geom_text(aes(label=labs_betaP), size=2)

outP_delta_gathered <- gather.matrix(outP_delta)
labs_deltaP <- c(apply(round(outP_delta[, ], 2), 2, as.character))
ggplot(data = outP_delta_gathered, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Single Connection p-values AP vs. RP (delta)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "p-values")+geom_text(aes(label=labs_deltaP), size=2)
##z-values
outP_beta_gathered_z <- gather.matrix(outP_beta_z)
labs_betaP_z <- c(apply(round(outP_beta_z[, ], 2), 2, as.character))
ggplot(data = outP_beta_gathered_z, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Single Connection p-values AP vs. RP (beta,z)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "p-values")+geom_text(aes(label=labs_betaP), size=2)

outP_delta_gathered_z <- gather.matrix(outP_delta_z)
labs_deltaP_z <- c(apply(round(outP_delta_z[, ], 2), 2, as.character))
ggplot(data = outP_delta_gathered_z, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Single Connection p-values AP vs. RP (delta,z)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "p-values")+geom_text(aes(label=labs_deltaP), size=2)



###effect size tables! negative values= RP>AP, positive values=AP>RP
outD_beta_gathered <- gather.matrix(outD_beta)
labs_betaD <- c(apply(round(outD_beta[, ], 2), 2, as.character))
ggplot(data = outD_beta_gathered, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Single Connection Cohens D AP vs. RP (beta)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "D-values")+geom_text(aes(label=labs_betaD), size=2)

outD_delta_gathered <- gather.matrix(outD_delta)
labs_deltaD <- c(apply(round(outD_delta[, ], 2), 2, as.character))
ggplot(data = outD_delta_gathered, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Single Connection Cohens D AP vs. RP (delta)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "D-values")+geom_text(aes(label=labs_deltaD), size=2)

#z-values
outD_beta_gathered_z <- gather.matrix(outD_beta_z)
labs_betaD_z <- c(apply(round(outD_beta_z[, ], 2), 2, as.character))
ggplot(data = outD_beta_gathered_z, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Single Connection Cohens D AP vs. RP (beta)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "D-values")+geom_text(aes(label=labs_betaD_z), size=2)

outD_delta_gathered_z <- gather.matrix(outD_delta_z)
labs_deltaD_z <- c(apply(round(outD_delta_z[, ], 2), 2, as.character))
ggplot(data = outD_delta_gathered_z, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Single Connection Cohens D AP vs. RP (delta)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "D-values")+geom_text(aes(label=labs_deltaD_z), size=2)
##FDR

FDR_delta_gathered <- gather.matrix(FDR_delta)
ggplot(data = FDR_delta_gathered, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Single Connection differences (Cohens D) AP vs. RP (delta)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "p-value<FDR")+geom_text(aes(label=labs_deltaD), size=2)

FDR_beta_gathered <- gather.matrix(FDR_beta)
ggplot(data = FDR_beta_gathered, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Single Connection differences (Cohens D) AP vs. RP (beta)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "p-value<FDR")+geom_text(aes(label=labs_betaD), size=2)
#z-values

FDR_delta_gathered_z <- gather.matrix(FDR_delta_z)
ggplot(data = FDR_delta_gathered_z, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Single Connection differences (Cohens D) AP vs. RP (delta,z)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "p-value<FDR")+geom_text(aes(label=labs_deltaD_z), size=2)

FDR_beta_gathered_z <- gather.matrix(FDR_beta_z)
ggplot(data = FDR_beta_gathered_z, aes(x=Var1, y=Var2, fill=value)) + geom_tile()+ ylab("Electrodes") +
  xlab("Electrodes") +ggtitle("Single Connection differences (Cohens D) AP vs. RP (beta,z)")+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1))+labs(fill = "p-value<FDR")+geom_text(aes(label=labs_betaD_z), size=2)

###################################################
#Plot differences as graphs
###################################################
diag(FDR_beta)<-0
FDR_beta<-FDR_beta[new_el_order,new_el_order]
graph_FDR_beta<-graph_from_adjacency_matrix(FDR_beta, mode = c("max"), weighted = TRUE, diag = TRUE)
plot(graph_FDR_beta,layout=layout_in_circle(graph_FDR_beta))
summary(graph_FDR_beta)

diag(FDR_delta)<-0
FDR_delta<-FDR_delta[new_el_order,new_el_order]
graph_FDR_delta<-graph_from_adjacency_matrix(FDR_delta, mode = c("max"), weighted = TRUE, diag = TRUE)
plot(graph_FDR_delta,layout=layout_in_circle(graph_FDR_delta))
summary(graph_FDR_delta)

diag(FDR_beta_z)<-0
FDR_beta_z<-FDR_beta_z[new_el_order,new_el_order]
graph_FDR_beta_z<-graph_from_adjacency_matrix(FDR_beta_z, mode = c("max"), weighted = TRUE, diag = TRUE)
plot(graph_FDR_beta_z,layout=layout_in_circle(graph_FDR_beta_z))
summary(graph_FDR_beta_z)

diag(FDR_delta_z)<-0
FDR_delta_z<-FDR_delta_z[new_el_order,new_el_order]
graph_FDR_delta_z<-graph_from_adjacency_matrix(FDR_delta_z, mode = c("max"), weighted = TRUE, diag = TRUE)
plot(graph_FDR_delta_z,layout=layout_in_circle(graph_FDR_delta_z))
summary(graph_FDR_delta_z)

####################################################
# Plot difference graphs on brain (export csv )
####################################################
diag(FDR_beta)<-0
FDR_beta<-FDR_beta[electrode_position$name,electrode_position$name]
outD_beta<-as.matrix(outD_beta)
outD_beta<-outD_beta[electrode_position$name,electrode_position$name]
difference_beta<-FDR_beta*outD_beta
diag(difference_beta)<-0
write.csv(difference_beta, "difference_beta.csv")

diag(FDR_beta_z)<-0
FDR_beta_z<-FDR_beta_z[electrode_position$name,electrode_position$name]
outD_beta_z<-as.matrix(outD_beta_z)
outD_beta_z<-outD_beta_z[electrode_position$name,electrode_position$name]
difference_beta_z<-FDR_beta_z*outD_beta_z
diag(difference_beta_z)<-0
write.csv(difference_beta_z, "difference_beta_z.csv")

diag(FDR_delta)<-0
FDR_delta<-FDR_delta[electrode_position$name,electrode_position$name]
outD_delta<-as.matrix(outD_delta)
outD_delta<-outD_delta[electrode_position$name,electrode_position$name]
difference_delta<-FDR_delta*outD_delta
diag(difference_delta)<-0
write.csv(difference_delta, "difference_delta.csv")

diag(FDR_delta_z)<-0
FDR_delta_z<-FDR_delta_z[electrode_position$name,electrode_position$name]
outD_delta_z<-as.matrix(outD_delta_z)
outD_delta_z<-outD_delta_z[electrode_position$name,electrode_position$name]
difference_delta_z<-FDR_delta_z*outD_delta_z
diag(difference_delta_z)<-0
write.csv(difference_delta_z, "difference_delta_z.csv")
