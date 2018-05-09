###plot individual subjects graphs


#BETA
##################################################sibling/twins pair!!!
#RP
##########
RPtwin<-as.matrix(rawMAT_AR30KAI134_RS_EO.mat)
diag(RPtwin)<-0
rownames(RPtwin)<-electrode_position$name
colnames(RPtwin)<-c("V253"="FP1","V254"="FP2","V255"= "F3","V256"="F4","V257"="C3","V258"="C4",
                     "V259"="P3","V260"="P4","V261"="O1","V262"="O2","V263"="F7","V264"="F8",
                    "V265"="T7","V266"="T8","V267"="P7","V268"="P8","V269"="Cz","V270"="Fz",
                    "V271"="Pz","V272"="CP3","V273"="CP4","V274"="FC3","V275"="FC4","V276"="TP7", 
                    "V277"="TP8","V278"="FT8","V279"="Oz","V280"="FT7")
heatmap(RPtwin,Rowv = NA,Colv =NA)
RPtwin<-RPtwin[new_el_order,new_el_order]
RPtwin[RPtwin< quantile(RPtwin,prob=80/100,na.rm=TRUE)]<-0
RPtwin<-graph_from_adjacency_matrix(RPtwin, mode = c("max"), weighted = TRUE, diag = TRUE)
plot(RPtwin,layout=layout_in_circle(RPtwin))
summary(RPtwin)

##########
#AP
##########
APtwin<-as.matrix(rawMAT_LI30KAI134_RS_EO.mat)
diag(APtwin)<-0
rownames(APtwin)<-electrode_position$name
colnames(APtwin)<-c("V253"="FP1","V254"="FP2","V255"= "F3","V256"="F4","V257"="C3","V258"="C4",
                  "V259"="P3","V260"="P4","V261"="O1","V262"="O2","V263"="F7","V264"="F8",
                  "V265"="T7","V266"="T8","V267"="P7","V268"="P8","V269"="Cz","V270"="Fz",
                  "V271"="Pz","V272"="CP3","V273"="CP4","V274"="FC3","V275"="FC4","V276"="TP7", 
                  "V277"="TP8","V278"="FT8","V279"="Oz","V280"="FT7")
heatmap(APtwin,Rowv = NA,Colv =NA)
APtwin<-APtwin[new_el_order,new_el_order]
APtwin[APtwin< quantile(APtwin,prob=80/100,na.rm=TRUE)]<-0
APtwin<-graph_from_adjacency_matrix(APtwin, mode = c("max"), weighted = TRUE, diag = TRUE)
plot(APtwin,layout=layout_in_circle(APtwin))
summary(APtwin)



###plot individual subjects graphs


#DELTA
##################################################sibling/twins pair!!!
#RP
##########
RPtwin_d<-as.matrix(rawMAT_AR30KAI134_RS_EC.mat)
diag(RPtwin_d)<-0
rownames(RPtwin_d)<-electrode_position$name
colnames(RPtwin_d)<-c("V253"="FP1","V254"="FP2","V255"= "F3","V256"="F4","V257"="C3","V258"="C4",
                    "V259"="P3","V260"="P4","V261"="O1","V262"="O2","V263"="F7","V264"="F8",
                    "V265"="T7","V266"="T8","V267"="P7","V268"="P8","V269"="Cz","V270"="Fz",
                    "V271"="Pz","V272"="CP3","V273"="CP4","V274"="FC3","V275"="FC4","V276"="TP7", 
                    "V277"="TP8","V278"="FT8","V279"="Oz","V280"="FT7")

heatmap(RPtwin_d,Rowv = NA,Colv =NA)
RPtwin_d<-RPtwin_d[new_el_order,new_el_order]
RPtwin_d[RPtwin_d< quantile(RPtwin_d,prob=85/100,na.rm=TRUE)]<-0
RPtwin_d<-graph_from_adjacency_matrix(RPtwin_d, mode = c("max"), weighted = TRUE, diag = TRUE)
plot(RPtwin_d,layout=layout_in_circle(RPtwin_d))
summary(RPtwin_d)

##########
#AP
##########
APtwin_d<-as.matrix(rawMAT_LI30KAI134_RS_EC.mat)
diag(APtwin_d)<-0
rownames(APtwin_d)<-electrode_position$name
colnames(APtwin_d)<-c("V253"="FP1","V254"="FP2","V255"= "F3","V256"="F4","V257"="C3","V258"="C4",
                    "V259"="P3","V260"="P4","V261"="O1","V262"="O2","V263"="F7","V264"="F8",
                    "V265"="T7","V266"="T8","V267"="P7","V268"="P8","V269"="Cz","V270"="Fz",
                    "V271"="Pz","V272"="CP3","V273"="CP4","V274"="FC3","V275"="FC4","V276"="TP7", 
                    "V277"="TP8","V278"="FT8","V279"="Oz","V280"="FT7")
heatmap(APtwin_d,Rowv = NA,Colv =NA)
APtwin_d<-APtwin_d[new_el_order,new_el_order]
APtwin_d[APtwin_d< quantile(APtwin_d,prob=85/100,na.rm=TRUE)]<-0
APtwin_d<-graph_from_adjacency_matrix(APtwin_d, mode = c("max"), weighted = TRUE, diag = TRUE)
plot(APtwin_d,layout=layout_in_circle(APtwin_d))
summary(APtwin_d)