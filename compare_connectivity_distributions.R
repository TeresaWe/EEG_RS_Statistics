library(ggplot2)
library(ggridges)


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

###compare connectivity distributions!
###################
#delta
###################


#take only selected threshold
file_list_Matrices<-ls(pattern="rawMAT_[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_RS_EC.mat") 
Matrix_list <- vector("list", "length" = length(file_list_Matrices))
for (i in 1:length(file_list_Matrices)) { 
  Matrices<-get(file_list_Matrices[i])
  delta<-Matrices[,] #change threshold index +freqband
  delta<-as.matrix(delta)
  diag(delta)<-NA  #set diagonale to NA
  Matrix_list[[i]]<-delta
}
delta_Matrices_raw<-Matrix_list #change threshold name +freqband

AP_MAtrices_excl_delta_raw<-delta_Matrices_raw[which(is.AP==1&is.excl==0)]#AP
RP_MAtrices_excl_delta_raw<-delta_Matrices_raw[which(is.AP==0&is.excl==0)]#RP
AQ_MAtrices_excl_delta_raw<-delta_Matrices_raw[which(is.AQ==2&is.excl==0)] #high AQ
mAQ_MAtrices_excl_delta_raw<-delta_Matrices_raw[which(is.AQ==1&is.excl==0)]#medium AQ
nAQ_MAtrices_excl_delta_raw<-delta_Matrices_raw[which(is.AQ==0&is.excl==0)]#low AQ
APAQ_MAtrices_excl_delta_raw<-delta_Matrices_raw[which(is.AQ>=1&is.excl==0&is.AP==1)] #high AQ+AP
APnAQ_MAtrices_excl_delta_raw<-delta_Matrices_raw[which(is.AQ==1&is.excl==0&is.AP==1)] #high AQ+AP

APavg_delta_raw<-simplify2array(AP_MAtrices_excl_delta_raw)
RPavg_delta_raw<-simplify2array(RP_MAtrices_excl_delta_raw)
AQavg_delta_raw<-simplify2array(AQ_MAtrices_excl_delta_raw)
mAQavg_delta_raw<-simplify2array(mAQ_MAtrices_excl_delta_raw)
nAQavg_delta_raw<-simplify2array(nAQ_MAtrices_excl_delta_raw)
APAQavg_delta_raw<-simplify2array(APAQ_MAtrices_excl_delta_raw)
APnAQavg_delta_raw<-simplify2array(APnAQ_MAtrices_excl_delta_raw)

p1<-hist(APavg_delta_raw)
p2<-hist(RPavg_delta_raw)
###DENSITIES
##APvsRP
APdens_delta<-density(APavg_delta_raw,na.rm=TRUE)
RPdens_delta<-density(RPavg_delta_raw,na.rm=TRUE)
plot( APdens_delta, col="red", xlim=c(0,1))  # first histogram
lines( RPdens_delta, col="blue", xlim=c(0,1))  # second
#AQvsnAQ
AQdens_delta<-density(AQavg_delta_raw,na.rm=TRUE)
nAQdens_delta<-density(nAQavg_delta_raw,na.rm=TRUE)
mAQdens_delta<-density(mAQavg_delta_raw,na.rm=TRUE)
APAQdens_delta<-density(APAQavg_delta_raw,na.rm=TRUE)
APnAQdens_delta<-density(APnAQavg_delta_raw,na.rm=TRUE)
##plot all Densioties for different groups
plot( nAQdens_delta, col="green",main="Density plots Delta", xlim=c(0,0.65))  # low AQ
text(0.5, y = 4, labels = c("nAQ"), col = "green")
lines( AQdens_delta, col="red", xlim=c(0,0.65))  # high AQ
text(0.5, y = 2.75, labels = c("AQ"), col = "red")
lines( mAQdens_delta, col="blue", xlim=c(0,0.65)) # medium AQ
text(0.5, y = 2.5, labels = c("mAQ"), col = "blue")
lines( APdens_delta, col="black", xlim=c(0,0.65)) #AP
text(0.5, y = 3.25, labels = c("AP"), col = "black")
lines( RPdens_delta, col="orange", xlim=c(0,0.65)) #RP
text(0.5, y = 3.75, labels = c("RP"), col = "orange")
lines( APAQdens_delta, col="purple", xlim=c(0,0.65)) #APhigh AQ
text(0.5, y = 3, labels = c("APAQ"), col = "purple")
lines( APnAQdens_delta, col="darkgreen", xlim=c(0,0.65)) #AP low AQ
text(0.5, y = 3.5, labels = c("APnAQ"), col = "darkgreen")

#text(locator(), labels = c("nAQ", "AQ","mAQ","AP","RP","APAQ","APnAQ"))
### distributions differ!
#stats
AP_delta_distribution<-as.vector(APavg_delta_raw)
RP_delta_distribution<-as.vector(RPavg_delta_raw)
AQ_delta_distribution<-as.vector(AQavg_delta_raw)
mAQ_delta_distribution<-as.vector(mAQavg_delta_raw)
nAQ_delta_distribution<-as.vector(nAQavg_delta_raw)
APAQ_delta_distribution<-as.vector(APAQavg_delta_raw)
APnAQ_delta_distribution<-as.vector(APnAQavg_delta_raw)
#compare distributions!
ks.test(AP_delta_distribution,RP_delta_distribution)#*
ks.test(AQ_delta_distribution,nAQ_delta_distribution) #*
ks.test(AQ_delta_distribution,mAQ_delta_distribution) #*
ks.test(AP_delta_distribution,AQ_delta_distribution)# ns
ks.test(AP_delta_distribution,nAQ_delta_distribution)# *
ks.test(AP_delta_distribution,mAQ_delta_distribution)# *
ks.test(RP_delta_distribution,AQ_delta_distribution)#  *
ks.test(RP_delta_distribution,nAQ_delta_distribution)# *
ks.test(RP_delta_distribution,mAQ_delta_distribution)# *
ks.test(APAQ_delta_distribution,APnAQ_delta_distribution)# *
ks.test(APAQ_delta_distribution,AQ_delta_distribution)# ns
ks.test(APAQ_delta_distribution,mAQ_delta_distribution)# *
ks.test(APAQ_delta_distribution,nAQ_delta_distribution)# *
ks.test(APAQ_delta_distribution,RP_delta_distribution)# *
ks.test(APnAQ_delta_distribution,nAQ_delta_distribution)# *
ks.test(APnAQ_delta_distribution,mAQ_delta_distribution)# *
ks.test(APnAQ_delta_distribution,AQ_delta_distribution)# *
ks.test(APnAQ_delta_distribution,RP_delta_distribution)# *
ks.test(APnAQ_delta_distribution,AP_delta_distribution)# *

plot(ecdf(x = AP_delta_distribution), main = "ECDF of AP and RP (delta)")
lines(ecdf(x = RP_delta_distribution), col = 2)

#####################
#BETA
#####################
#take only selected threshold
file_list_Matrices<-ls(pattern="rawMAT_[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_RS_EO.mat") 
Matrix_list <- vector("list", "length" = length(file_list_Matrices))
for (i in 1:length(file_list_Matrices)) { 
  Matrices<-get(file_list_Matrices[i])
  beta<-Matrices[,] #change threshold index +freqband
  beta<-as.matrix(beta)
  diag(beta)<-NA  #set diagonale to NA
  Matrix_list[[i]]<-beta
}
beta_Matrices_raw<-Matrix_list #change threshold name +freqband

AP_MAtrices_excl_beta_raw<-beta_Matrices_raw[which(is.AP==1&is.excl==0)]#AP
RP_MAtrices_excl_beta_raw<-beta_Matrices_raw[which(is.AP==0&is.excl==0)]#RP
AQ_MAtrices_excl_beta_raw<-beta_Matrices_raw[which(is.AQ==2&is.excl==0)] #high AQ
mAQ_MAtrices_excl_beta_raw<-beta_Matrices_raw[which(is.AQ==1&is.excl==0)]#medium AQ
nAQ_MAtrices_excl_beta_raw<-beta_Matrices_raw[which(is.AQ==0&is.excl==0)]#low AQ
APAQ_MAtrices_excl_beta_raw<-beta_Matrices_raw[which(is.AQ>=1&is.excl==0&is.AP==1)] #high AQ+AP
APnAQ_MAtrices_excl_beta_raw<-beta_Matrices_raw[which(is.AQ==1&is.excl==0&is.AP==1)] #high AQ+AP

APavg_beta_raw<-simplify2array(AP_MAtrices_excl_beta_raw)
RPavg_beta_raw<-simplify2array(RP_MAtrices_excl_beta_raw)
AQavg_beta_raw<-simplify2array(AQ_MAtrices_excl_beta_raw)
mAQavg_beta_raw<-simplify2array(mAQ_MAtrices_excl_beta_raw)
nAQavg_beta_raw<-simplify2array(nAQ_MAtrices_excl_beta_raw)
APAQavg_beta_raw<-simplify2array(APAQ_MAtrices_excl_beta_raw)
APnAQavg_beta_raw<-simplify2array(APnAQ_MAtrices_excl_beta_raw)

p1<-hist(APavg_beta_raw)
p2<-hist(RPavg_beta_raw)
##APvsRP
APdens_beta<-density(APavg_beta_raw,na.rm=TRUE)
RPdens_beta<-density(RPavg_beta_raw,na.rm=TRUE)
plot( APdens_beta, col="red", xlim=c(0,1))  # first histogram
lines( RPdens_beta, col="blue", xlim=c(0,1))  # second
#AQvsnAQ
AQdens_beta<-density(AQavg_beta_raw,na.rm=TRUE)
nAQdens_beta<-density(nAQavg_beta_raw,na.rm=TRUE)
mAQdens_beta<-density(mAQavg_beta_raw,na.rm=TRUE)
APAQdens_beta<-density(APAQavg_beta_raw,na.rm=TRUE)
APnAQdens_beta<-density(APnAQavg_beta_raw,na.rm=TRUE)

plot(RPdens_beta, col="orange", xlim=c(0,1),main="Density plots BETA") #RP
text(0.5, y = 4, labels = c("RP"), col = "orange")
lines( AQdens_beta, col="red", xlim=c(0,1))  # high AQ
text(0.5, y = 3.5, labels = c("AQ"), col = "red")
lines( mAQdens_beta, col="blue", xlim=c(0,1)) # medium AQ
text(0.5, y = 3.75, labels = c("mAQ"), col = "blue")
lines( APdens_beta, col="black", xlim=c(0,1)) #AP
text(0.5, y = 2.5, labels = c("AP"), col = "black")
lines( nAQdens_beta, col="green", xlim=c(0,1)) # low AQ
text(0.5, y = 3.25, labels = c("nAQ"), col = "green")
lines( APAQdens_beta, col="purple", xlim=c(0,1)) #APhigh AQ
text(0.5, y = 2.75, labels = c("APAQ"), col = "purple")
lines( APnAQdens_beta, col="darkgreen", xlim=c(0,1)) #AP low AQ
text(0.5, y =3, labels = c("APnAQ"), col = "darkgreen")
### distributions differ!
#stats
AP_beta_distribution<-as.vector(APavg_beta_raw)
RP_beta_distribution<-as.vector(RPavg_beta_raw)
AQ_beta_distribution<-as.vector(AQavg_beta_raw)
mAQ_beta_distribution<-as.vector(mAQavg_beta_raw)
nAQ_beta_distribution<-as.vector(nAQavg_beta_raw)
APAQ_beta_distribution<-as.vector(APAQavg_beta_raw)
APnAQ_beta_distribution<-as.vector(APnAQavg_beta_raw)

ks.test(AP_beta_distribution,RP_beta_distribution)#*
ks.test(AQ_beta_distribution,nAQ_beta_distribution) #*
ks.test(AQ_beta_distribution,mAQ_beta_distribution) #*
ks.test(AP_beta_distribution,AQ_beta_distribution)# *
ks.test(AP_beta_distribution,nAQ_beta_distribution)# *
ks.test(AP_beta_distribution,mAQ_beta_distribution)# *
ks.test(RP_beta_distribution,AQ_beta_distribution)#  *
ks.test(RP_beta_distribution,nAQ_beta_distribution)# *
ks.test(RP_beta_distribution,mAQ_beta_distribution)# *
ks.test(APAQ_beta_distribution,APnAQ_beta_distribution)# ns
ks.test(APAQ_beta_distribution,AQ_beta_distribution)# *
ks.test(APAQ_beta_distribution,AP_beta_distribution)# *
ks.test(APAQ_beta_distribution,mAQ_beta_distribution)# *
ks.test(APAQ_beta_distribution,nAQ_beta_distribution)# *
ks.test(APAQ_beta_distribution,RP_beta_distribution)# *
ks.test(APnAQ_beta_distribution,nAQ_beta_distribution)# *
ks.test(APnAQ_beta_distribution,mAQ_beta_distribution)# *
ks.test(APnAQ_beta_distribution,AQ_beta_distribution)# *
ks.test(APnAQ_beta_distribution,RP_beta_distribution)# *
ks.test(APnAQ_beta_distribution,AP_beta_distribution)# ns


plot(ecdf(x = AP_beta_distribution), main = "ECDF of AP and RP (beta)")
lines(ecdf(x = RP_beta_distribution), col = 2)

############################################################################################
############################################################################################

##############plot density distributions for each subject

######################################
#DELTA
######################################
file_list_Matrices<-ls(pattern="rawMAT_[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_RS_EC.mat") 
vec <- vector("numeric",length = length(file_list_Matrices)*378)
for (i in 1:length(file_list_Matrices)) { 
  Matrices<-get(file_list_Matrices[i])
  subject<-Matrices[,] 
  subject<-as.matrix(subject)
  subject<-as.vector(subject[upper.tri(subject, diag = FALSE)])  #set diagonale to NA
  vec[(1+(i-1)*378):(i*378)]<-subject
}
connect_values<-vec 

vec <- vector("numeric",length = length(file_list_Matrices)*378)
for (i in 1:length(file_list_Matrices)) {
subject<-rep(i,378)
vec[(1+(i-1)*378):(i*378)]<-subject
}
subj<-vec 




vec <- vector("numeric",length = length(file_list_Matrices)*378)
vec2 <- vector("numeric",length = length(file_list_Matrices)*378)
vec3 <- vector("numeric",length = length(file_list_Matrices)*378)
for (i in 1:length(file_list_Matrices)) {
  pitch<-rep(is.AP[i],378)
  excl<-rep(is.excl[i],378)
  AQdef<-rep(is.AQ[i],378)
  vec[(1+(i-1)*378):(i*378)]<-pitch
  vec2[(1+(i-1)*378):(i*378)]<-excl
  vec3[(1+(i-1)*378):(i*378)]<-AQdef
  
}
groupID<-vec
excl<-vec2
AQ<-vec3


distr_table<-cbind(subj,connect_values,groupID,excl,AQ)
distr_table<-data.frame(distr_table)
distr_table$subj <- as.factor(distr_table$subj)
distr_table$groupID <-as.factor(distr_table$groupID)
distr_table$excl<- as.factor(distr_table$excl)
distr_table$AQ<- as.factor(distr_table$AQ)
distr_table_excl<-rbind(distr_table[distr_table$excl==0,])

ggplot(distr_table_excl, aes(x = connect_values, y = subj)) + 
  #geom_density_ridges(scale = 5, alpha = 0.6) +
  scale_fill_continuous() +facet_wrap(~groupID)+
  theme_ridges() +
  #scale_y_continuous(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  #scale_x_discrete(expand = c(0, 0)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,quantile_lines=TRUE)

ggplot(distr_table_excl, aes(x = connect_values, y = subj)) + 
  #geom_density_ridges(scale = 5, alpha = 0.6) +
  scale_fill_continuous() +facet_wrap(~AQ)+
  theme_ridges() +
  #scale_y_continuous(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  #scale_x_discrete(expand = c(0, 0)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,quantile_lines=TRUE)


AP<-rbind(distr_table_excl[distr_table_excl$groupID==1,])

ggplot(AP, aes(x = connect_values, y = subj)) + 
  #geom_density_ridges(scale = 5, alpha = 0.6) +
  scale_fill_continuous() +facet_wrap(~AQ)+
  theme_ridges() +
  #scale_y_continuous(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  #scale_x_discrete(expand = c(0, 0)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,quantile_lines=TRUE)


RP<-rbind(distr_table_excl[distr_table_excl$groupID==0,])

ggplot(RP, aes(x = connect_values, y = subj)) + 
  #geom_density_ridges(scale = 5, alpha = 0.6) +
  scale_fill_continuous() +facet_wrap(~AQ)+
  theme_ridges() +
  #scale_y_continuous(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  #scale_x_discrete(expand = c(0, 0)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,quantile_lines=TRUE)



######################################
#BETA
######################################
file_list_Matrices<-ls(pattern="rawMAT_[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_RS_EO.mat") 
vec <- vector("numeric",length = length(file_list_Matrices)*378)
for (i in 1:length(file_list_Matrices)) { 
  Matrices<-get(file_list_Matrices[i])
  subject<-Matrices[,] 
  subject<-as.matrix(subject)
  subject<-as.vector(subject[upper.tri(subject, diag = FALSE)])  #set diagonale to NA
  vec[(1+(i-1)*378):(i*378)]<-subject
}
connect_values<-vec 

vec <- vector("numeric",length = length(file_list_Matrices)*378)
for (i in 1:length(file_list_Matrices)) {
  subject<-rep(i,378)
  vec[(1+(i-1)*378):(i*378)]<-subject
}
subj<-vec 




vec <- vector("numeric",length = length(file_list_Matrices)*378)
vec2 <- vector("numeric",length = length(file_list_Matrices)*378)
vec3 <- vector("numeric",length = length(file_list_Matrices)*378)
for (i in 1:length(file_list_Matrices)) {
  pitch<-rep(is.AP[i],378)
  excl<-rep(is.excl[i],378)
  AQdef<-rep(is.AQ[i],378)
  vec[(1+(i-1)*378):(i*378)]<-pitch
  vec2[(1+(i-1)*378):(i*378)]<-excl
  vec3[(1+(i-1)*378):(i*378)]<-AQdef
  
}
groupID<-vec
excl<-vec2
AQ<-vec3


distr_table<-cbind(subj,connect_values,groupID,excl,AQ)
distr_table<-data.frame(distr_table)
distr_table$subj <- as.factor(distr_table$subj)
distr_table$groupID <-as.factor(distr_table$groupID)
distr_table$excl<- as.factor(distr_table$excl)
distr_table$AQ<- as.factor(distr_table$AQ)
distr_table_excl<-rbind(distr_table[distr_table$excl==0,])

ggplot(distr_table_excl, aes(x = connect_values, y = subj)) + 
  #geom_density_ridges(scale = 5, alpha = 0.6) +
  scale_fill_continuous() +facet_wrap(~groupID)+
  theme_ridges() +
  #scale_y_continuous(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  #scale_x_discrete(expand = c(0, 0)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,quantile_lines=TRUE)

ggplot(distr_table_excl, aes(x = connect_values, y = subj)) + 
  #geom_density_ridges(scale = 5, alpha = 0.6) +
  scale_fill_continuous() +facet_wrap(~AQ)+
  theme_ridges() +
  #scale_y_continuous(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  #scale_x_discrete(expand = c(0, 0)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,quantile_lines=TRUE)


AP<-rbind(distr_table_excl[distr_table_excl$groupID==1,])

ggplot(AP, aes(x = connect_values, y = subj)) + 
  #geom_density_ridges(scale = 5, alpha = 0.6) +
  scale_fill_continuous() +facet_wrap(~AQ)+
  theme_ridges() +
  #scale_y_continuous(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  #scale_x_discrete(expand = c(0, 0)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,quantile_lines=TRUE)


RP<-rbind(distr_table_excl[distr_table_excl$groupID==0,])

ggplot(RP, aes(x = connect_values, y = subj)) + 
  #geom_density_ridges(scale = 5, alpha = 0.6) +
  scale_fill_continuous() +facet_wrap(~AQ)+
  theme_ridges() +
  #scale_y_continuous(expand = c(0.01, 0)) +   # will generally have to set the `expand` option
  #scale_x_discrete(expand = c(0, 0)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,quantile_lines=TRUE)
