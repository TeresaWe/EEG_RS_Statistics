####Clusteranalyse
Clustertable<-cbind(betaanddelta$Group..0.RP..1.AP..2.ASD..3.NM.,
                    betaanddelta$early,betaanddelta$AQ_Score,betaanddelta$MAD,betaanddelta$`L_ T10`,
                    (betaanddelta$vis_SACS_Lcon-betaanddelta$vis_SACS_Linc),Crand_beta)
colnames(Clustertable)<-c("group","early","AQ","MAD","L_delta","cogstyle","C_beta")

Clustertable <- na.omit(Clustertable) # listwise deletion of missing
Clustertable <- scale(Clustertable) # standardize variables 

# Determine number of clusters
wss <- (nrow(Clustertable)-1)*sum(apply(Clustertable,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(Clustertable,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

# K-Means Cluster Analysis
fit2 <- kmeans(Clustertable, 4) # 5 cluster solution
# get cluster means
aggregate(Clustertable,by=list(fit2$cluster),FUN=mean)
# append cluster assignment
Clustertable <- data.frame(Clustertable, fit2$cluster) 
summary(fit2)
# Cluster Plot against 1st 2 principal components

library(fpc)
cluster.stats(d, fit$cluster, fit2$cluster)


# vary parameters for most readable graph
library(cluster)
clusplot(Clustertable[,3:4], fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(Clustertable[,3:4], fit$cluster) 


# Model Based Clustering
library(mclust)
fit <- Mclust(Clustertable)
plot(fit) # plot results
summary(fit) # display the best model 

#########################




####Clusteranalyse II (fewer variables)
Clustertable<-cbind(betaanddelta$Group..0.RP..1.AP..2.ASD..3.NM.,betaanddelta$early,betaanddelta$AQ_Score,
                    (betaanddelta$vis_SACS_Lcon-betaanddelta$vis_SACS_Linc))
colnames(Clustertable)<-c("group","early","AQ","cogstyle")

Clustertable <- na.omit(Clustertable) # listwise deletion of missing
Clustertable <- scale(Clustertable) # standardize variables 

# Determine number of clusters
wss <- (nrow(Clustertable)-1)*sum(apply(Clustertable,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(Clustertable,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

# K-Means Cluster Analysis
fit <- kmeans(Clustertable, 4) # 5 cluster solution
# get cluster means
aggregate(Clustertable,by=list(fit$cluster),FUN=mean)
# append cluster assignment
Clustertable <- data.frame(Clustertable, fit$cluster) 


#######################################################
####Clusteranalyse III

Clustertable<-cbind(betaanddelta$Group..0.RP..1.AP..2.ASD..3.NM.,
                    betaanddelta$early,betaanddelta$AQ_Score,betaanddelta$`L_ T10`,
                    (betaanddelta$vis_SACS_Lcon-betaanddelta$vis_SACS_Linc),Crand_beta)
colnames(Clustertable)<-c("group","early","AQ","L_delta","cogstyle","Cbeta")
Clustertable <- na.omit(Clustertable) # listwise deletion of missing
Clustertable <- scale(Clustertable) # standardize variables 

# Determine number of clusters
wss <- (nrow(Clustertable)-1)*sum(apply(Clustertable,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(Clustertable,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

# K-Means Cluster Analysis
fit <- kmeans(Clustertable, 6) # 5 cluster solution
# get cluster means
aggregate(Clustertable,by=list(fit$cluster),FUN=mean)
# append cluster assignment
Clustertable <- data.frame(Clustertable, fit$cluster) 