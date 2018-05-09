Clustertable<-cbind(betaanddelta$starting_age,betaanddelta$AP_sums,betaanddelta$AQ_Score,betaanddelta$MAD,betaanddelta$SDfoM,betaanddelta$`L_ T10`,
                    (betaanddelta$vis_SACS_Lcon-betaanddelta$vis_SACS_Linc),Crand_beta)
colnames(Clustertable)<-c("starting_age","AP_sums","AQ","MAD","SDfoM","L_delta","cogstyle","C_beta")

Clustertable <- na.omit(Clustertable) # listwise deletion of missing
Clustertable <- scale(Clustertable) # standardize variables 



# Ward Hierarchical Clustering
d <- dist(Clustertable, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram
groups <- cutree(fit, k=4) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=4, border="red")
result<-cbind(groups,Clustertable)

##combine raw data with classification

Clustertable<-cbind(betaanddelta$Group..0.RP..1.AP..2.ASD..3.NM.,
                    betaanddelta$starting_age,betaanddelta$AP_sums,betaanddelta$AQ_Score,betaanddelta$MAD,betaanddelta$SDfoM,betaanddelta$`L_ T10`,
                    (betaanddelta$vis_SACS_Lcon-betaanddelta$vis_SACS_Linc),Crand_beta)
colnames(Clustertable)<-c("group","starting_age","AP_sums","AQ","MAD","SDfoM","L_delta","cogstyle","C_beta") ##added grouping variables

Clustertable <- na.omit(Clustertable)
result<-cbind(groups,Clustertable)
AP<-subset(result, result[,2]==1)
RP<-subset(result, result[,2]==0)
hist(AP[,1])
hist(RP[,1])
##groups 1,2 --> RP, groups 3,4 --> AP


###################################
#Plot differences of Clusters
###################################

library("psych", lib.loc="~/R/win-library/3.3")
summary_ward<-describe.by(result, result[,1])
boxplot(AQ~groups,data=result, main="AQ",
        xlab="Ward-Groups", ylab="AQ_Score",cex.lab=1.5,cex.main=1.5, col=c("orange","orange","lightblue","lightblue"))
legend("topleft", inset=.02,
       c("RP","AP"), fill=c("orange","lightblue"), horiz=TRUE, cex=1.5)
boxplot(starting_age~groups,data=result, main="starting_age",
        xlab="Ward-Groups", ylab="years",cex.lab=1.5,cex.main=1.5, col=c("orange","orange","lightblue","lightblue"))
legend("topleft", inset=.02,
       c("RP","AP"), fill=c("orange","lightblue"), horiz=TRUE, cex=1.5)
boxplot(MAD~groups,data=result, main="Pitch adjustment (MAD)",
        xlab="Ward-Groups", ylab="Mean absolute derivation",cex.lab=1.5,cex.main=1.5, col=c("orange","orange","lightblue","lightblue"))
legend("topright", inset=.02,
       c("RP","AP"), fill=c("orange","lightblue"), horiz=TRUE, cex=1.5)
boxplot(SDfoM~groups,data=result, main="Pitch adjustment (SDfoM)",
        xlab="Ward-Groups", ylab="SD from own mean",cex.lab=1.5,cex.main=1.5, col=c("orange","orange","lightblue","lightblue"))
legend("topleft", inset=.02,
       c("RP","AP"), fill=c("orange","lightblue"), horiz=TRUE, cex=1.5)
boxplot(L_delta~groups,data=result, main="Path length delta",
        xlab="Ward-Groups", ylab="Path length delta",cex.lab=1.5,cex.main=1.5, col=c("orange","orange","lightblue","lightblue"))
legend("topleft", inset=.02,
       c("RP","AP"), fill=c("orange","lightblue"), horiz=TRUE, cex=1.5)
boxplot(cogstyle~groups,data=result, main="Cognitive Style",
        xlab="Ward-Groups", ylab="Lcon-Linc",cex.lab=1.5,cex.main=1.5, col=c("orange","orange","lightblue","lightblue"))
legend("topleft", inset=.02,
       c("RP","AP"), fill=c("orange","lightblue"), horiz=TRUE, cex=1.5)
boxplot(C_beta~groups,data=result, main="Clustering beta",
        xlab="Ward-Groups", ylab="Clustering beta",cex.lab=1.5,cex.main=1.5, col=c("orange","orange","lightblue","lightblue"))
legend("topleft", inset=.02,
       c("RP","AP"), fill=c("orange","lightblue"), horiz=TRUE, cex=1.5)
