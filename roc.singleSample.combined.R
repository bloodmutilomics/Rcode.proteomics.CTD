#' ROC curve of class probabilities of RECEITAL /UC-Davis derived from 4 machine learning methods
#'
#' @name roc.singleSample.combined
#' @author Yong Huang
      

library(pROC)
###############################
df1<-read.csv("pred.link.lasso.csv")
df2<-read.csv("pred.decision.svm.csv")
df3<-read.csv("pred.prob.rf.csv")
df4<- read.csv("qclassifier.single.prob2.csv")

rownames(df1)<-df1[, 1]
rownames(df2)<-df2[, 1]
rownames(df3)<-df3[, 1]
rownames(df4)<-df4[, 1]

############# roc.lasso ###############
diagnosis1 <- df1$Diagnosis.test
score1 <- df1$mean.plink

roc.lasso<- roc(diagnosis1, score1 , plot=T)
auc<-auc(roc.lasso)
# 0.96

xx<- roc.lasso$sensitivities + roc.lasso$specificities
idx<- which(xx== max(xx) )
se<- roc.lasso$sensitivities[idx]
sp<- roc.lasso$specificities[idx]

#se [1] 0.87

# sp [1] 0.92

roc.res<- c(auc, se, sp)
names(roc.res) <- c("auc", "se", "sp")
yy<-t(roc.res)
rownames(yy)[1]<-"lasso"
write.csv( yy, file="roc.lasso.csv")


### plot
tiff(file="roc.lasso.tiff")
par(new=T)
plot(roc.lasso$specificities,roc.lasso$sensitivities,type="n",xlim=c(1,0), xlab="Specificity", ylab="Sensitivity", cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5, main="RECITAL/UC-Davis cohort: CTD-ILD/IPF=56/192")
lines(roc.lasso$specificities,roc.lasso$sensitivities,col="green")
abline(1,-1,col="gray")
legend("bottomright",  "LASSO (AUC=0.889; SE:SP=0.833:0.786)", col="green", lty=1, cex=1.25 )

dev.off()  


############# roc.svm ###############
diagnosis2<- df2$Diagnosis.test
score2 <-df2$mean.pdecision

roc.svm<- roc(diagnosis2, score2 , plot=T)
auc<-auc(roc.svm)
# 0.94

xx<- roc.svm$sensitivities + roc.svm$specificities
idx<- which(xx== max(xx) )
se<- roc.svm$sensitivities[idx]
sp<- roc.svm$specificities[idx]

# se 0.81
# sp 0.94

roc.res<- c(auc, se, sp)
names(roc.res) <- c("auc", "se", "sp")
yy<-t(roc.res)
rownames(yy)[1]<-"svm"
write.csv( yy, file="roc.svm.csv")


#### plot rc.svm
tiff(file="roc.svm.tiff")
par(new=T)
plot(roc.svm$specificities,roc.svm$sensitivities,type="n",xlim=c(1,0),xlab="Specificity", ylab="Sensitivity", cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5, main="RECITAL/UC-Davis cohort: CTD-ILD/IPF=56/192")
lines(roc.svm$specificities,roc.svm$sensitivities,col="red")
abline(1,-1,col="gray")
legend("bottomright",  "SVM (AUC=0.874; SE:SP=0.833:0.768)", col="red", lty=1, cex=1.25)

dev.off()  


############# roc.rf ###############
diagnosis3<- df3$Diagnosis.test
score3 <-df3$mean.prob

roc.rf<- roc(diagnosis3, score3 , plot=T)
auc<-auc(roc.rf)
# 0.94

xx<- roc.rf$sensitivities + roc.rf$specificities
idx<- which(xx== max(xx) )
se<- roc.rf$sensitivities[idx]
sp<- roc.rf$specificities[idx]

# se 0.85
# sp 0.90

roc.res<- c(auc, se, sp)
names(roc.res) <- c("auc", "se", "sp")
yy<-t(roc.res)
rownames(yy)[1]<-"svm"
write.csv( yy, file="roc.rf.csv")


#### plot roc.rf
tiff(file="roc.rf.tiff")
par(new=T)
plot(roc.rf$specificities,roc.rf$sensitivities,type="n",xlim=c(1,0),xlab="Specificity", ylab="Sensitivity", cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5, main="RECITAL/UC-Davis cohort: CTD-ILD/IPF=56/192")
lines(roc.rf$specificities,roc.rf$sensitivities,col="blue")
abline(1,-1,col="gray")
legend("bottomright",  "RF (AUC=0.869; SE:SP=0.818:0.714)", col="blue", lty=1, cex=1.25)

dev.off()  



########### imbalanced RF ################
diagnosis4 <- df4$Diagnosis.test
score4 <-df4$IPF.prob

roc.rfsr<- roc(diagnosis4, score4 , plot=T)
auc<-auc(roc.rfsr)
# 0.95

xx<- roc.rfsr$sensitivities + roc.rfsr$specificities
idx<- which(xx== max(xx) )
se<- roc.rfsr$sensitivities[idx]
sp<- roc.rfsr$specificities[idx]

# se 0.86
# sp 0.90

roc.res<- c(auc, se, sp)
names(roc.res) <- c("auc", "se", "sp")
yy<-t(roc.res)
rownames(yy)[1]<-"svm"
write.csv( yy, file="roc.rfsr.csv")



############# plot combined0 ##########
tiff(file="roc.combined0.tiff")
#1
plot(roc.svm$specificities,roc.svm$sensitivities,type="n",xlim=c(1,0), xlab="Specificity",ylab="Sensitivity", cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5,  main="RECITAL/UC-Davis cohort: CTD-ILD/IPF=77/174")
lines(roc.svm$specificities,roc.svm$sensitivities,col="red")

par(new=T)
#2
plot(roc.lasso$specificities,roc.lasso$sensitivities,type="n",xlim=c(1,0),xlab=NA,ylab=NA,axes=T, cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5)
lines(roc.lasso$specificities,roc.lasso$sensitivities,col="green")

par(new=T)
#3
plot(roc.rf$specificities, roc.rf$sensitivities,type="n",xlim=c(1,0),xlab=NA,ylab=NA,axes=T, cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5)
lines(roc.rf$specificities, roc.rf$sensitivities,col="blue")

par(new=T)
#4
plot(roc.rfsr$specificities,roc.rfsr$sensitivities,type="n",xlim=c(1,0),xlab=NA,ylab=NA,axes=T, cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5)
lines(roc.rfsr$specificities,roc.rfsr$sensitivities,col="orange")

# label
abline(1,-1,col="gray")

legend("bottomright", c("SVM (AUC=0.94; SE:SP=0.81:0.94)", "LASSO (AUC=0.96; SE:SP=0.87:0.92)", "RF  (AUC=0.94; SE:SP=0.85:0.90)", "Imblanced RF (AUC=0.95; SE:SP=0.86:0.90)" ),  col=c("red","green", "blue", "orange"), lty=1, cex=1.19)  

dev.off()  
