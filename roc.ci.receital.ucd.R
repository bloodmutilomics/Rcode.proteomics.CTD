#' ROC curve with confidence interval: RECEITAL/UC-Davis cohort
#'
#' @name roc.ci.receital.ucd
#' @author Yong Huang

library(pROC)

df1<-read.csv("pred.link2.seq005.csv")
df2<-read.csv("pred.decision.csv")
df3<-read.csv("pred.prob2.csv")
df4<- read.csv("qclassifier.single.prob2.csv")

rownames(df1)<-df1[, 1]
rownames(df2)<-df2[, 1]
rownames(df3)<-df3[, 1]
rownames(df4)<-df4[, 1]


############# 1. roc.lasso ###############
diagnosis1 <- df1$Diagnosis.test
score1 <- df1$mean.plink

roc.lasso<- roc(diagnosis1, score1 , plot=T)
auc<-auc(roc.lasso)
# 0.96

xx<- roc.lasso$sensitivities + roc.lasso$specificities
idx<- which(xx== max(xx) )
se<- roc.lasso$sensitivities[idx]
sp<- roc.lasso$specificities[idx]

# se 0.87
# sp [1] 0.92

roc.res<- c(auc, se, sp)
names(roc.res) <- c("auc", "se", "sp")
yy<-t(roc.res)
rownames(yy)[1]<-"lasso"
write.csv( yy, file="roc.lasso.csv")

### add ci ###

### plot
tiff(file="roc.lasso.tiff")
par(new=T)
plot(roc.lasso$specificities,roc.lasso$sensitivities,type="n",xlim=c(1,0), xlab="Specificity", ylab="Sensitivity", cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5, main="RECITAL/UC-Davis cohort: CTD-ILD/IPF=56/192")
lines(roc.lasso$specificities,roc.lasso$sensitivities,col="green")
abline(1,-1,col="gray")
legend("bottomright",  "LASSO (AUC=0.889; SE:SP=0.833:0.786)", col="green", lty=1, cex=1.25 )

dev.off()  



############# 2. roc.svm ###############
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


############# 3. roc.rf ###############
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



########### 4. imbalanced RF ################
diagnosis4 <- df4$Diagnosis.test
score4 <-df4$IPF.prob

roc.rfsr<- roc(diagnosis4, score4 , plot=T)
auc<-auc(roc.rfsr)
# 0.95

xx<- roc.rfsr$sensitivities + roc.rfsr$specificities
idx<- which(xx== max(xx) )
se<- roc.rfsr$sensitivities[idx]
sp<- roc.rfsr$specificities[idx]

se 0.86
sp 0.90

roc.res<- c(auc, se, sp)
names(roc.res) <- c("auc", "se", "sp")
yy<-t(roc.res)
rownames(yy)[1]<-"svm"
write.csv( yy, file="roc.rfsr.csv")


########### plot roc.lasso.ci
tiff(file="roc.lasso.ci.tiff")
par(new=T)

ci.sp.obj <- ci.sp(roc.lasso, specificity=seq(0, 1, .01), boot.n=100)
plot(roc.lasso$specificities,roc.lasso$sensitivities,type="n",xlim=c(1,0), xlab="Specificity",ylab="Sensitivity", cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5,  main="RECITAL/UC-Davis cohort: CTD-ILD/IPF=56/192")

plot(ci.sp.obj, type="shape", col="light grey")

lines(roc.lasso$specificities,roc.lasso$sensitivities,col="green")
abline(1,-1,col="gray")
legend("bottomright",  "LASSO (AUC=0.96; SE:SP=0.87:0.92)", col="green", lty=1, cex=1.2 )

dev.off()  


### plot roc.svm ci 
tiff(file="roc.svm.ci.tiff")
par(new=T)
ci.sp.obj <- ci.sp(roc.svm, specificity=seq(0, 1, .01), boot.n=100)
plot(roc.svm$specificities,roc.svm$sensitivities,type="n",xlim=c(1,0), xlab="Specificity",ylab="Sensitivity", cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5,  main="RECITAL/UC-Davis cohort: CTD-ILD/IPF=56/192")

plot(ci.sp.obj, type="shape", col="light grey")
lines(roc.svm$specificities,roc.svm$sensitivities,col="red")
abline(1,-1,col="gray")
legend("bottomright",  "SVM (AUC=0.94; SE:SP=0.81:0.94)", col="red", lty=1, cex=1.2 )

dev.off()  

### 3. roc.rf ci 
tiff(file="roc.rf.ci.tiff")
par(new=T)
ci.sp.obj <- ci.sp(roc.rf, specificity=seq(0, 1, .01), boot.n=100)
plot(roc.rf$specificities,roc.rf$sensitivities,type="n",xlim=c(1,0), xlab="Specificity",ylab="Sensitivity", cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5,  main="RECITAL/UC-Davis cohort: CTD-ILD/IPF=56/192")

plot(ci.sp.obj, type="shape", col="light grey")
lines(roc.rf$specificities,roc.rf$sensitivities,col="blue")
abline(1,-1,col="gray")
legend("bottomright",  "RF  (AUC=0.94; SE:SP=0.85:0.90)", col="blue", lty=1, cex=1.2 )

dev.off()  


### 4. roc.rfsr ci 
tiff(file="roc.rfsr.ci.tiff")
par(new=T)
ci.sp.obj <- ci.sp(roc.rf, specificity=seq(0, 1, .01), boot.n=100)
plot(roc.rfsr$specificities,roc.rfsr$sensitivities,type="n",xlim=c(1,0), xlab="Specificity",ylab="Sensitivity", cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5,  main="RECITAL/UC-Davis cohort: CTD-ILD/IPF=56/192")

plot(ci.sp.obj, type="shape", col="light grey")
lines(roc.rf$specificities,roc.rf$sensitivities,col="orange")
abline(1,-1,col="gray")
legend("bottomright",  "Imblanced RF (AUC=0.95; SE:SP=0.86:0.90)", col="orange", lty=1, cex=1.2 )

dev.off()  









