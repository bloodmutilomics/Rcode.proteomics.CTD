#' ROC curve of GAP scores in PFF and UVA/UChicago
#'
#' @name roc.gap
#' @author Yong Huang
      
library(pROC)

################## Read clincial data of PFFR and UVA/UChicago 

## Read PFF Gap
df1<-read.csv("PFF.npx1100.gap.clean2.csv")
rownames(df1)<-df1[, 1]

## Read UVA/UChicago GAP
df2<-read.csv("clinic.nonPFF.ipf.ctd.gap0.csv")
rownames(df2)<-df2[, 1]

diagnosis1<- df1$Diagnosis
diagnosis2<- df2$Dx

gap.score1 <- df1$gap.score
gap.score2<- df2$gap.score
gap.stage1<- df1$gap.stage
gap.stage2<- df2$gap.stage
 
## ROC curve and AUC: PFFR
roc1<- roc(diagnosis1, gap.score1 , plot=T)
auc<- auc(roc1)

xx<- roc1$sensitivities + roc1$specificities
idx<- which(xx== max(xx) )
se<- roc1$sensitivities[idx]
sp<- roc1$specificities[idx]

roc.res<- c(auc, se, sp)
names(roc.res) <- c("auc", "se", "sp")
yy<-t(roc.res)
rownames(yy)[1]<-"PFF"

## ROC curve and AUC: UVA/UChicago
roc2<- roc(diagnosis2, gap.score2 , plot=T)
auc<- auc(roc2)

xx<- roc2$sensitivities + roc2$specificities
idx<- which(xx== max(xx) )
se<- roc2$sensitivities[idx]
sp<- roc2$specificities[idx]

roc.res<- c(auc, se, sp)
names(roc.res) <- c("auc", "se", "sp")
zz<-t(roc.res)
rownames(zz)[1]<-"UVA.UChicago"

write.csv( rbind(yy, zz), file="roc.gap.csv")


############### plot ROC curve: PFF
#### plot PFF
tiff(file="roc.pff.tiff")
par(new=T)
plot(roc1$specificities,roc1$sensitivities,type="n",xlim=c(1,0),xlab="Specificity", ylab="Sensitivity", cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5, main="ROC anaylsys of Gap scores")
lines(roc1$specificities,roc1$sensitivities,col="brown")
abline(1,-1,col="gray")
legend("bottomright",  "PFF (AUC=0.708; SE:SP=0.786:0.54)", col="brown", lty=1 )

dev.off()  



######## plot ROC curve: UVA/UChicago
tiff(file="roc.uva.uc.tiff")
par(new=T)
plot(roc1$specificities,roc1$sensitivities,type="n",xlim=c(1,0),xlab="Specificity", ylab="Sensitivity", cex.main=1.5, cex.sub=1.5, cex.lab=1.5, cex.axis=1.5, main="ROC anaylsys of Gap scores")
lines(roc1$specificities,roc1$sensitivities,col="blue")
abline(1,-1,col="gray")
legend("bottomright",  "PFF (AUC=0.681; SE:SP=0.648:0.604)", col="blue", lty=1 )

dev.off()  


############# plot PFF and UVA/UChciago combined 
tiff(file="roc.combined.tiff")
#1
plot(roc1$specificities,roc1$sensitivities,type="n",xlim=c(1,0),xlab="Specificity", ylab="Sensitivity", cex.main=1.3, cex.sub=1.3, cex.lab=1.3, cex.axis=1.3, main="ROC anaylsys of Gap scores")
lines(roc1$specificities,roc1$sensitivities,col="orange")

par(new=T)
#2
plot(roc2$specificities,roc2$sensitivities,type="n",xlim=c(1,0),xlab="Specificity", ylab="Sensitivity", cex.main=1.3, cex.sub=1.3, cex.lab=1.3, cex.axis=1.3, main="ROC anaylsys of Gap scores")
lines(roc2$specificities,roc2$sensitivities,col="blue")

# label
abline(1,-1,col="gray")

legend("bottomright", c("PFF cohort      (AUC=0.71; SE:SP=0.79:0.54)", "UVA/UChicago (AUC=0.68; SE:SP=0.65:0.604)" ),  col=c("orange","blue"), lty=1, cex=1.1)  
dev.off()  



######################### plot ci ###############
# Plotting a shape. We need more
### Example ###
ci.sp.obj <- ci.sp(rocobj, specificity=seq(0, 1, .01), boot.n=100)
plot(rocobj) # restart a new plot
plot(ci.sp.obj, type="shape", col="light grey")
#######################


########### plot ROC confidence interval: PFF
tiff(file="roc.pff.ci.tiff")
par(new=T)

ci.sp.obj <- ci.sp(roc1, specificity=seq(0, 1, .01), boot.n=100)
plot(roc1$specificities,roc1$sensitivities,type="n",xlim=c(1,0), xlab="Specificity",ylab="Sensitivity", cex.main=1.3, cex.sub=1.3, cex.lab=1.3, cex.axis=1.3,  main="ROC anaylsys of PFF Gap scores")

plot(ci.sp.obj, type="shape", col="light grey")

lines(roc1o$specificities,roc1$sensitivities,col="green")
abline(1,-1,col="gray")
legend("bottomright",  "PFF (AUC=0.71; SE:SP=0.79:0.54)", col="orange", lty=1, cex=1.15 )

dev.off()  


########### plot ROC confidence interval: UVA/UChicago
tiff(file="roc.uva.ci.tiff")
par(new=T)
ci.sp.obj <- ci.sp(roc2, specificity=seq(0, 1, .01), boot.n=100)
plot(roc2$specificities,roc2$sensitivities,type="n",xlim=c(1,0), xlab="Specificity",ylab="Sensitivity", cex.main=1.3, cex.sub=1.3, cex.lab=1.3, cex.axis=1.3,  main="ROC anaylsys of PFF Gap scores")

plot(ci.sp.obj, type="shape", col="light grey")
lines(roc2$specificities,roc2$sensitivities,col="blue")
abline(1,-1,col="gray")
legend("bottomright",  "UVA/UChicago (AUC=0.68; SE:SP=0.65:0.604)", col="blue", lty=1, cex=1.15 )

dev.off()  





