#' t-test of PFF cohort between CTD and IPF: sex matched 100x subsampling 
#'
#' @name ttest.pff.100resampling
#' @author Yong Huang

library("dplyr")
library("ggpubr")
library(matrixTests)


library("dplyr")
library("ggpubr")
library(matrixTests)


############ Read PFF Olink NXP data 
df<-read.csv("pff.nxp.csv")
rownames(df)<-df[, 1]

prt<- colnames(df)[-c(1:10)]

table(df$Diagnosis)
#CTD IPF 
#219 881 


### sub-sample equal CTD and IPF
dd.ctd<- subset(df, Diagnosis=="CTD")
dd.ipf<- subset(df, Diagnosis=="IPF")

table(dd.ctd$Sex)
#female   male 
#  141    78 


table(dd.ipf$Sex)
#female   male 
#   214    667 

###
dd.ipf.f<- subset(dd.ipf, Sex=="female")
dd.ipf.m<- subset(dd.ipf, Sex=="male")

dd.ctd.f<- subset(dd.ctd, Sex=="female")
dd.ctd.m<- subset(dd.ctd, Sex=="male")

###################### 100x sampling t-test #############
n=100
logFC <- data.frame( matrix(nrow=length(prt), ncol=n) ) 
padj <- data.frame( matrix(nrow=length(prt), ncol=n) ) 

pval <- data.frame( matrix(nrow=length(prt), ncol=n) ) 
mean.ctd <-  data.frame( matrix(nrow=length(prt), ncol=n) ) 
mean.ipf <- data.frame( matrix(nrow=length(prt), ncol=n) ) 
ci.lo <- data.frame( matrix(nrow=length(prt), ncol=n) ) 
ci.hi <- data.frame( matrix(nrow=length(prt), ncol=n) ) 

for (i in 1:n) {
  dd.ipf.f.resampling <- sample_n(dd.ipf.f, 60)
  dd.ipf.m.resampling <- sample_n(dd.ipf.m, 60)

  dd.ctd.f.resampling <- sample_n(dd.ctd.f, 60)
  dd.ctd.m.resampling <- sample_n(dd.ctd.m, 60)

  dd<-rbind(dd.ctd.f.resampling, dd.ctd.m.resampling, dd.ipf.f.resampling, dd.ipf.m.resampling)
  
  #dd<-rbind(dd.ctd.f.resampling, dd.ipf.f.resampling)
 
  dd1<- subset(dd, Diagnosis=="IPF")
  dd2<- subset(dd, Diagnosis=="CTD")
  
  x<- dd1[, -c(1:10)]
  y<- dd2[, -c(1:10)]

  tt<- col_t_equalvar(x, y, alternative = "two.sided", mu = 0, conf.level = 0.95)

  pval[, i] <- tt$pvalue
  mean.ctd[, i]<- tt$mean.x
  mean.ipf[, i]<- tt$mean.y
  ci.lo[, i]<-tt$conf.low
  ci.hi[, i]<-tt$conf.high

  padj[, i] <- p.adjust(pval[, i], "BH")
  logFC[, i] <- tt$mean.x -tt$mean.y

} #End of j loop


logFC.mean<- rowSums(logFC) /ncol(logFC)
padj.mean<- rowSums(padj) /ncol(padj)

pval.mean <- rowSums(pval) /ncol(pval)
ctd.mean<- rowSums(mean.ctd) /ncol(mean.ctd)
ipf.mean<- rowSums(mean.ipf) /ncol(mean.ipf)

res<-cbind(ctd.mean, ipf.mean, logFC.mean, pval.mean,  padj.mean)
rownames(res) <- colnames(x)

write.csv( res, file="ttest.pff.100resampling.csv")
      