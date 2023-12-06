#' Unsupervised PCA of PFF cohort 
#'
#' @name pff.pca.unsupervised
#' @author Yong Huang
      

library("FactoMineR")

df<-read.csv("npx.pff.ipf.ctd.csv")
rownames(df)<-df$SampleID

Diagnosis<- df$Diagnosis
Sex<-df$Sex
Site<-df$Site

dd<-df[, -c(1:4)]


### standard deviation of proteins 
sd<- apply(dd, 2, sd)
names(sd)<- colnames(dd)

## top 500 proteins
sd<-sort(sd, decreasing = T)
var<- sd[c(1:500)]

xx <- dd[, names(var)]

dat<- cbind(Diagnosis, xx)

# pca variables
res.pca <- PCA(dat,  quali.sup=1)

# pca subjects
plot(res.pca, choix="ind", habillage=1, legend = list(bty = "y", x = "right"), col.hab=c("green","blue"), col.quali = c("green", "blue"), label="none", cex=0.6, cex.axis=1, cex.lab=1)

dim(res.pca$ind$coord)
# [1] 1100    5

pc<- res.pca$ind$coord

## t-test of PC1 and PC2
x1<- pc[, 1]
x2<- pc[, 2]
y<-dat$Diagnosis

tt1<- t.test(x1 ~ y)
#p1=0.003932993

tt2<- t.test(x2 ~ y)
#p2 = 0.2886373


######### sex 
dat<- cbind(Sex, xx)
res.pca <- PCA(dat,  quali.sup=1)

dat<- cbind(Sex, xx)
res.pca <- PCA(dat,  quali.sup=1)
pc<- res.pca$ind$coord


## t-test of PC1 and PC2
x1<- pc[, 1]
x2<- pc[, 2]
y<-dat$Sex

tt1<- t.test(x1 ~ y)
# p1=0.2284

tt2<- t.test(x2 ~ y)
# p2 = 0.1574