#' Unsupervised PCA of RECEITAL and UC-Davis NPX 
#'
#' @name pca.recital.ucd.unsupervised
#' @author Yong Huang
      

library("FactoMineR")

df<-read.csv("npx.recital.ucd.csv")
rownames(df)<-df[, 1]

Diagnosis<- df$Diagnosis
Sex<-df$Sex
Site<-df$Site

dd<-df[, -c(1:12)]


### PCA

## calculate standard deviation of proteins
sd<- apply(dd, 2, sd)
names(sd)<- colnames(dd)

$ top 500 proteins
sd<-sort(sd, decreasing = T)
var<- sd[c(1:500)]

xx <- dd[, names(var)]

dat<- cbind(Site, xx)


res.pca <- PCA(dat,  quali.sup=1)
plot(res.pca, choix="ind", habillage=1, legend = list(bty = "y", x = "right"), col.hab=c("green","blue"), label="none", col.quali = c("green", "blue"), cex=0.6)


t.test(res.pca$ind$coord[, 1] ~ Site)
#p-value < 2.2e-16

t.test(res.pca$ind$coord[, 2] ~ Site)
#p-value = 4.169e-08
