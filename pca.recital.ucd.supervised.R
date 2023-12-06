#' Supervised PCA of RECEITAL and UC-Davis NPX by 37 proteins
#'
#' @name pca.recital.ucd.supervised
#' @author Yong Huang
      
library("FactoMineR")

# Read RECEITAL and UC-Davis NPX 
df<-read.csv("npx.recital.ucd.csv")
rownames(df)<-df[, 1]

Diagnosis<- df$Diagnosis
Sex<-df$Sex
Site<-df$Site


### PCA by 37 proteins 
df2<-read.csv("bestsize.counts.csv")
rownames(df2)<-df2[, 1]

dd2 <- subset(df2, S120>=83)
dd<- df[, rownames(dd2) ]

dat<- cbind(Site, dd)

res.pca <- PCA(dat,  quali.sup=1)

plot(res.pca, choix="ind", habillage=1, legend = list(bty = "y", x = "right"), col.hab=c("green","blue"), label="none", col.quali = c("green", "blue"), cex=0.6)


t.test(res.pca$ind$coord[, 1] ~ Site)
#p-value < 2.2e-16

t.test(res.pca$ind$coord[, 2] ~ Site)
#p-value =  2.771e-11

