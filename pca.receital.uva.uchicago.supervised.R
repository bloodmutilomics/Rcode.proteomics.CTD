#' Supervised PCA of RECEITAL and UVA/UChicago NPX by 37 proteins
#'
#' @name pca.receital.uva.uchicago.supervised
#' @author Yong Huang
      
library("FactoMineR")

# Read UC-Davis and UVA/UChicago NPX 
df<-read.csv("npx.recital.ucd.uva.uchicago.csv")
rownames(df)<-df[, 1]

# Remove UC-Davis NPX
dd<- subset(df, Site !="UC-Davis")
df<-dd

Diagnosis<- df$Diagnosis
Sex<-df$Sex
Site<-df$Site


### PCA
df2<-read.csv("bestsize.counts.csv")
rownames(df2)<-df2[, 1]

dd2 <- subset(df2, S120>=83)
dd<- df[, rownames(dd2) ]

dat<- cbind(Site, dd)

res.pca <- PCA(dat,  quali.sup=1)

plot(res.pca, choix="ind", habillage=1, legend = list(bty = "y", x = "right"), col.hab=c("green","blue"), label="none", col.quali = c("blue", "blue"), cex=0.6 )


t.test(res.pca$ind$coord[, 1] ~ Site)
#p-value < 2.2e-16

t.test(res.pca$ind$coord[, 2] ~ Site)
#p-value =2.143e-10


