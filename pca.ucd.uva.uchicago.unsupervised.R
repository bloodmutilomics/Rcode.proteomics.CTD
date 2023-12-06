#' Unsupervised PCA of UC-Davis and UVA/UChicago NPX 
#'
#' @name pca.ucd.uva.uchicago.unsupervised
#' @author Yong Huang
      

library("FactoMineR")

df<-read.csv("npx.recital.ucd.uva.uchicago.csv")
rownames(df)<-df[, 1]

df2 <- subset(df, Site != "RECITAL")

Diagnosis<- df2$Diagnosis
Sex<-df2$Sex
Site<-df2$Site

dd<-df2[, -c(1:6)]

# Calculate standard deviation
sd<- apply(dd, 2, sd)
names(sd)<- colnames(dd)

# top 500 proteins
sd<-sort(sd, decreasing = T)
var<- sd[c(1:500)]

xx <- dd[, names(var)]
dat<- cbind(Site, xx)


res.pca <- PCA(dat,  quali.sup=1)

plot(res.pca, choix="ind", habillage=1, legend = list(bty = "y", x = "right"), col.hab=c("green", "blue"), label="none", col.quali = c("green",  "blue"), cex=0.6, select = "dist 8" )


t.test(res.pca$ind$coord[, 1] ~ Site)
# p-value =2.36e-13

t.test(res.pca$ind$coord[, 2] ~ Site)
# p-value = 0.4008

