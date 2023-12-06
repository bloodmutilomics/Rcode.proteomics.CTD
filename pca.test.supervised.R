#' Supervised PCA of UVA/UChicago cohort by protein classifier
#'
#' @name pca.test.supervised
#' @author Yong Huang
      
library("FactoMineR")

df<-read.csv("npx.nonPFF.ipf.ctd4.csv")
rownames(df)<-df$SampleID

Diagnosis<- df$Diagnosis
Sex<-df$Sex
Site<-df$Site


# dd<-df[, -c(1:15)]

# priotein classifier
df2<-read.csv("bestsize.counts.csv")
rownames(df2)<-df2[, 1]

dd2 <- subset(df2, S120>=83)

dd<- df[, rownames(dd2) ]
dat<-cbind(Diagnosis, dd)


# Diagnosis
res.pca <- PCA(dat,  quali.sup=1)

plot(res.pca, choix="ind", habillage=1, legend = list(bty = "y", x = "right"), col.hab=c("green","blue"), col.quali = c("green", "blue"), label="none", cex=0.6, cex.axis=1, cex.lab=1)


###### Site 
dat<- cbind(Site, dd)

res.pca <- PCA(dat,  quali.sup=1)

plot(res.pca, choix="ind", habillage=1, legend = list(bty = "y", x = "right"), col.hab=c("green","blue"), col.quali = c("green", "blue"), label="none", cex=0.6, cex.axis=1, cex.lab=1)



###### Sex
dat<- cbind(Sex, dd)

res.pca <- PCA(dat,  quali.sup=1)

plot(res.pca, choix="ind", habillage=1, legend = list(bty = "y", x = "right"), col.hab=c("green","blue"), col.quali = c("green", "blue"), label="none", cex=0.6, cex.axis=1, cex.lab=1)




################### t-test of PC 
#Dx
t.test(res.pca$ind$coord[, 1] ~ Diagnosis)
p-value = 4.297e-09

t.test(res.pca$ind$coord[, 2] ~ Diagnosis)
p-value = 3.331e-4


# Sex
t.test(res.pca$ind$coord[, 1] ~ Sex)
p-value = 3.405e-06

t.test(res.pca$ind$coord[, 2] ~ Sex)
p-value = 0.005807


# Site
t.test(res.pca$ind$coord[, 1] ~ Site)
p-value = 0.5739

t.test(res.pca$ind$coord[, 2] ~ Site)
p-value =0.1864

