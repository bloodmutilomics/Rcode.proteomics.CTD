#' Unsupervised PCA of UVA/UChicago cohort
#'
#' @name pca.test.unsupervised
#' @author Yong Huang
      

library("FactoMineR")

#Read UVA/UChicago NPX data
df<-read.csv("npx.nonPFF.ipf.ctd4.csv")
rownames(df)<-df$SampleID

Diagnosis<- df$Diagnosis
Sex<-df$Sex
Site<-df$Site

dd<-df[, -c(1:14)]


# Standard deviation
sd<- apply(dd, 2, sd)
names(sd)<- colnames(dd)

# top 500 proteins
sd<-sort(sd, decreasing = T)
var<- sd[c(1:500)]

xx <- dd[, names(var)]

dat<- cbind(Diagnosis, xx)
#dat<- cbind(Sex, xx)
#dat<- cbind(Site, xx)

res.pca <- PCA(dat,  quali.sup=1)

plot(res.pca, choix="ind", habillage=1, legend = list(bty = "y", x = "right"), col.hab=c("green","blue"), col.quali = c("green", "blue"), label="none", cex=0.6, cex.axis=1, cex.lab=1)


t.test(res.pca$ind$coord[, 1] ~ Diagnosis)
#p-value = 0.0011

t.test(res.pca$ind$coord[, 2] ~ Diagnosis)
#p-value = 0.4379


# not run
t.test(res.pca$ind$coord[, 1] ~ Sex)
# p-value = 0.2099

t.test(res.pca$ind$coord[, 2] ~ Sex)
# p-value = 0.2582


t.test(res.pca$ind$coord[, 1] ~ Site)
#p-value = 1.111e-11


t.test(res.pca$ind$coord[, 2] ~ Site)
# p-value < 2.2e-16