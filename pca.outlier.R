#' Unsupervised PCA of PFF to identify sample outliers 
#'
#' @name pca.pff.outlier
#' @author Yong Huang
      

library("FactoMineR")

df<-read.csv("npx.PFF.IPF.CTD_original.csv")
rownames(df)<-df$Unique_Sample_ID

Sex<-df$Sex
Diagnosis <- df$Diagnosis

dd<-df[, -c(1:4)]

########################
sd<- apply(dd, 2, sd)
names(sd)<- colnames(dd)

sd<-sort(sd, decreasing = T)
var<- sd[c(1:500)]

xx <- dd[, names(var)]

dat<- cbind(Diagnosis, xx)

res.pca <- PCA(dat,  quali.sup=1)

plot(res.pca, choix="ind", habillage=1, legend = list(bty = "y", x = "topleft"), col.hab=c("green","blue"), col.quali = c("green", "blue"), label="none", cex=0.5, cex.axis=1, cex.lab=1)

##################################
coord<- res.pca$ind$coord
dat<- cbind(coord, df[rownames(coord), c(1:4)] )
write.csv(dat, file="coord.csv")

##################
coord<- res.pca$ind$coord
coord<-as.data.frame(coord)

### remove outlier
sub<-subset(coord, Dim.1 < 46)

dim(sub)
# [1] 881   5


df2<- df[rownames(sub), ]
dim(df2)
# [1]  881 2953

write.csv(df2, file="npx.PFF.IPF.flt.csv")


