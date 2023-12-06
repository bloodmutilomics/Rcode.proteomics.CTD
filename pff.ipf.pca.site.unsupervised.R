#' Unsupervised PCA of PFF IPF: bar plot of PC1 and PC2 for each site
#'
#' @name pff.ipf.pca.site.unsupervised
#' @author Yong Huang
      
library("FactoMineR")

## Read PFF NPX data for IPF
df1<-read.csv("npx.pff.ipf.ctd.csv")
rownames(df1)<-df1[, 1]

# Read PFF clinical data
# df2<-read.csv("SampleInfo.JO.csv")
df2<-read.csv("Explore_3072_PFF_Surv.csv")
rownames(df2)<- paste("PFF", df2[, 1], sep="")

dd1<- subset(df1, Diagnosis=="IPF")
dd2<- subset(df2, IPF==1)

idx<- intersect(rownames(dd1), rownames(dd2))
length(idx)

dd<-cbind(dd2[idx, ]$Site, dd1[idx, ])
colnames(dd)[1]<-"Site0"
write.csv(dd, file="PFF.IPF.csv")

###########
Diagnosis<- dd$Diagnosis
Sex<-dd$Sex
Site<-dd$Site0

dt<-dd[, -c(1:5)] 

### standard deviation

sd<- apply(dt, 2, sd)
names(sd)<- colnames(dd)

## top 500 proteins
sd<-sort(sd, decreasing = T)
var<- sd[c(1:500)]

xx <- dd[, names(var)]

dat<- cbind(Site, xx)

res.pca <- PCA(dat,  quali.sup=1)

plot(res.pca, choix="ind", habillage=1, legend = list(bty = "y", x = "right"), col.hab=c("green","blue"), label="none" )


#############
library("ggpubr")
pc1<-res.pca$ind$coord[, 1]
pc2<-res.pca$ind$coord[, 2]

ano<- aov(pc1 ~ Site)
anova(ano)


boxplot(pc1 ~Site,lwd = 1, cex.axis=0.9, las=3, xlab = "", srt = 35)

ano<- aov(pc2 ~ Site)
anova(ano)

##############box plot 
my_data1 <- as.data.frame( cbind(Site, pc1) )
ggboxplot(my_data1, x = "Site", y = "pc1", 
                    ylab = "pc1", xlab = "Site")

my_data2 <- as.data.frame( cbind(Site, pc2) )
ggboxplot(my_data1, x = "Site", y = "pc2", 
                    ylab = "pc1", xlab = "Site")

