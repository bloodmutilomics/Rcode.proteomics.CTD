#' PCA supervised by protein classifier; PC1 and PC2 bar plot of site for PFF CTD
#'
#' @name pff.ctd.pca.site.supervised
#' @author Yong Huang
      

library("FactoMineR")

# Read PFF NPX data for CTD
df1<-read.csv("../npx.pff.ipf.ctd2.csv")
rownames(df1)<-df1[, 1]

# Read clinical data
#df2<-read.csv("SampleInfo.JO.csv")
df2<-read.csv("Explore_3072_PFF_Surv.csv")
rownames(df2)<- paste("PFF", df2[, 1], sep="")

dd1<- subset(df1, Diagnosis=="CTD")
dd2<- subset(df2, IPF==0)


idx<- intersect(rownames(dd1), rownames(dd2))
length(idx)

dd<-cbind(dd2[idx, ]$Site, dd1[idx, ])
colnames(dd)[1]<-"Site0"
# write.csv(dd, file="PFF.IPF.csv")

###########
Diagnosis<- dd$Diagnosis
Sex<-dd$Sex
Site<-dd$Site0


## top 37 proteins by RFE
df3<- read.csv("bestsize.counts.csv")
dd3<-subset(df3, S120>=83)
dat<- cbind(Site, dd[, dd3[, 1] ] )

res.pca <- PCA(dat,  quali.sup=1)
plot(res.pca, choix="ind", habillage=1, legend = list(bty = "y", x = "right"), col.hab=c("green","blue"), label="none" )



#############
library("ggpubr")
pc1<-res.pca$ind$coord[, 1]
pc2<-res.pca$ind$coord[, 2]

# pc1
boxplot(pc1 ~Site,lwd = 1, cex.axis=0.9, las=3, xlab = "", srt = 35)

ano<- aov(pc1 ~ Site)
anova(ano)
# p1= 0.01331464

p.adjust(p = 0.09433, method ="bonferroni", n = 40)


##pc2
ano<- aov(pc2 ~ Site)
anova(ano)
#p2=0.07006

boxplot(pc2 ~Site,lwd = 1, cex.axis=0.9, las=3, xlab = "", srt = 35)

#########################
my_data1 <- as.data.frame( cbind(Site, pc1) )

ggboxplot(my_data1, x = "Site", y = "pc1", 
                    ylab = "pc1", xlab = "Site")


