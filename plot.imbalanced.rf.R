#' Plot Imbalanced Random Forrest probabilities across RECEITAL, UC-Davis and UVA/UChicago cohort
#'
#' @name plot.imbalanced.rf
#' @author Yong Huang
  
library(ggplot2)
library(reshape2)

options(digits = 2)

# Read imbalanced RF probabilities of UVA/UChicago
df1<-read.csv("qclassifier2.test.csv")
rownames(df1)<-df1[, 1]

dd1<-subset(df1, Diagnosis.test=="CTD")
dd2<-subset(df1, Diagnosis.test=="IPF")

# Read imbalanced RF probabilities of RECEITAL/UC-Davis
df2<-read.csv("qclassifier.single.prob2.csv")
rownames(df2)<-df2[, 1]

dat1<- subset(df2, Diagnosis.test==0)
dat2<- subset(df2, Diagnosis.test==1)


Dx<- c(df1$Diagnosis.test, df2$Diagnosis.test)
Site<- c(df1$Site.test, df2$Site.test)
Sex<- c(df1$Sex.test, df2$Sex.test)
prob<- c(df1$IPF.prob, df2$IPF.prob)


Dx<- gsub(pattern = 0, replacement = "CTD", x = Dx)
Dx<- gsub(pattern = 1, replacement = "IPF", x = Dx)

col<- gsub(pattern = 'CTD', replacement = 'red', x = Dx)
col<- gsub(pattern = 'IPF', replacement = 'blue', x = col)
names(col) <- Site

dd<-cbind(col, Dx, Site, prob)
colnames(dd)[4] <- "Probabilities"
dd<-as.data.frame(dd)
dd$Probabilities<- as.numeric(dd$Probabilities)
rownames(dd)<- c(rownames(df1), rownames(df2))

options(digits = 2)

Site<- dd$Site
Probabilities<- dd$Probabilities


######################## t-test ##################
# RECITAL RF probabilities 
ctd.recital <- dat1$IPF.prob

# Test CTD RF probabilities 
ctd.test <- dd1$IPF.prob


# UCD RF probabilities 
ipf.ucd <- dat2$IPF.prob


# Test IPF RF probabilities 
ipf.test <- dd2$IPF.prob


t.test(ctd.recital, ctd.test)
#p-value = 0.06

t.test(ipf.ucd, ipf.test)
#p-value = 0.5



############# plot probabilities
p1 <- ggplot(dd)+ ggtitle( "Imbalanced-RF Probability") +  
geom_jitter(aes(x =Site, y =Probabilities , col=Dx ), pch=19, width = 0.2, height =0, show.legend=T) + theme_bw()   

p1+ labs(y = "IPF Probalility", sixe=6) + geom_hline(yintercept = 0.5, col="blue", linetype = "dashed")


##################################################################################################
my.theme<- theme( # axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x =element_blank(),
      axis.text.x=element_text(angle=0, hjust=0, size=12),
      legend.text=element_text(size=12),     
      legend.title=element_blank(),    
      axis.title.y =element_text(size=12) 
)  

p<- p1  + theme_classic() + my.theme 




######################### not run 
dd1<- subset(dd, Probabilities< 0.5 )
dd2<- subset(dd, Probabilities >= 0.5)
