#' Plot Random Forrest probabilities across RECEITAL, UC-Davis and UVA/UChicago cohort
#'
#' @name plot.rf
#' @author Yong Huang
  

library(ggplot2)
library(reshape2)

options(digits = 2)

## Read RF probabilities of UVA/Uchicago
df1<-read.csv("pred.prob.csv")
rownames(df1)<-df1[, 1]

dd1<-subset(df1, Diagnosis.test==0)
dd2<-subset(df1, Diagnosis.test==1)


## Read RF probabilities of RECITAL/UC-Davis
df2<-read.csv("pred.prob2.single.csv")
rownames(df2)<-df2[, 1]

dat1<- subset(df2, Diagnosis.test=="CTD")
dat2<- subset(df2, Diagnosis.test=="IPF")


Dx<- c(df1$Diagnosis.test, df2$Diagnosis.test)
Site<- c(df1$Site.test, df2$Site.test)
Sex<- c(df1$Sex.test, df2$Sex.test)
prob<- c(df1$mean.probability, df2$mean.probability2)


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


p1 <- ggplot(dd)+ ggtitle( "Random Forrest") +  
geom_jitter(aes(x =Site, y =Probabilities , col=Dx ),   pch=19, width = 0.2, height =0, show.legend=T) + theme_bw() 

p1 + labs(y = "IPF Probalility", sixe=6) + geom_hline(yintercept = 0.5, col="blue", linetype = "dashed")


######################## t-test ##################
# RECITAL RF probabilities 
ctd.recital <- dat1$mean.probability2

# Test CTD RF probabilities 
ctd.test <- dd1$mean.probability2


# UCD RF probabilities 
ipf.ucd <- dat2$mean.probability2


# Test IPF RF probabilities 
ipf.test <- dd2$mean.probability2

t.test(ctd.recital, ctd.test)
#p-value = 0.1

t.test(ipf.ucd, ipf.test)
#p-value = 0.1



########################### not run: stats ############
dd1<- subset(dd, Probabilities< 0.5 )
dd2<- subset(dd, Probabilities >= 0.5)


### UVA/UChicago < 0.5
dd1.uva.uc<- subset(dd1, Site=="UVA/Uhicago")

table(dd1.uva.uc$Dx)

## misclassified IPF =31 /192 =16.1%
# CTD IPF 
  45  31 

### UVA/UChicago > 0.5
dd2.uva.uc<- subset(dd2, Site=="UVA/Uhicago")

table(dd2.uva.uc$Dx)

## misclassified CTD =11 / 56 =19.6%
# CTD IPF 
# 11 161 


##############
my.theme<- theme( # axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x =element_blank(),
      axis.text.x=element_text(angle=0, hjust=0, size=12),
      legend.text=element_text(size=12),     
      legend.title=element_blank(),    
      axis.title.y =element_text(size=15), 
)  


p<- p1 + my.theme

#####################
mynamestheme <- theme(
  plot.title = element_text(family = "Helvetica", face = "bold", size = (15)),
  legend.title = element_text(colour = "steelblue", face = "bold.italic", family = "Helvetica"),
  legend.text = element_text(face = "italic", colour = "steelblue4", family = "Helvetica"),
  axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
  axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10))
)


p<- p1  + theme_classic() + my.theme 



   