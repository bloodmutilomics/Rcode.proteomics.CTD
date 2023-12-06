#' Plot partial effect and variable importance in imbalanced Rrandon forrest model
#'
#' @name randomForest.src.pEffect
#' @author Yong Huang
      

library(randomForestSRC)
library(dplyr)

######## step 0. read in protein classifier #####
df0<-read.csv("bestsize.counts.s120.csv") # input 1
rownames(df0)<- df0[, 1]

sub<- subset(df0, Sum >=83)
core.sig <- rownames(sub)
length(core.sig)

############ step 1. read in pre-assembled test cohort ##########
df2<- read.csv("test.csv", check.names = F) # input 2
rownames(df2)<-df2[, 1]

Diagnosis.test0  <- as.factor(df2$Diagnosis)
Site.test0 <- as.factor( df2$Site )
Sex.test0 <- as.factor( df2$Sex )
Age.test0 <- as.factor( df2$age.score )

x.test0 <- cbind(Diagnosis.test0, df2[, -c(1:14)])
colnames(x.test0)[1] <-"Diagnosis"

x.test <- cbind(Diagnosis.test0, df2[, core.sig])
colnames(x.test)[1] <-"Diagnosis"

x.test2<- cbind(Diagnosis.test0, Sex.test0, Age.test0, df2[, core.sig] )
colnames(x.test2)[1:3] <- c("Diagnosis", "Sex", "Age")
levels(x.test2$Sex)<- c(0, 1)
x.test2$Age <-as.integer(x.test2$Age) -1
x.test2$Sex <-as.integer(x.test2$Sex) -1



########## step 2. prepare Training cohort for dynamic assignment  ###########
df3 <-read.csv("training.csv", check.names=F)  #input 4
rownames(df3)<-df3$SampleID

Diagnosis <- as.factor(df3$Diagnosis)
dd3<- df3[, core.sig]

x.train0<- cbind(Diagnosis, df3[, -c(1:10)])
x.train<- cbind(Diagnosis, dd3)

Sex<- df3$sex.score
Age<-df3$age.score
x.train2<- cbind(Diagnosis, Sex, Age, df3[, core.sig])


##### Step 3. plot.rfsrc  ############################
# train.obj <- rfsrc(Diagnosis ~ ., data = x.train2, block.size = 1, importance = TRUE)
# train.obj <- imbalanced(Diagnosis ~ ., data = x.train2, block.size = 1, importance = TRUE, ntrees=500)

train.obj <- imbalanced(Diagnosis ~ ., data = x.train2, block.size = 1, importance = TRUE)


write.csv(train.obj$importance, file="vimp.csv")


############ Step 4a. plot.rfsrc Plot Error Rate and Variable Importance from a RF-SRC analysis 
## Ref-- Breiman L. (2001). Random forests, Machine Learning, 45:5-32. (http://dx.doi.org/10.1023/A:1010933404324)
## Plot out-of-bag (OOB) error rates and variable importance (VIMP) from a RF-SRC analysis
## Left:  cumulative OOB error rates as a function of number of trees
## Right: variable importance (VIMP)

plot(train.obj, cex.main=0.45)  # cex.axis=0.5, cex.lab=0.5, 

## permutation vimp
print(vimp(train.obj, importance = "permute")$importance) 
perm<-vimp(train.obj, importance = "permute")$importance

write.csv(perm, file="vimp.perm.csv")


##### Step 4b.  Plot Marginal Effect of Variables 
# Ref-- Friedman J.H. (2001). Greedy function approximation: a gradient boosting machine, Ann. of Statist., 5:1189-1232.
# Ishwaran H., Gerds T.A., Kogalur U.B., Moore R.D., Gange S.J. and Lau B.M. (2014). Random survival forests for competing risks. Biostatistics, 15(4):757-773.

## Plot the marginal effect of an x-variable on the class probability (classification)

# plot.variable(train.obj,  partial = TRUE, class.type='prob', plots.per.page=4, target=2 )  # target: class to focus

plot.variable(train.obj,  partial = TRUE, class.type='prob', plots.per.page=12, target=2, cex.label=1.2)  # target: class to focus


###### Step 4c.  Plot Subsampled VIMP Confidence Intervals  ############
# Ref -- Ishwaran H, Lu M. Standard errors and confidence intervals for variable importance in random forest regression, classification, and survival. Stat Med. 2019 Feb 20;38(4):558-582. doi: 10.1002/sim.7803. Epub 2018 Jun 4. PMID: 29869423; PMCID: PMC6279615.

## Plots VIMP (variable importance) confidence regions obtained from subsampling a forest.
## o <- rfsrc(Diagnosis ~ ., x.train2)
## oo <- subsample(o)
## plot(oo, cex.axis=.5) 

oo <- subsample (train.obj) 
plot(oo, cex.label=.5) 

tiff(file="VIMP.ci.tiff", width = 800, height = 800)
plot(oo, cex.lavb=.2) 
dev.off()

pdf(file="VIMP.ci.pdf")
plot(oo, cex.axis=.2) 
dev.off()

