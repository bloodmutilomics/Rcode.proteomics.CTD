#' Robust rank aggregation for gene list integration and meta-analysis
#'
#' @name clusterProfile
#' @author Yong Huang
      

library(RobustRankAggreg)

## Read RFE result
df<-read.csv("bestsize.counts.s120.csv")
rownames(df)<-df$X

dd<- df[, -c(1:2)]
list <- as.list(dd)

glist<- vector(mode = "list", length = ncol(dd) )

for (i in 1:length(list)) {    
   names(list[[i]]) <-rownames(dd)
   gg <-sort(list[[i]], decreasing = T)
   glist[i] <- list(names(gg))
}


r = rankMatrix(glist)

rk<- aggregateRanks(glist)
write.csv(rk, file="rank.csv")

## betaScore ##############
# Takes in a vector with values in [0, 1]. It sorts the values to get the order statistics and calculates
# p-values for each of the order statistics. These

betaScore<-betaScores(rk$Score)


## rho: Takes in a vector with values in [0, 1]. Applies betaScores to the vector, takes the minimum of the
# beta scores and converts it to a valid p-value.

rho<-rhoScores(betaScore)
write.csv(cbind(rk, betaScore, rho), file="rank0.csv")



################# plot log-score 
df<- read.csv("rank0.csv")

score<- df$logScore 
xx<- score[1:100]
plot(xx)
abline(v = 37, col="red")