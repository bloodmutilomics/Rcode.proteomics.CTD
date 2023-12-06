#' volcano plot for 100x t-test 
#'
#' @name volcano.plot
#' @author Yong Huang
#' @import EnhancedVolcano


library(EnhancedVolcano)

## read the 100x resampling t-test result 
df<-read.csv("ttest.pff.100resampling.csv")
rownames(df)<-df[, 1]

#########################################
# create custom key-value pairs for 'high', 'low', 'mid' expression by fold-change
    # set the base colour as 'black'
    keyvals.colour1<- rep('grey', nrow(df))
    
    # set the base name/label as 'Mid'
    names(keyvals.colour1) <- rep('Insignificant', nrow(df) )

    # modify keyvals for transcripts with fold change > 1
    keyvals.colour1[match ( which(df$logFC > 0.584) ,  which( df$p.adj < 0.05)  ) ] <- 'red'
    names(keyvals.colour1)[ match ( which(df$logFC >0.584) ,  which(df$p.adj < 0.05)  )  ] <- 'logFC > 1'

    # modify keyvals for transcripts with fold change < -1
    keyvals.colour1[match ( which(df$logFC < -0.584) ,  which(df$p.adj < 0.05)  ) ] <- 'green'
    names(keyvals.colour1)[ match ( which(df$logFC < -0.584) ,  which(df$p.adj <0.05)  )  ] <- 'logFC < -1'
    
    unique(names(keyvals.colour1))


############# volcano plot ############
## tiff(filename = "volcano.tiff", height = 480, width = 1000)

# Create a volcano plot
plot.volcano <- EnhancedVolcano(df,  
      lab = rownames(df),  
      pCutoff = 0.01,
      FCcutoff = 0.3, 
      x = 'logFC',     
      y = 'p.adj',  
      xlim = c(-0.7, 1.2), 
      xlab = bquote('logFC'), 
      ylim = c(0, 4), 
      ylab = bquote('-log(p.adj)'),   
      title = 'PFF IPF Cox-PH',
      col = c('grey30', 'grey30', 'forestgreen', 'red2'),
      legendLabels = c("NS", "logFC >0.5", "p.adj <0.001", "p.adj<0.01 & logFC>0.5")  )

# Display the plot
plot.volcano   

dev.off()
