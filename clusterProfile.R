#' Pathway analysis based on 100x t-test
#'
#' @name clusterProfile
#' @author Yong Huang
     

# Load required libraries
library("R.utils")
R.utils::setOption("clusterProfiler.download.method","auto")
library(clusterProfiler)
library(enrichplot)
library(ggplot2)
library(org.Hs.eg.db)

# Set organism
organism = "org.Hs.eg.db"

# Read in data from deseq2
df = read.csv("ttest.pff.100resampling.csv")
rownames(df) <- df[, 1]

# Create a vector of the log2 fold change
original_gene_list <- df$logFC
names(original_gene_list) <- rownames(df)
gene_list <- na.omit(original_gene_list)
gene_list <- sort(gene_list, decreasing = TRUE)

# Perform gene set enrichment
gseGO <- gseGO(geneList = gene_list, 
             ont = "ALL", 
             keyType = "SYMBOL", 
             nPerm = 10000, 
             minGSSize = 5, 
             maxGSSize = 500, 
             pvalueCutoff = 0.05, 
             verbose = TRUE, 
             OrgDb = org.Hs.eg.db, 
             pAdjustMethod = "BH")

# save the results to a CSV file
write.csv(gseGO@result, file = "gseGO.csv")

# Create a dotplot of the results
require(DOSE)
dotplot(gseGO, showCategory = 7, split = ".sign") + facet_grid(.~.sign)



############################# wikipathwas ################################
get_wp_organisms()

########################## GSEA ##############################
ids<-bitr(names(original_gene_list), fromType = "SYMBOL", toType = "ENTREZID", OrgDb=organism)

# remove duplicate IDS 
dedup_ids = ids[!duplicated(ids[c("SYMBOL")]),]

df2 = df[rownames(df) %in% dedup_ids$SYMBOL,]
df2$ENTREZID = dedup_ids$ENTREZID

gene_list <- df2$logFC # Create a vector of the gene unuiverse
names(gene_list) <- df2$ENTREZID # Name vector with ENTREZ ids
gene_list <-na.omit(gene_list)  # omit any NA values 
gene_list <-sort(gene_list, decreasing = T)

### run wikipathway gsea
gseWP<- gseWP(gene_list, organism = "Homo sapiens")

write.csv(gseWP@result, file="gseWP.csv")

### plot
dotplot(gseWP, showCategory = 9, title = "Enriched Pathways" , split=".sign") + facet_grid(.~.sign)

######GSEA plot
gseaplot(gseWP, by = "all", title = gseWP$Description[1], geneSetID = 1)

## network
gseWP.pairwise <- pairwise_termsim(gseWP)
emapplot(gseWP.pairwise)

# convert GeneID to Symbol
# pos<- which(kk2@result$Description=="NOD-like receptor signaling pathway")

symbol<-""

for (i in 1:nrow(gseWP@result) ) {
   gid<- gseWP@result$core_enrichment[i] 
   gid<-strsplit(gid, split = "/")
   gid<-as.data.frame(gid)
   gid<- gid[,1]
   id.mtx <- bitr(gid, fromType = "ENTREZID",toType = "SYMBOL", OrgDb = org.Hs.eg.db)
   symbol[i] <- capture.output ( cat(id.mtx[, 2], sep="/" ) )
 }

write.csv(cbind(gseWP@result, symbol), file="gseWP2.csv")
