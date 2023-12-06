#' Protein classifier enriched in Gene Ontology
#'
#' @name GOSim
#' @author Yong Huang
      

library(GOSim)
library(org.Hs.eg.db)
library(topGO)
library(clusterProfiler)

#################################

setEvidenceLevel(organism = "human");
x <- org.Hs.egGENENAME;
# Get the gene names that are mapped to an entrez gene identifier
mapped_genes <- mappedkeys(x)
# Convert to a list
xx <- as.list(x[mapped_genes])
GOGeneCodes = as.numeric(names(xx));



######## Read RFE report
df<-read.csv("bestsize.counts.csv")
rownames(df)<-df[, 1]

## Top 37 proteins
dd <- subset(df, S120>=83)

## Convert Symbol to ENTREZID
genes.symbol <- rownames(dd)
genes.entrez <- bitr(genes.symbol, fromType = "SYMBOL",toType = "ENTREZID",OrgDb = org.Hs.eg.db)

bkgd.genes.name <- rownames(df)
bkgd.genes.entrez <- bitr(bkgd.genes.name, fromType = "SYMBOL",toType = "ENTREZID",OrgDb = org.Hs.eg.db)


####################### retrieve GO 
bkgd <- bkgd.genes.entrez
bkgd.gid <- unique(bkgd[, 2]);

# Match them to the gene identifiers in the GO database
bkgd2GO = match(bkgd.gid, GOGeneCodes);
table(is.finite(bkgd2GO));

# Keep only ones that were found
bkgdInDB = bkgd.gid[is.finite(bkgd2GO)];

moduleLL = genes.entrez[, 2]
# moduleLL = unique(moduleLL[is.finite(moduleLL)]);
moduleLL <- unique(moduleLL)

# Match module Gene ID to the GO gene identifiers
mod2GO = match(moduleLL, GOGeneCodes);
table(is.finite(mod2GO));

# Keep only those that were found in the database
modLLInDB = moduleLL[is.finite(mod2GO)];

# This GOSim function performs the enrichment analysis
GOana = GOenrichment(as.character(modLLInDB), as.character(bkgdInDB));

# The list GOana contains the results, for example: 
GOana$GOTerms;
GOana$p.values;

dd1<- GOana$GOTerms;
p.adj<- GOana$p.values
dd<- cbind(dd1, p.adj)

write.csv(dd, file="GOterm.csv")


### save GO_id and ENTREZID
library (plyr)
df <- ldply (GOana$genes, data.frame)
colnames(df)<- c("GOid", "GeneID")
write.csv(df, file="GOgenes.csv")


################ Convert ENTREZID to Symbolin output

yy<- data.frame(matrix(ncol=ncol(df) + ncol(genes.entrez) ) )
colnames(yy) <- c(colnames(df), colnames(genes.entrez) )

xx<-""; n=1;

for ( i in 1:nrow(df) ) {
  for (j in 1:nrow(genes.entrez) ) {
     if (df[i, 2] == genes.entrez[j, 2]) {
        xx[n] <- genes.entrez[j, 1]
        yy[n, ] <- cbind(df[i, ], genes.entrez[j, ] )

        n<- n+1
      }
    }
}
        
write.csv(yy, file="GOterm.Symbol.csv")


# Citation: Alexa A, Rahnenführer J, Lengauer T. Improved scoring of functional groups from gene expression data by decorrelating GO graph structure, Bioinformatics, 2006, 22(13):1600-160





