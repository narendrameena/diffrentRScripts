
# internet connection 

#options(download.file.method="wget")
#install.packages("lsa")
# install the biomaRt package
#source("http://bioconductor.org/biocLite.R")
#biocLite("biomaRt")
#pkgs <- rownames(installed.packages())
#biocLite(pkgs, type="source")
#biocLite("BiocUpgrade")
#Load biomart Library
setwd("/Users/naru/Documents/R_workshop/geneAnnotation")
getwd()
#update.packages("biomaRt")
library(biomaRt)
#ensembl=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org")
#listMarts(host="grch37.ensembl.org", path="/biomart/martservice")
# look at top 10 databases
head(listMarts(), 10)
#remove.packages("biomaRt")

listMarts()

ensMart<-useMart("ensembl")
listDatasets(ensMart)

ensembl_hs_mart <- useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl")

#ensembl = useDataset("hsapiens_gene_ensembl",mart=ensembl) 

list =listAttributes(ensembl_hs_mart)
#mart
list[1:100]
list[1:5,]$name
#ensembl_hs_mart <- useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl")
 ensembl_df <-  getBM(attributes=c("ensembl_gene_id", "ensembl_transcript_id",
                                  "hgnc_symbol","chromosome_name", "entrezgene"),
                      mart=ensembl_hs_mart)
 
write.csv(file="ensamble_data", x= ensembl_df)
#ensembl_hs_mart <- useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl")

#ensembl_df <-  getBM(attributes=c(unlist(listAttributes(ensembl_hs_mart)[1:5,]$name)), mart=ensembl_hs_mart)
write.csv(file="biomart_data.csv",x=getBM(attributes=c(unlist(listAttributes(ensembl_hs_mart)[1:40,]$name)), mart=ensembl_hs_mart))
my_genes = c("PIK3R1", "VCAM1")
write.csv(file="ensamble.csv",x=ensembl_df)
my_genes_ann = ensembl_df[match(my_genes, ensembl_df$hgnc_symbol),]
write.csv(file="HGBC_annotation.csv", x=my_genes_ann)
