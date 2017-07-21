# author@narendra
# info@variantAnnotationScript
#installing variant annotation pacakages using bioconductor
#source("http://bioconductor.org/biocLite.R")
#biocLite("VariantAnnotation")



library(VariantAnnotation)
fl <- system.file("extdata", "chr22.vcf.gz", package="VariantAnnotation")
fl
vcf <- readVcf(fl, "hg19")
vcf
header(vcf)
info(header(vcf))
samples(header(vcf))
geno(header(vcf))
head(rowRanges(vcf))
ref(vcf)
qual(vcf)
alt(vcf)
geno(vcf)
geno(VCF)["DS"]

install.packages("jsonlite")
install.packages("httr")
library(RCurl)
library(httr)
r <- POST("http://www.datasciencetoolkit.org/text2people", 
          body = "Tim O'Reilly, Archbishop Huxley")
stop_for_status(r)
content(r, "parsed", "application/json")
