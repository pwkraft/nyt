#################################################
# Extract top 10 article 
#################################################
## Part I: Topic models, selection of articles, measure complexity

rm(list=ls())
setwd("/data/Dropbox/Uni/Projects/2015/nyt/calc")
load("in/nyt_articles.Rdata")
load("in/nyt_src.Rdata")

## reduce articles to the ones that are included in nyt_top10
titles <- unique(c(as.matrix(src_shared[,grep("Title",colnames(src_shared))])))
urls <- unique(c(as.matrix(src_shared[,grep("URL",colnames(src_shared))])))
nyt_articles <- nyt_articles[(nyt_articles$title %in% titles | nyt_articles$link %in% urls),]

## save hazard data and articles in one data frame
nyt_top10 <- src_shared

save(nyt_articles, nyt_top10, file="out/nyt_top10.Rdata")