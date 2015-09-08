#################################################
# Preliminary analyses of nytimes articles (stm)
#################################################

rm(list=ls())
setwd("/data/Uni/projects/2015/nyt/calc/scraping")
load("../in/nyt_combined.Rdata")
library(stm)


### reduce dataset to single observations for each article

nyt_combined <- nyt_combined[order(nyt_combined$id),]
nyt_combined$select <- c(1,diff(nyt_combined$id))
nyt_reduced <- nyt_combined[nyt_combined$select == 1
                          , !is.element(colnames(nyt_combined), c("uniqueid","type","select"))]


### stm analyses of unique articles





