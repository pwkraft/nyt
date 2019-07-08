library(tidyverse)

## load data
load("out/nyt_top10.Rdata")
load("in/nyt_reduced.Rdata")

## unique articles in top10 data
length(unique(unlist(list(nyt_top10[,grep("Title", colnames(nyt_top10))]))))
length(unique(unlist(list(nyt_top10[,grep("URL", colnames(nyt_top10))]))))

## compare duplicate titles and urls
titles_dup <- matrix(duplicated(unlist(list((nyt_top10[,grep("Title", colnames(nyt_top10))])))), ncol = 4)
urls_dup <- matrix(duplicated(unlist(list((nyt_top10[,grep("URL", colnames(nyt_top10))])))), ncol = 4)
View(nyt_top10[apply((titles_dup - urls_dup) != 0, 1, sum) > 0,])

## different urls link to the same articles!
cbind(select(nyt_top10, contains("Title"))[(titles_dup - urls_dup) != 0],
      select(nyt_top10, contains("URL"))[(titles_dup - urls_dup) != 0]) %>%
  View()

## unique articles in nyt_reduced
nyt_reduced %>%
  filter(emailed == 1 | facebook == 1 | tweeted == 1 | viewed == 1) %>%
  View()
# not sure why the number of articles is lower in the redced dataset
# probably because I removed empty articles, but I should investigate further
