#################################################
# Overlap between top 10 articles
#################################################

rm(list=ls())
setwd("~/Dropbox/Uni/Projects/2015/nyt/calc")
load("out/nyt_top10.Rdata")


## function to count percent of duplicates out of total number of unique articles
percentDuplicates <- function(x, y){
  ## remove duplicates in each vector, save as character, combine both vectors (creating new duplicates)
  xy <- c(as.character(unique(x)), as.character(unique(y)))
  ## percent of duplicates out of total number of unique articles
  (length(xy) - length(unique(xy))) / length(unique(xy))
}

## check function
percentDuplicates(c("A","B","C"), c("D", "E"))
percentDuplicates(c("A","B","C"), c("A","B","C"))
percentDuplicates(c("A","B","C"), c("A","B","B","D"))

## % overlap in unique articles (i.e., not counting how often or on what day they appear in each group)
titles <- c("Most.Emailed.Title", "Most.Facebook.Title", "Most.Tweeted.Title", "Most.Viewed.Title")
overlap_total <- matrix(NA, ncol = 4, nrow = 4, dimnames = list(titles, titles))
for(i in 1:4){
  for(j in i:4){
    overlap_total[i,j] <- percentDuplicates(nyt_top10[,titles[i]], nyt_top10[,titles[j]])
  }
}
overlap_total

## average % overlap on the same day
dates <- as.character(na.omit(unique(nyt_top10$Date)))
overlap_day <- array(NA, dim = c(4, 4, length(dates)), 
                     dimnames = list(titles, titles, as.character(dates)))
for(t in dates){
  for(i in 1:4){
    for(j in i:4){
      overlap_day[i,j,t] <- percentDuplicates(nyt_top10[nyt_top10$Date == t, titles[i]], 
                                              nyt_top10[nyt_top10$Date == t, titles[j]])
    }
  }
}
apply(overlap_day, c(1,2), mean)
