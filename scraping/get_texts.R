rm(list=ls())
setwd("/data/Uni/projects/2015/nyt/scraping")
library(rvest)
library(stringr)
library(magrittr)
library(XML)

# test using rvest
page <- html("http://www.nytimes.com/interactive/2015/02/17/upshot/what-do-people-actually-order-at-chipotle.html?src=me")

title <- page %>% html_nodes("title") %>% html_text()
title <- sub(" - The New York Times", "", title[1])

text <- page %>% html_nodes("p") %>% html_text()
text <- text[text != "Advertisement"]
text <- text[text != ""]
text <- paste(text, collapse = " ")
text <- gsub("\n"," ",text,fixed=T)
text <- gsub("\t"," ",text,fixed=T)
text <- gsub("[[:punct:]+]"," ",text)
text <- gsub("[[:digit:]+]"," ",text)
text <- gsub("[[:space:]]+"," ",text)
text <- str_trim(text)
text <- tolower(text)

meta <- page %>% html_nodes("meta")
news_keywords <- meta[(xml_attr(meta, name="name") %in% "news_keywords")][1] %>% xml_attr(name="content")
keywords <- meta[(xml_attr(meta, name="name") %in% "keywords")][1] %>% xml_attr(name="content")


# combine to dataset, loop over all entries in xls sheet
