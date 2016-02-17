rm(list=ls())
setwd("/data/Uni/projects/2015/nyt/calc/scraping")
library(rvest)
library(stringr)
library(magrittr)
library(xlsx)

## reading in the xlsx file -> not run every time since it takes too long
readxlsx <- F
if(readxlsx == T){
    src_shared <- read.xlsx("../in/NYTimes Shared Digital Front Page 0218-0428_check.xlsx"
                          , sheetName = "Shared")
    src_front <- read.xlsx("../in/NYTimes Shared Digital Front Page 0218-0428_check.xlsx"
                         , sheetName = "FrontPage")
    src_digital <- read.xlsx("../in/NYTimes Shared Digital Front Page 0218-0428_check.xlsx"
                           , sheetName = "DigitalEdition")
     save.image("../in/nyt_src.Rdata")
} else load("../in/nyt_src.Rdata")

## function to parse the article from nyt, maybe I could switch from a loop to apply/plyr
get_text <- function(urlvec, printurl = F){
    urlvec <- as.character(urlvec)
    out <- NULL
    if(printurl == F) pb <- txtProgressBar(min = 0, max = length(urlvec), style = 3)
    for(i in 1:length(urlvec)){
        # print url in each iteration instead of progress bar
        if(printurl == T) print(paste0(i,": ", urlvec[i]))
        if(!is.na(urlvec[i])){
            # parse html page
            page <- try(read_html(urlvec[i]), silent = T)
            if(class(page)[1] == "try-error"){
                warning(paste0("Error in url: ",urlvec[i]))
                out <- rbind(out, c(urlvec[i], NA, NA, NA, NA, NA))
            } else {
                ## get title information
                title <- page %>% html_nodes("title") %>% html_text()
                title <- sub(" - The New York Times", "", title[1])
                title <- sub(" - NYTimes.com", "", title)

                ## get full text of article
                text <- page %>% html_nodes("p") %>% html_text()
                text <- text[text != "Advertisement"]
                text <- paste(text, collapse = " ")

                ## get keywords etc
                meta <- page %>% html_nodes("meta")
                news_keywords <- meta[(xml_attr(meta, attr="name") %in% c("news_keywords","subj"))]
                if(length(news_keywords) > 0){
                    news_keywords <- xml_attr(news_keywords, attr="content")[1]
                } else news_keywords <- NA
                keywords <- meta[(xml_attr(meta, attr="name") %in% c("keywords","subj"))]
                if(length(keywords) > 0){
                    keywords <- xml_attr(keywords, attr="content")[1]
                } else keywords <- NA
                articleid <- meta[(xml_attr(meta, attr="name") %in% "articleid")]
                if(length(articleid) > 0){
                    articleid <- xml_attr(articleid, attr="content")[1]
                } else articleid <- NA

                ## combine outout
                out <- rbind(out, c(urlvec[i], title, keywords, news_keywords, articleid, text))
            }
        } else out <- rbind(out, c(urlvec[i], NA, NA, NA, NA, NA))
        if(printurl == F) setTxtProgressBar(pb, i)
    }
    if(printurl == F) close(pb)
    out <- data.frame(out, stringsAsFactors = FALSE)
    colnames(out) <- c("link", "title", "keywords", "news_keywords", "articleid" ,"text")
    return(out)
}


### download articles

## combine urls in single string, delete dublicates
urls <- unique(na.omit(c(as.character(src_shared$Most.Viewed.URL)
                       , as.character(src_shared$Most.Facebook.URL)
                       , as.character(src_shared$Most.Emailed.URL)
                       , as.character(src_shared$Most.Tweeted.URL)
                       , as.character(src_front$url)
                       , as.character(src_digital$url))))

## scrape text and meta info
nyt_articles <- get_text(urls)

## repeat scraping for missing texts
tmp <- length(urls) - sum(is.na(nyt_articles$text))
iteration <- 1
while(tmp > 0){
    urls_tmp <- urls[is.na(nyt_articles$text)]
    nyt_tmp <- get_text(urls_tmp)
    nyt_articles[is.na(nyt_articles$text), ] <- nyt_tmp
    tmp <- length(urls_tmp) - sum(is.na(nyt_articles$text))
    print(paste0("Iteration ",iteration,", improvements: ",tmp))
    iteration <- iteration + 1
}

## save dataset
save(nyt_articles, file="../in/nyt_articles.Rdata")



