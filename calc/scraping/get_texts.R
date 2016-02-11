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
     save.image("../in/src_nytimes.Rdata")
} else load("../in/src_nytimes.Rdata")

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
while(tmp > 0){
    urls_tmp <- urls[is.na(nyt_articles$text)]
    nyt_tmp <- get_text(urls_tmp)
    nyt_articles[is.na(nyt_articles$text), ] <- nyt_tmp
    tmp <- length(urls_tmp) - sum(is.na(nyt_articles$text))
}


###############################

# most viewed
nyt_viewed <- get_text(src_shared$Most.Viewed.URL)
nyt_viewed$uniqueid <- seq(200001, 200000 + nrow(nyt_viewed), 1)
save(nyt_viewed, file="../in/nyt_viewed.Rdata")
rm(nyt_viewed)

# most facebook
nyt_facebook <- get_text(src_shared$Most.Facebook.URL)
nyt_facebook$uniqueid <- seq(300001, 300000 + nrow(nyt_facebook), 1)
save(nyt_facebook, file="../in/nyt_facebook.Rdata")
rm(nyt_facebook)

# most emailed
nyt_emailed <- get_text(src_shared$Most.Emailed.URL)
nyt_emailed$uniqueid <- seq(400001, 400000 + nrow(nyt_emailed), 1)
save(nyt_emailed, file="../in/nyt_emailed.Rdata")
rm(nyt_emailed)

# most tweeted
nyt_tweeted <- get_text(src_shared$Most.Tweeted.URL)
nyt_tweeted$uniqueid <- seq(500001, 500000 + nrow(nyt_tweeted), 1)
save(nyt_tweeted, file="../in/nyt_tweeted.Rdata")
rm(nyt_tweeted)

# front page
nyt_front <- get_text(src_front$url)
nyt_front$uniqueid <- seq(600001, 600000 + nrow(nyt_front), 1)
save(nyt_front, file="../in/nyt_front.Rdata")
rm(nyt_front)

## digital (split up due to frequent connection problems)
nyt_digital1 <- get_text(src_digital$url[1:5000])
nyt_digital2 <- get_text(src_digital$url[5001:10000])
nyt_digital3 <- get_text(src_digital$url[10001:length(src_digital$url)])
nyt_digital <- rbind(nyt_digital1,nyt_digital2,nyt_digital3)
nyt_digital$uniqueid <- seq(700001, 700000 + nrow(nyt_digital), 1)
save(nyt_digital, file="../in/nyt_digital.Rdata")
rm(nyt_digital)


### combine all dataframes

combine <- T
if(combine == T){
    # load individual datasets
    load("../in/nyt_viewed.Rdata")
    load("../in/nyt_facebook.Rdata")
    load("../in/nyt_emailed.Rdata")
    load("../in/nyt_tweeted.Rdata")
    load("../in/nyt_front.Rdata")
    load("../in/nyt_digital.Rdata")

    # add type variable
    nyt_viewed$type <- "viewed"
    nyt_facebook$type <- "facebook"
    nyt_emailed$type <- "emailed"
    nyt_tweeted$type <- "tweeted"
    nyt_front$type <- "front"
    nyt_digital$type <- "digital"

    # combine dataframes
    nyt_combined <- rbind(nyt_viewed, nyt_facebook, nyt_emailed
                          , nyt_tweeted, nyt_front, nyt_digital)

    # create common ids for articles and combine them in dataframe
    # I used urls here first, but titles work better
    ids <- unique(nyt_combined$title)
    ids <- data.frame(id = seq(100001,100000+length(ids),1), title = ids)

    # merge ids to dataframe
    nyt_combined <- merge(nyt_combined, ids, all = TRUE)
    
    save(nyt_combined, file = "../in/nyt_combined.Rdata")
}





