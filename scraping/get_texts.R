rm(list=ls())
setwd("/data/Uni/projects/2015/nyt/scraping")
library(rvest)
library(stringr)
library(magrittr)
library(xlsx)

### reading in the xlsx file -> not run everytime since it takes too long
# src_shared <- read.xlsx("NYTimes Shared Digital Front Page 0218-0428.xlsm", sheetName = "Shared")
# src_front <- read.xlsx("NYTimes Shared Digital Front Page 0218-0428.xlsm", sheetName = "FrontPage")
# src_digital <- read.xlsx("NYTimes Shared Digital Front Page 0218-0428.xlsm", sheetName = "DigitalEdition")
# save.image("src_nytimes.Rdata")
load("src_nytimes.Rdata")

# function to parse the article from nyt, maybe I could switch from a loop to apply/plyr
get_text <- function(urlvec){
    urlvec <- as.character(urlvec)
    out <- NULL
    pb <- txtProgressBar(min = 0, max = length(urlvec), style = 3)
    for(i in 1:length(urlvec)){
        if(!is.na(urlvec[i])){
            # parse html page
            page <- html(urlvec[i])

            # get title information
            title <- page %>% html_nodes("title") %>% html_text()
            title <- sub(" - The New York Times", "", title[1])

            # get full text of article
            text <- page %>% html_nodes("p") %>% html_text()
            text <- text[text != "Advertisement"]
            text <- paste(text, collapse = " ") %>%
                gsub("\n"," ", . ,fixed=T) %>% gsub("\t"," ", . ,fixed=T) %>%
                gsub("[[:punct:]+]"," ", .) %>% gsub("[[:digit:]+]"," ", .) %>%
                gsub("[[:space:]]+"," ", .) %>% str_trim() %>% tolower()

            # get keywords
            meta <- page %>% html_nodes("meta")
            news_keywords <- meta[(xml_attr(meta, name="name") %in% "news_keywords")][1] %>%
                xml_attr(name="content")
            keywords <- meta[(xml_attr(meta, name="name") %in% "keywords")][1] %>%
                xml_attr(name="content")

            # combine outout
            out <- rbind(out, c(urlvec[i], title, keywords, news_keywords, text))
        } else out <- rbind(out, c(urlvec[i], NA, NA, NA, NA))
        setTxtProgressBar(pb, i)
    }
    close(pb)
    out <- data.frame(out, stringsAsFactors = FALSE)
    colnames(out) <- c("link", "title", "keywords", "news_keywords", "text")
    return(out)
}


### download articles

# most viewed
nyt_viewed <- get_text(src_shared$Most.Viewed.URL)
save(nyt_viewed, file="nyt_viewed.Rdata")
rm(nyt_viewed)

# most facebook
nyt_facebook <- get_text(src_shared$Most.Facebook.URL)
save(nyt_facebook, file="nyt_facebook.Rdata")
rm(nyt_facebook)

# most emailed
nyt_emailed <- get_text(src_shared$Most.Emailed.URL)
save(nyt_emailed, file="nyt_emailed.Rdata")
rm(nyt_emailed)

# most tweeted
nyt_tweeted <- get_text(src_shared$Most.Tweeted.URL)
save(nyt_tweeted, file="nyt_tweeted.Rdata")
rm(nyt_tweeted)

# front page
nyt_front <- get_text(src_front$url)
save(nyt_front, file="nyt_front.Rdata")
rm(nyt_front)

# digital
nyt_digital <- get_text(src_digital$url)
save(nyt_digital, file="nyt_digital.Rdata")
rm(nyt_digital)




