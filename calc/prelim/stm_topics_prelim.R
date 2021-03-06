#################################################
# Preliminary analyses of nytimes articles (stm)
#################################################
## Part I: Topic models, selection of articles, measure complexity

rm(list=ls())
setwd("/data/Uni/projects/2015/nyt/calc")
load("in/nyt_articles.Rdata")
load("in/nyt_src.Rdata")
library(stm)
library(dplyr)
library(car)
library(quanteda)


### function taken from
# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           capture.output(format(utils::object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

# shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

lsos()


### merge original link lists with scraped articles

nyt_viewed <- data.frame(link = src_shared$Most.Viewed.URL, date = src_shared$Date
                       , time = src_shared$Time) %>% left_join(nyt_articles) %>%
    mutate(uniqueid = 200001:(200000 + nrow(src_shared)), type = "viewed")
nyt_facebook <- data.frame(link = src_shared$Most.Facebook.URL, date = src_shared$Date
                         , time = src_shared$Time) %>% left_join(nyt_articles) %>%
    mutate(uniqueid = 300001:(300000 + nrow(src_shared)), type = "facebook")
nyt_emailed <- data.frame(link = src_shared$Most.Emailed.URL, date = src_shared$Date
                         , time = src_shared$Time) %>% left_join(nyt_articles) %>%
    mutate(uniqueid = 400001:(400000 + nrow(src_shared)), type = "emailed")
nyt_tweeted <- data.frame(link = src_shared$Most.Tweeted.URL, date = src_shared$Date
                         , time = src_shared$Time) %>% left_join(nyt_articles) %>%
    mutate(uniqueid = 500001:(500000 + nrow(src_shared)), type = "tweeted")
nyt_front <- data.frame(link = src_front$url, date = src_front$Date
                      , time = src_front$Time, author = src_front$Author
                      , srctitle = src_front$Title) %>% left_join(nyt_articles) %>%
    mutate(uniqueid = 600001:(600000 + nrow(src_front)), type = "front")
nyt_digital <- data.frame(link = src_digital$url, date = src_digital$Date
                        , time = src_digital$Time, author = src_digital$Author
                        , srctitle = src_digital$Title, section = src_digital$Section
                        , subsection = src_digital$Subsection) %>% left_join(nyt_articles) %>%
    mutate(uniqueid = 700001:(700000 + nrow(src_digital)))
nyt_digital$type <- recode(as.numeric(nyt_digital$section)
                         , "1 = 'digital_opinion'; 2 = 'digital_bottom'; 3:5 = 'digital_topnews'")


### combine merged articles in single dataframe

nyt_combined <- bind_rows(nyt_viewed, nyt_facebook, nyt_emailed
                        , nyt_tweeted, nyt_front, nyt_digital)
save(nyt_combined, file = "in/nyt_combined.Rdata")
rm(nyt_viewed, nyt_facebook, nyt_emailed, nyt_tweeted, nyt_front, nyt_digital, nyt_articles
 , src_shared, src_front, src_digital)
gc()


### reduce dataset to single observations for each article, add meta data

nyt_reduced <- nyt_combined %>% filter(!duplicated(nyt_combined$title) & text != "") %>%
    select(link, author, title, keywords, news_keywords, text) 
meta <- data.frame(model.matrix(~ type, nyt_combined)[,-1])
colnames(meta) <- gsub("type","",colnames(meta))
meta$digital_bottom <- as.numeric(apply(meta,1,sum) == 0)
meta$title <- nyt_combined$title
meta <- meta %>% group_by(title) %>% summarize_each(funs(max))
nyt_reduced <- nyt_reduced %>% left_join(meta)
save(nyt_reduced, file = "in/nyt_reduced.Rdata")
rm(nyt_combined)
gc()


### calculate readability

nyt_readab <- readability(nyt_reduced$text)
save(nyt_readab, file = "in/nyt_readab.Rdata")
rm(nyt_readab)
gc()


### stm analyses to select politics/econ articles

processed_select <- textProcessor(nyt_reduced$text
                      , metadata = nyt_reduced[c("title","emailed","facebook","front","tweeted"
                                                   ,"viewed","digital_opinion","digital_topnews"
                                                   ,"digital_bottom")])
out_select <- prepDocuments(processed_select$documents, processed_select$vocab
                          , processed_select$meta, lower.thresh = 10)
length(out_select$vocab) # vocabulary < 10000 for "Spectral" analyses
stm_select <- stm(out_select$documents, out_select$vocab, K = 5
                  , prevalence =~ emailed + facebook + front + tweeted + viewed +
                        digital_opinion + digital_topnews + digital_bottom
                  , max.em.its = 75, data = out_select$meta, init.type = "Spectral")

## explore words associated with each topic
labelTopics(stm_select)
topic_polecon <- apply(stm_select$theta, 1, function(x) which(x == max(x)))
topic_polecon <- topic_polecon == 1 | topic_polecon == 3 | topic_polecon == 5

## reduce dataset to politics/econ topic
nyt_polecon <- nyt_reduced[topic_polecon,]
save(nyt_polecon, file = "in/nyt_polecon.Rdata")

## save and delete stm for selection
save(processed_select, out_select, stm_select, file = "in/stm_select.Rdata")
rm(processed_select, out_select, stm_select)
gc()


### stm analysis on remaining articles

processed_polecon <- textProcessor(nyt_polecon$text
                         , metadata = nyt_polecon[c("title","emailed","facebook","front","tweeted"
                                                   ,"viewed","digital_opinion","digital_topnews"
                                                   ,"digital_bottom")])
out_polecon <- prepDocuments(processed_polecon$documents, processed_polecon$vocab
                           , processed_polecon$meta, lower.thresh = 10)
length(out_polecon$vocab)
stm_polecon <- stm(out_polecon$documents, out_polecon$vocab, K = 10
                  , prevalence =~ emailed + facebook + front + tweeted + viewed +
                        digital_opinion + digital_topnews + digital_bottom
                  , max.em.its = 75, data = out_polecon$meta, init.type = "Spectral")


### save results
save(processed_polecon, out_polecon, stm_polecon, file = "in/stm_polecon.Rdata")



