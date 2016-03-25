##################################
## Analyses for MPSA 2016 paper ##
##################################

setwd("/data/Uni/projects/2015/nyt/calc")
rm(list=ls())
library(stm)
library(dplyr)
library(tidyr)
library(car)
library(ggplot2)

## load data
load("in/nyt_combined.Rdata")
load("in/nyt_reduced.Rdata")
load("in/nyt_polecon.Rdata")
load("in/nyt_readab.Rdata")
load("in/stm_res.Rdata")

## separate metadata
nyt_part <- c("digital_bottom","digital_opinion","digital_topnews","front")
nyt_share <- c("emailed","facebook","tweeted","viewed")




plot.STM(stm_res, type = "summary", custom.labels = topics, text.cex=.6)
plot.STM(stm_res, type = "summary", topics = topics_polecon
       , custom.labels = topics[topics_polecon], text.cex=.6)

plot.STM(stm_res, type = "perspective", topics = c(1,3))


prep <- estimateEffect(topics_polecon ~ emailed + facebook + front + tweeted + viewed +
                           digital_opinion + digital_topnews + digital_bottom
                     , stm_res, meta = out$meta, uncertainty = "Global")
par(mfrow = c(2,2))
plot.estimateEffect(prep, covariate = "front", model = stm_res, xlim = c(-.3,.3)
                  , method = "difference", cov.value1 = 1, cov.value2 = 0, main = "front"
                  , labeltype = "custom", custom.labels = topics[topics_polecon])
plot.estimateEffect(prep, covariate = "digital_topnews", model = stm_res, xlim = c(-.3,.3)
                  , method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "digital_topnews", labeltype = "custom"
                  , custom.labels = topics[topics_polecon])
plot.estimateEffect(prep, covariate = "digital_bottom", model = stm_res
                  , xlim = c(-.3,.3), method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "digital_bottom", labeltype = "custom"
                  , custom.labels = topics[topics_polecon])
plot.estimateEffect(prep, covariate = "digital_opinion", model = stm_res
                  , xlim = c(-.3,.3), method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "digital_opinion", labeltype = "custom"
                  , custom.labels = topics[topics_polecon])

par(mfrow = c(2,2))
plot.estimateEffect(prep, covariate = "emailed", model = stm_res
                  , xlim = c(-.3,.3), method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "emailed", labeltype = "custom", custom.labels = topics[topics_polecon])
plot.estimateEffect(prep, covariate = "facebook", model = stm_res
                  , xlim = c(-.3,.3), method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "facebook", labeltype = "custom", custom.labels = topics[topics_polecon])
plot.estimateEffect(prep, covariate = "tweeted", model = stm_res
                  , xlim = c(-.3,.3), method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "tweeted", labeltype = "custom", custom.labels = topics[topics_polecon])
plot.estimateEffect(prep, covariate = "viewed", model = stm_res
                  , xlim = c(-.3,.3), method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "viewed", labeltype = "custom", custom.labels = topics[topics_polecon])


### look at topic proportions for each category over time
topic_select <- apply(stm_res$theta, 1, function(x) which(x == max(x)))
topic_select <- topic_select %in% topics_polecon
topic_select <- nyt_reduced$title[topic_select == T]

nyt_series <- nyt_combined %>% select(date, title, type) %>%
    left_join(bind_cols(select(nyt_reduced, title), data.frame(stm_res$theta))) %>%
    na.omit() %>% group_by(date, type) %>% select(-title) %>% summarize_each(funs(mean)) %>%
    gather("topic","proportion",3:ncol(.))
nyt_series$topic <- as.numeric(gsub("X","",nyt_series$topic))
nyt_series$topic <- factor(nyt_series$topic, labels = topics)

ggplot(filter(nyt_series, topic %in% topics[topics_polecon] & type %in% nyt_part)
       , aes(x = date, y = proportion, col = topic)) + geom_line() + facet_wrap(~type) +
  theme_bw() + theme(legend.position = "bottom")

ggplot(filter(nyt_series, topic %in% topics[topics_polecon] & type %in% nyt_share)
       , aes(x = date, y = proportion, col = topic)) + geom_line() + facet_wrap(~type) +
  theme_bw() + theme(legend.position = "bottom")

ggplot(filter(nyt_series, topic %in% c("Presidential Race", "Legal/Court", "Police") & type %in% nyt_part)
       , aes(x = date, y = proportion, col = topic)) + geom_line() + facet_wrap(~type) +
  theme_bw() + theme(legend.position = "bottom")

ggplot(filter(nyt_series, topic %in% c("Presidential Race", "Legal/Court", "Police") & type %in% nyt_share)
       , aes(x = date, y = proportion, col = topic)) + geom_line() + facet_wrap(~type) +
  theme_bw() + theme(legend.position = "bottom")



### complexity by category
ci <- function(x){
    mu <- mean(x, na.rm = T)
    se <- sd(x, na.rm = T)/sqrt(length(na.omit(x)))
    ci_lo <- mu - 1.96 * se
    ci_hi <- mu + 1.96 * se
    out <- c(mu, ci_lo, ci_hi)
    return(out)
}

test <- nyt_reduced %>% mutate(readab = nyt_readab) %>% right_join(nyt_polecon) %>% data.frame()
readab_summary <- data.frame(NULL)
for(i in c("emailed", "facebook", "front", "tweeted", "viewed", "digital_opinion"
         , "digital_topnews", "digital_bottom")){
    tmp <- data.frame(t(ci(test[which(test[i] == 1), "readab"])))
    tmp$variable = i
    readab_summary <- rbind(readab_summary, tmp)
}
colnames(readab_summary)[1:3] <- c("mean","cilo","cihi")

ggplot(readab_summary, aes(y = mean, ymin = cilo, ymax = cihi, x = variable)) + geom_pointrange() + theme_bw() + coord_flip()




### switches between categories

nyt_switch <- nyt_combined %>% filter(title %in% topic_select) %>% select(date, title, type) %>%
    filter(title %in% unique(title[duplicated(title)])) %>% arrange(title, date) %>%
    group_by(title) %>% mutate(day = as.numeric(date - min(date))) %>% unique()
tmp <- nyt_switch %>% group_by(title) %>% summarize(maxday = max(day)) %>% filter(maxday==0)
nyt_switch <- nyt_switch %>% filter(!title %in% tmp$title) %>% count(day, type)
tmp <- nyt_switch %>% group_by(day) %>% summarize(total = sum(n))
nyt_switch <- nyt_switch %>% left_join(tmp) %>% mutate(prop = n/total) %>% filter(day <=5)

ggplot(nyt_switch, aes(x = day, y = prop, col = type)) + geom_line() + 
    theme_bw() + theme(legend.position = "bottom")


