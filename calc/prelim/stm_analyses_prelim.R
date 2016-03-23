#################################################
# Preliminary analyses of nytimes articles (stm)
#################################################


rm(list=ls())
setwd("/data/Uni/projects/2015/nyt/calc")
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
load("in/stm_select.Rdata")
load("in/stm_polecon.Rdata")


### summarize selection
labelTopics(stm_select)

### summarize results
labelTopics(stm_polecon)
topics <- c("Presidential Race","Technology","International","Supreme Court/Laws","Police","Religion (unclear)","Iran/Israel","Health/Care","Sport","Economy")
plot.STM(stm_polecon, type = "summary", custom.labels = topics)
plot.STM(stm_polecon, type = "perspectives", topics=c(1,3))

topic_polecon <- apply(stm_polecon$theta, 1, function(x) which(x == max(x)))
View(nyt_polecon[topic_polecon == 6,])



### topic proportions in each category
prep <- estimateEffect(1:10 ~ emailed + facebook + front + tweeted + viewed +
                           digital_opinion + digital_topnews + digital_bottom
                     , stm_polecon, meta = out_polecon$meta, uncertainty = "Global")
par(mfrow = c(2,4))
plot.estimateEffect(prep, covariate = "emailed", topics = 1:10, model = stm_polecon
                  , xlim = c(-.3,.3), method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "emailed", labeltype = "custom", custom.labels = topics)
plot.estimateEffect(prep, covariate = "facebook", topics = 1:10, model = stm_polecon
                  , xlim = c(-.3,.3), method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "facebook", labeltype = "custom", custom.labels = topics)
plot.estimateEffect(prep, covariate = "front", topics = 1:10, model = stm_polecon
                  , xlim = c(-.3,.3), method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "front", labeltype = "custom", custom.labels = topics)
plot.estimateEffect(prep, covariate = "tweeted", topics = 1:10, model = stm_polecon
                  , xlim = c(-.3,.3), method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "tweeted", labeltype = "custom", custom.labels = topics)
plot.estimateEffect(prep, covariate = "viewed", topics = 1:10, model = stm_polecon
                  , xlim = c(-.3,.3), method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "viewed", labeltype = "custom", custom.labels = topics)
plot.estimateEffect(prep, covariate = "digital_opinion", topics = 1:10, model = stm_polecon
                  , xlim = c(-.3,.3), method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "digital_opinion", labeltype = "custom", custom.labels = topics)
plot.estimateEffect(prep, covariate = "digital_topnews", topics = 1:10, model = stm_polecon
                  , xlim = c(-.3,.3), method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "digital_topnews", labeltype = "custom", custom.labels = topics)
plot.estimateEffect(prep, covariate = "digital_bottom", topics = 1:10, model = stm_polecon
                  , xlim = c(-.3,.3), method = "difference", cov.value1 = 1, cov.value2 = 0
                  , main = "digital_bottom", labeltype = "custom", custom.labels = topics)
par(mfrow = c(1,1))


### complexity by category
ci <- function(x){
    mu <- mean(x, na.rm = T)
    se <- sd(x, na.rm = T)/sqrt(length(na.omit(x)))
    ci_lo <- mu - 1.96 * se
    ci_hi <- mu + 1.96 * se
    out <- c(mu, ci_lo, ci_hi)
    return(out)
}

test <- nyt_reduced %>% mutate(readab = nyt_readab$ARI) %>% right_join(nyt_polecon) %>% data.frame()
readab_summary <- data.frame(NULL)
for(i in c("emailed", "facebook", "front", "tweeted", "viewed", "digital_opinion"
         , "digital_topnews", "digital_bottom")){
    tmp <- data.frame(t(ci(test[which(test[i] == 1), "readab"])))
    tmp$variable = i
    readab_summary <- rbind(readab_summary, tmp)
}
colnames(readab_summary)[1:3] <- c("mean","cilo","cihi")

ggplot(readab_summary, aes(y = mean, ymin = cilo, ymax = cihi, x = variable)) + geom_pointrange()


### look at topic proportions for each category over time
topic_select <- apply(stm_select$theta, 1, function(x) which(x == max(x)))
topic_select <- topic_select == 1 | topic_select == 3 | topic_select == 5
topic_select <- nyt_reduced$title[topic_select == T]

nyt_series <- nyt_combined %>% filter(title %in% topic_select) %>% select(date, title, type) %>%
    left_join(bind_cols(select(nyt_polecon, title), data.frame(stm_polecon$theta))) %>%
    group_by(date, type) %>% select(-title) %>% summarize_each(funs(mean)) %>%
    gather("topic","proportion",3:12)
nyt_series$topic <- factor(nyt_series$topic, labels = topics)

ggplot(nyt_series, aes(x = date, y = proportion, col = topic)) + geom_line() + facet_wrap(~type)


### switches between categories

nyt_switch <- nyt_combined %>% filter(title %in% topic_select) %>% select(date, title, type) %>%
    filter(title %in% unique(title[duplicated(title)])) %>% arrange(title, date) %>%
    group_by(title) %>% mutate(day = as.numeric(date - min(date))) %>% unique()
tmp <- nyt_switch %>% group_by(title) %>% summarize(maxday = max(day)) %>% filter(maxday==0)
nyt_switch <- nyt_switch %>% filter(!title %in% tmp$title) %>% count(day, type)
tmp <- nyt_switch %>% group_by(day) %>% summarize(total = sum(n))
nyt_switch <- nyt_switch %>% left_join(tmp) %>% mutate(prop = n/total) %>% filter(day <=5)

ggplot(nyt_switch, aes(x = day, y = prop, col = type)) + geom_line()


