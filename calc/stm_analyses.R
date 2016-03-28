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
library(gridExtra)

## load data
load("in/nyt_combined.Rdata")
load("in/nyt_reduced.Rdata")
load("in/nyt_polecon.Rdata")
load("in/nyt_readab.Rdata")
load("in/stm_res.Rdata")

## separate metadata
nyt_part <- c("Front Page","Opinion (Digital Edition)"
              ,"Top News (Digital Edition)","Bottom Part (Digital Edition)")
nyt_share <- c("Most Viewed","Shared on Facebook","Most Emailed","Tweeted")
nyt_part_var <- c("front","digital_opinion","digital_topnews","digital_bottom")
nyt_share_var <- c("viewed","facebook","emailed","tweeted")


### basic summaries / overview

## indicative words for each topic (highest probability)
pdf("fig/words.pdf")
par(mfcol = c(2,2), cex = .5, mar=rep(0,4))
plot.STM(stm_res, type = "labels", topics = 1:5)
plot.STM(stm_res, type = "labels", topics = 6:10)
plot.STM(stm_res, type = "labels", topics = 11:15)
plot.STM(stm_res, type = "labels", topics = 16:20)
dev.off()
par(mfcol = c(1,1), cex = 1, mar = c(5, 4, 4, 2) + 0.1, mgp = c(3,1,0))

## proportions of topics (all)
pdf("fig/prop.pdf", height = 4)
par(mar = c(5, 2, 2, 2) + 0.1)
plot.STM(stm_res, type = "summary", custom.labels = topics, text.cex=.7, main = NA)
dev.off()

## proportion of topics (polecon)
pdf("fig/prop_polecon.pdf", height = 4)
par(mar = c(5, 2, 2, 2) + 0.1)
plot.STM(stm_res, type = "summary", topics = topics_polecon
         , custom.labels = topics[topics_polecon], text.cex=.7, main = NA)
dev.off()

## example for topic differences
pdf("fig/perspective.pdf", height = 4)
par(mar = c(1, 1, 1, 1) + 0.1)
plot.STM(stm_res, type = "perspective", topics = c(1,3), custom.labels = topics[c(1,3)])
dev.off()


### estimate effect of meta-information (all topics)

## model estimation
prep <- estimateEffect(1:20 ~ emailed + facebook + front + tweeted + viewed +
                           digital_opinion + digital_topnews + digital_bottom
                     , stm_res, meta = out$meta, uncertainty = "Global")

## plot results
pdf("fig/res_nyt.pdf", height = 5)
par(mfcol = c(2,2), mar = c(0, 3, 3, 1), mgp = c(1,1,0), cex=.6)
plot.estimateEffect(prep, covariate = "front", model = stm_res, xlim = c(-.25,.25)
                    , ylab = "Front Page", method = "difference", cov.value1 = 1, cov.value2 = 0
                    , labeltype = "custom", custom.labels = topics, xaxt="n")
plot.estimateEffect(prep, covariate = "digital_topnews", model = stm_res, xlim = c(-.25,.25)
                    , method = "difference", cov.value1 = 1, cov.value2 = 0
                    , ylab = "Top News (Digital Edition)", labeltype = "custom"
                    , custom.labels = topics, xaxt="n")
par(mar = c(3, 3, 0, 1), mgp = c(1,1,0))
plot.estimateEffect(prep, covariate = "digital_opinion", model = stm_res
                    , xlim = c(-.25,.25), method = "difference", cov.value1 = 1, cov.value2 = 0
                    , ylab = "Opinion (Digital Edition)", labeltype = "custom"
                    , custom.labels = topics)
plot.estimateEffect(prep, covariate = "digital_bottom", model = stm_res
                    , xlim = c(-.25,.25), method = "difference", cov.value1 = 1, cov.value2 = 0
                    , ylab = "Bottom Part (Digital Edition)", labeltype = "custom"
                    , custom.labels = topics)
dev.off()

pdf("fig/res_share.pdf", height = 5)
par(mfrow = c(2,2), mar = c(0, 3, 3, 1), mgp = c(1,1,0), cex=.6)
plot.estimateEffect(prep, covariate = "viewed", model = stm_res, xlim = c(-.25,.25)
                    , ylab = "Most Viewed", method = "difference", cov.value1 = 1, cov.value2 = 0
                    , labeltype = "custom", custom.labels = topics, xaxt="n")
plot.estimateEffect(prep, covariate = "facebook", model = stm_res
                    , xlim = c(-.25,.25), method = "difference", cov.value1 = 1, cov.value2 = 0
                    , ylab = "Shared on Facebook", labeltype = "custom"
                    , custom.labels = topics, xaxt="n")
par(mar = c(3, 3, 0, 1), mgp = c(1,1,0))
plot.estimateEffect(prep, covariate = "emailed", model = stm_res, xlim = c(-.25,.25)
                    , method = "difference", cov.value1 = 1, cov.value2 = 0
                    , ylab = "Emailed", labeltype = "custom"
                    , custom.labels = topics)
plot.estimateEffect(prep, covariate = "tweeted", model = stm_res
                    , xlim = c(-.25,.25), method = "difference", cov.value1 = 1, cov.value2 = 0
                    , ylab = "Tweeted", labeltype = "custom"
                    , custom.labels = topics)
dev.off()


### estimate effect of meta-information (polecon)

## model estimation
prep <- estimateEffect(topics_polecon ~ emailed + facebook + front + tweeted + viewed +
                         digital_opinion + digital_topnews + digital_bottom
                       , stm_res, meta = out$meta, uncertainty = "Global")

## plot results
pdf("fig/res_nyt_polecon.pdf", height = 4)
par(mfrow = c(2,2), mar = c(0, 3, 3, 1), mgp = c(1,1,0), cex=.6)
plot.estimateEffect(prep, covariate = "front", model = stm_res, xlim = c(-.25,.25)
                    , ylab = "Front Page", method = "difference", cov.value1 = 1, cov.value2 = 0
                    , labeltype = "custom", custom.labels = topics[topics_polecon], xaxt="n")
plot.estimateEffect(prep, covariate = "digital_opinion", model = stm_res
                    , xlim = c(-.25,.25), method = "difference", cov.value1 = 1, cov.value2 = 0
                    , ylab = "Opinion (Digital Edition)", labeltype = "custom"
                    , custom.labels = topics[topics_polecon], xaxt = "n")
par(mar = c(3, 3, 0, 1), mgp = c(1,1,0))
plot.estimateEffect(prep, covariate = "digital_topnews", model = stm_res, xlim = c(-.25,.25)
                    , method = "difference", cov.value1 = 1, cov.value2 = 0
                    , ylab = "Top News (Digital Edition)", labeltype = "custom"
                    , custom.labels = topics[topics_polecon])
plot.estimateEffect(prep, covariate = "digital_bottom", model = stm_res
                    , xlim = c(-.25,.25), method = "difference", cov.value1 = 1, cov.value2 = 0
                    , ylab = "Bottom Part (Digital Edition)", labeltype = "custom"
                    , custom.labels = topics[topics_polecon])
dev.off()

pdf("fig/res_share_polecon.pdf", height = 4)
par(mfrow = c(2,2), mar = c(0, 3, 3, 1), mgp = c(1,1,0), cex=.6)
plot.estimateEffect(prep, covariate = "viewed", model = stm_res, xlim = c(-.25,.25)
                    , ylab = "Most Viewed", method = "difference", cov.value1 = 1, cov.value2 = 0
                    , labeltype = "custom", custom.labels = topics[topics_polecon], xaxt="n")
plot.estimateEffect(prep, covariate = "facebook", model = stm_res
                    , xlim = c(-.25,.25), method = "difference", cov.value1 = 1, cov.value2 = 0
                    , ylab = "Shared on Facebook", labeltype = "custom"
                    , custom.labels = topics[topics_polecon], xaxt="n")
par(mar = c(3, 3, 0, 1), mgp = c(1,1,0))
plot.estimateEffect(prep, covariate = "emailed", model = stm_res, xlim = c(-.25,.25)
                    , method = "difference", cov.value1 = 1, cov.value2 = 0
                    , ylab = "Emailed", labeltype = "custom"
                    , custom.labels = topics[topics_polecon])
plot.estimateEffect(prep, covariate = "tweeted", model = stm_res
                    , xlim = c(-.25,.25), method = "difference", cov.value1 = 1, cov.value2 = 0
                    , ylab = "Tweeted", labeltype = "custom"
                    , custom.labels = topics[topics_polecon])
dev.off()


### topic proportions for each category over time

## prepare data
nyt_series <- nyt_combined %>% select(date, title, type) %>%
    left_join(bind_cols(select(nyt_reduced, title), data.frame(stm_res$theta))) %>%
    na.omit() %>% group_by(date, type) %>% select(-title) %>% summarize_each(funs(mean)) %>%
    gather("topic","proportion",3:ncol(.))
nyt_series$type <- factor(nyt_series$type, levels = c(nyt_part_var,nyt_share_var)
                          , labels = c(nyt_part,nyt_share))
nyt_series$topic <- as.numeric(gsub("X","",nyt_series$topic))
nyt_series$topic <- factor(nyt_series$topic, labels = topics)

## polecon
ggplot(filter(nyt_series, topic %in% topics[topics_polecon] & type %in% nyt_part)
       , aes(x = date, y = proportion, col = topic)) + geom_line() + facet_wrap(~type) +
  theme_classic() + theme(panel.border = element_rect(fill=NA)) + theme(legend.position = "bottom") + 
  ylab("Proportion") + xlab("Date") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + scale_color_discrete(name = "Topic")
ggsave("fig/series_nyt.pdf", height = 5)

ggplot(filter(nyt_series, topic %in% topics[topics_polecon] & type %in% nyt_share)
       , aes(x = date, y = proportion, col = topic)) + geom_line() + facet_wrap(~type) + 
  theme_classic() + theme(panel.border = element_rect(fill=NA)) +
  theme(legend.position = "bottom") + ylab("Proportion") + xlab("Date") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + scale_color_discrete(name = "Topic")
ggsave("fig/series_share.pdf", height = 5)

## 3 main topics
ggplot(filter(nyt_series, topic %in% c("Presidential Race", "Legal/Court", "Police") & 
                type %in% nyt_part), aes(x = date, y = proportion, col = topic, lty = topic)) + 
  geom_line() + facet_wrap(~type) + theme_classic() + theme(panel.border = element_rect(fill=NA)) + 
  theme(legend.position = "bottom") + ylab("Proportion") + xlab("Date") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  scale_color_discrete(name = "Topic") +  scale_linetype_discrete(name = "Topic")
ggsave("fig/series_nyt_main.pdf", height = 5)

ggplot(filter(nyt_series, topic %in% c("Presidential Race", "Legal/Court", "Police") & 
                type %in% nyt_share), aes(x = date, y = proportion, col = topic, lty = topic)) + 
  geom_line() + facet_wrap(~type) + theme_classic() + theme(panel.border = element_rect(fill=NA)) + 
  theme(legend.position = "bottom") + ylab("Proportion") + xlab("Date") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  scale_color_discrete(name = "Topic") +  scale_linetype_discrete(name = "Topic")
ggsave("fig/series_share_main.pdf", height = 5)


### complexity by category
ci <- function(x){
    mu <- mean(x, na.rm = T)
    se <- sd(x, na.rm = T)/sqrt(length(na.omit(x)))
    ci_lo <- mu - 1.96 * se
    ci_hi <- mu + 1.96 * se
    out <- c(mu, ci_lo, ci_hi)
    return(out)
}

readab <- nyt_reduced %>% mutate(readab = nyt_readab) %>% filter(topic_pred %in% topics_polecon) %>% 
  data.frame()
readab_summary <- data.frame(NULL)
for(i in c(nyt_part_var,nyt_share_var)){
    tmp <- data.frame(t(ci(readab[which(readab[i] == 1), "readab"])))
    tmp$variable = i
    readab_summary <- rbind(readab_summary, tmp)
}
colnames(readab_summary)[1:3] <- c("mean","cilo","cihi")
readab_summary$variable <- factor(readab_summary$variable
                                  , levels = c(nyt_share_var[c(1,3,2,4)],nyt_part_var[c(1,3,2,4)])
                                  , labels = c(nyt_share[c(1,3,2,4)],nyt_part[c(1,3,2,4)]))
readab_summary$group <- rep(c("Newspaper section","Shared/Viewed"),each=4)

ggplot(readab_summary, aes(y = mean, ymin = cilo, ymax = cihi, x = variable)) + 
  theme_classic() + theme(panel.border = element_rect(fill=NA)) +
  geom_pointrange() + facet_wrap(~group, scales="free_x") + 
  ylab("Flesch-Kincaid Grade Level") + xlab(NULL) + 
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
ggsave("fig/readability.pdf",height=5)


### switches between categories (polecon)

## prepare data
nyt_switch <- nyt_combined %>% filter(title %in% nyt_polecon$title) %>% select(date, title, type) %>%
    filter(title %in% unique(title[duplicated(title)])) %>% arrange(title, date) %>%
    group_by(title) %>% mutate(day = as.numeric(date - min(date))) %>% unique()
tmp <- nyt_switch %>% group_by(title) %>% summarize(maxday = max(day)) %>% filter(maxday==0)
nyt_switch <- nyt_switch %>% filter(!title %in% tmp$title) %>% count(day, type)
tmp <- nyt_switch %>% group_by(day) %>% summarize(total = sum(n))
nyt_switch <- nyt_switch %>% left_join(tmp) %>% mutate(prop = n/total) %>% filter(day <=5)
nyt_switch$type <- factor(nyt_switch$type, levels = c(nyt_part_var,nyt_share_var)
                                          , labels = c(nyt_part,nyt_share))
nyt_switch$group <- factor(nyt_switch$type %in% nyt_share
                           , labels = c("Newspaper section","Shared/Viewed"))
  
## create plot
ggplot(nyt_switch, aes(x = day, y = prop, col = type, lty=type)) + geom_line() + theme_classic() + 
  theme(panel.border = element_rect(fill=NA)) + theme(legend.position = "bottom") + 
  scale_color_discrete(name = NULL) +  scale_linetype_discrete(name = NULL) + 
  ylab("Proportion") + xlab("Day") + facet_wrap(~group)
ggsave("fig/switch.pdf")
  


