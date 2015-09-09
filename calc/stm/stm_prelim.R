#################################################
# Preliminary analyses of nytimes articles (stm)
#################################################

rm(list=ls())
setwd("/data/Uni/projects/2015/nyt/calc/scraping")
load("../in/nyt_combined.Rdata")
library(stm)


### reduce dataset to single observations for each article

nyt_combined <- nyt_combined[order(nyt_combined$id),]
nyt_combined$select <- c(1,diff(nyt_combined$id))
nyt_reduced <- nyt_combined[nyt_combined$select == 1
                          , !is.element(colnames(nyt_combined), c("uniqueid","type","select"))]


### add metadata to reduced dataset

meta <- data.frame(model.matrix(~ type, nyt_combined)[,-1])
colnames(meta) <- gsub("type","",colnames(meta))
meta$digital <- as.numeric(apply(meta,1,sum) == 0)
nyt_combined <- cbind(nyt_combined, meta)
meta <- aggregate(cbind(emailed,facebook,front,tweeted,viewed,digital) ~ id
                , data = nyt_combined, function(x) as.numeric(sum(x) > 0))
nyt_reduced <- merge(nyt_reduced, meta)
rm(meta)


### stm analyses of unique articles

processed <- textProcessor(nyt_reduced$text, metadata = nyt_reduced[c("id"
                           ,"emailed","facebook","front","tweeted","viewed","digital")])
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 15)
length(out$vocab) # vocabulary is almost too long for "Spectral" analyses
test <- stm(out$documents, out$vocab, K = 20
          , prevalence =~ emailed + facebook + front + tweeted + viewed + digital
          , max.em.its = 75, data = out$meta, init.type = "Spectral")
save.image("../in/test.Rdata")
#load("../in/test.Rdata")


### summarize results

# explore words associated with each topic
labelTopics(test)
plot.STM(test, type = "summary", xlim = c(0, .3))
cloud(test, topic = 1, scale = c(2,.25))

# topic correlations
plot.topicCorr(topicCorr(test))


# estimate effects
prep <- estimateEffect(1:20 ~ emailed + facebook + front + tweeted + viewed + digital
                     , test, meta = out$meta, uncertainty = "Global")
plot.estimateEffect(prep, covariate = "emailed", topics = 1:20, model = test, xlim = c(-0.1,0.1)
                  , method = "difference", cov.value1 = 1, cov.value2 = 0, main = "emailed")
plot.estimateEffect(prep, covariate = "facebook", topics = 1:20, model = test, xlim = c(-0.1,0.1)
                  , method = "difference", cov.value1 = 1, cov.value2 = 0, main = "facebook")
plot.estimateEffect(prep, covariate = "front", topics = 1:20, model = test, xlim = c(-0.1,0.1)
                  , method = "difference", cov.value1 = 1, cov.value2 = 0, main = "front")
plot.estimateEffect(prep, covariate = "tweeted", topics = 1:20, model = test, xlim = c(-0.1,0.1)
                  , method = "difference", cov.value1 = 1, cov.value2 = 0, main = "tweeted")
plot.estimateEffect(prep, covariate = "viewed", topics = 1:20, model = test, xlim = c(-0.1,0.1)
                  , method = "difference", cov.value1 = 1, cov.value2 = 0, main = "viewed")
plot.estimateEffect(prep, covariate = "digital", topics = 1:20, model = test, xlim = c(-0.1,0.1)
                  , method = "difference", cov.value1 = 1, cov.value2 = 0, main = "digital")








