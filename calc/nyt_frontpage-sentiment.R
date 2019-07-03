#################################################
# Sentiment anlysis of front page articles
#################################################

library(tidyverse)
library(quanteda)

## load data
load("in/nyt_reduced.Rdata")

## select front page only from reduced dataset
front <- nyt_reduced %>% filter(front == 1)

## compare with full data to make sure nothing is missing
load("in/nyt_combined.Rdata")
tmp <- nyt_combined %>% filter(type == "front")
table(front$title %in% tmp$title)
table(tmp$title %in% front$title)
tmp[!tmp$title %in% front$title,]
rm(tmp, nyt_combined, nyt_reduced)

## pre-processing specific for lexicoder sentiment dictionary 
source("LSDprep_jan2018.R") # (code from lexicoder.com)
text <- front$text %>%
   LSDprep_contr() %>% # replace contractions
   LSDprep_dict_punct() %>% # remove false dict terms
   remove_punctuation_from_acronyms() %>% # optional
   remove_punctuation_from_abbreviations %>% # optional
   LSDprep_punctspace() %>% # insert spaces around punctuation
   LSDprep_negation() %>% # process negation
   LSDprep_dict() %>% # remove additional false dict terms
   mark_proper_nouns() # optional

## sentiment dictionary count
lsd <- dfm(text, 
           remove_numbers = TRUE, 
           remove_punct = TRUE,
           remove_symbols = TRUE,
           dictionary = data_dictionary_LSD2015) %>%
  convert(to = "data.frame") %>%
  mutate(total = ntoken(text,
                        remove_numbers = TRUE, 
                        remove_punct = TRUE,
                        remove_symbols = TRUE),
         tone = 100 * (positive - neg_positive - (negative - neg_negative))/total)

## plots
lsd %>% summarize(mean = mean(tone),
                  se = sd(tone)/sqrt(n())) %>%
  ggplot(aes(y = mean, 
             ymin = mean - 1.96*se, 
             ymax = mean + 1.96*se, 
             x = "Front Page")) +
  geom_point() + 
  geom_pointrange() +
  theme_minimal() +
  scale_y_continuous(limits = c(-.8, -.1), 
                     breaks = seq(-.8, -.1, by = .1)) +
  labs(y = "Mean Net Sentiment", x = "Platform")
ggsave("fig/sent_mean.png", height = 3, width = 4)
  
ggplot(lsd, aes(x = tone)) +
  geom_density() +
  theme_minimal() +
  geom_vline(xintercept = 0, lty = "dashed", color = "darkgrey") +
  geom_vline(xintercept = mean(lsd$tone), lty = "dotted") +
  labs(y = "Density", x = "Negative  <-->  Positive")
ggsave("fig/sent_density.png", height = 3, width = 4)
