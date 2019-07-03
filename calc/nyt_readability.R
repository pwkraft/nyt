#################################################
# Readability of all articles (including non-political)
#################################################
## NOTE: this is just a quick check based on my old code, could be much more efficient...

library(tidyverse)
library(quanteda)

## load data
load("in/nyt_reduced.Rdata")
load("in/nyt_readab.Rdata")

## separate metadata
nyt_part <- c("Front Page","Opinion (Digital Edition)"
              ,"Top News (Digital Edition)","Bottom Part (Digital Edition)")
nyt_share <- c("Most Viewed","Shared on Facebook","Most Emailed","Tweeted")
nyt_part_var <- c("front","digital_opinion","digital_topnews","digital_bottom")
nyt_share_var <- c("viewed","facebook","emailed","tweeted")

ci <- function(x){
  mu <- mean(x, na.rm = T)
  se <- sd(x, na.rm = T)/sqrt(length(na.omit(x)))
  ci_lo <- mu - 1.96 * se
  ci_hi <- mu + 1.96 * se
  out <- c(mu, ci_lo, ci_hi)
  return(out)
}

readab <- nyt_reduced %>% 
  mutate(readab = nyt_readab) %>%
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

p <- ggplot(readab_summary, aes(y = mean, ymin = cilo, ymax = cihi, x = variable)) + 
  theme_classic() + geom_pointrange() + facet_wrap(~group, scales="free_x") + 
  ylab("Flesch-Kincaid Grade Level") + xlab(NULL) + 
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
p + theme(panel.border = element_rect(fill=NA))
ggsave("fig/readability.png",height=5,width=9)

