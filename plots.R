### Plots

# Frequency of tweets over time
ts_plot(tw, by="hours") +
  labs(x = NULL, y = NULL,
       title = "Tweets Frequency on Palestine Change over Time",
       subtitle = paste0(format(min(tw$created_at), "%d %B %Y"), " to ", format(max(tw$created_at),"%d %B %Y")),
       caption = "Data Acquired using Twitter's API v2") +
  theme_minimal()


# Find Optimum Number of Topics
indingk <- searchK(dfm2stm$documents, 
                   dfm2stm$vocab, 
                   K = c(5:25),
                   prevalence =~ threshold + US, 
                   data = dfm2stm$meta, 
                   verbose=FALSE)
plot(indingk) #see diagnostics


## Topics

plot(model.stm)
plot(model.stm, type = "summary", xlim = c(0, .3))
plot(model.stm, type = "labels", xlim = c(0, .3))


## Relationship between topics and various covariates of interest

predict_topics <- estimateEffect(formula = 1:9 ~ threshold + US + s(days), 
                                 stmobj = model.stm, metadata = dfm2stm$meta, 
                                 un = "Global")

topicnames <- c("Humanitarian","Violence/Armed Conflict","Apolitical",
                "US Politics","Settlement","Antisemitism",
                "Ethnic","Activism","Religion")

par(mfrow=c(2,1), mai=c(0.9,0.3,0.6,0.3))

plot(predict_topics, covariate = "threshold", topics = 1:9,
     model = model.stm, method = "difference",
     cov.value1 = 1, cov.value2 = 0,
     xlab = "Before ........................ After",
     main = "Effect of Bombings and Rockets",
     xlim = c(-.05, .05), 
     labeltype = "custom",
     custom.labels = topicnames)

plot(predict_topics, covariate = "US", topics = 1:9,
     model = model.stm, method = "difference",
     cov.value1 = 0, cov.value2 = 1,
     xlab = "US .................. UK",
     main = "US vs UK",
     xlim = c(-.05, .05), 
     labeltype = "custom",
     custom.labels = topicnames)


## plot change in the prevalence of topic over time
par(mfrow=c(1,1), mai=c(1,1,1,1))

plot(predict_topics, "days", method = "continuous", topics = 1,
     model = z, xaxt = "n", xlab = "Days in May 2021",
     printlegend = FALSE)

dayseq <- seq(from = as.Date("2021-05-01"),
              to = as.Date("2021-05-31"), by = "day")
axis(1,at = as.numeric(dayseq) - min(as.numeric(dayseq)),
     labels = 1:31)
abline(v=c(5,9,20), col=c("blue","red","blue"), lty=c(1,1,1), lwd=c(1,2,1))


## 9 graphs together
par(mfrow=c(3,3), mai=c(0.6,0.3,0.4,0.3))

topicnum <- 1:9

for (i in topicnum) {
  plot(predict_topics, "days", method = "continuous", topics = i,
       model = z, xaxt = "n", 
       xlab = paste0("Topic ",i,": ",topicnames[i]), ylab = "",
       printlegend = FALSE)
  
  dayseq <- seq(from = as.Date("2021-05-01"),
                to = as.Date("2021-05-31"), by = "day")
  axis(1,at = as.numeric(dayseq) - min(as.numeric(dayseq)),
       labels = 1:31)
  abline(v=c(5,9,20), col=c("blue","red","blue"), lty=c(1,1,1), lwd=c(1,2,1))
}
mtext("Prevalence of Topics Over Time", side = 3, line = -2.3, outer = TRUE, cex=2,font=1)



## Topical Contents

## UK vs US

contentCountry <- stm(dfm2stm$documents, dfm2stm$vocab, K = 9,
                      prevalence =~ threshold + US + s(days), content =~ US,
                      max.em.its = 75, data = dfm2stm$meta, init.type = "Spectral")


par(mfrow=c(1,2), mai=c(0.5,0.5,1,0.5))

topicnum <- 2
plot(contentCountry, type = "perspectives", plabels = c("UK","US"), 
     topics = topicnum, main = paste0("Topic ",topicnum,": ",topicnames[topicnum]))

topicnum <- 9
plot(contentCountry, type = "perspectives", plabels = c("UK","US"), 
     topics = topicnum, main = paste0("Topic ",topicnum,": ",topicnames[topicnum]))



## Before vs After
contentBA <- stm(dfm2stm$documents, dfm2stm$vocab, K = 9,
                 prevalence =~ threshold + US + s(days), content =~ threshold,
                 max.em.its = 75, data = dfm2stm$meta, init.type = "Spectral")

topicnum <- 1
plot(contentBA, type = "perspectives", plabels = c("Before","After"), 
     topics = topicnum, main = paste0("Topic ",topicnum,": ",topicnames[topicnum]))


topicnum <- 2
plot(contentBA, type = "perspectives", plabels = c("Before","After"), 
     topics = topicnum, main = paste0("Topic ",topicnum,": ",topicnames[topicnum]))


par(mfrow=c(1,1), mai=c(1,1,1,1))


## Word Clouds

install.packages("quanteda.textplots")
library(quanteda.textplots)

textplot_wordcloud(mydfm, comparison = FALSE, max_words = 300, color = rev(RColorBrewer::brewer.pal(10, "RdBu")))

mydfm_groupedbyUS <- dfm_group(mydfm, groups = US)
mydfm_groupedbythreshold <- dfm_group(mydfm, groups = threshold)

textplot_wordcloud(mydfm_groupedbyUS, comparison = TRUE, max_words = 300, color = rev(RColorBrewer::brewer.pal(10, "RdBu")))
textplot_wordcloud(mydfm_groupedbythreshold, comparison = TRUE, max_words = 300, color = rev(RColorBrewer::brewer.pal(10, "RdBu")))


## Frequencey Plots

install.packages("quanteda.textstats")
library(quanteda.textstats)
library(ggplot2)

freq_weight <- textstat_frequency(mydfm.trim, n = 10, 
                                  groups = mydfm.trim$threshold)
#change threshold with US

ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")


result_keyness1 <- textstat_keyness(mydfm_groupedbyUS, target = 1)
textplot_keyness(result_keyness1)

result_keyness2 <- textstat_keyness(mydfm_groupedbythreshold, target = 1)
textplot_keyness(result_keyness2) 


install.packages("dplyr")
library(tidytext)
library(dplyr)
library(ggplot2)

model.stm_tidy <- tidy(model.stm, matrix = "beta")

model.stm_tidy_terms <- 
  model.stm_tidy %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

model.stm_tidy_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
