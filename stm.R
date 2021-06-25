#install.packages("stm")
#install.packages("quanteda")
#install.packages("lubridate")

library(stm)
library(quanteda)
library(lubridate)


bearer_token <- ...

texts2$days <- day(texts2$created_at)

texts2$US <- factor(texts2$US,
                    levels = c(0,1),
                    labels = c("UK", "US"))


texts2$threshold <- factor(texts2$threshold,
                           levels = c(0,1),
                           labels = c("Before", "After"))

mycorpus <- corpus(texts2)

token <-
  tokens(
    mycorpus,
    remove_numbers = TRUE,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_url = TRUE,
    split_hyphens = TRUE,
    include_docvars = TRUE)

token_ungd1 <- tokens_select(
  token,
  c("[\\d-]", "[[:punct:]]", "^.{1,2}$"),
  selection = "remove",
  valuetype = "regex",
  verbose = TRUE)

token_ungd2 <- tokens_remove(
  token_ungd1,
  pattern = stopwords(source="smart"))

token_ungd3 <- tokens_tolower(
  token_ungd2, 
  keep_acronyms = FALSE)

token_ungd4 <- tokens_wordstem(
  token_ungd3, 
  language = "en")

mydfm <- dfm(token_ungd4)

mydfm.trim <-
  dfm_trim(
    mydfm,
    min_docfreq = 0.001, 
    max_docfreq = 0.25,
    docfreq_type = "prop")

library(stm)
# Assigns the number of topics
topic.count <- 9

# Convert DFM to a STM object
dfm2stm <- convert(mydfm.trim, to = "stm")
# Run the topic model
model.stm <- stm(dfm2stm$documents, dfm2stm$vocab,
                 K = topic.count,
                 prevalence = ~ US + threshold + s(days),
                 data = dfm2stm$meta,
                 init.type = "Spectral")

