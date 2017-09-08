library(quanteda)
library(topicmodels)
library(tidytext)

#opens connections
connection_en_US_twitter <- file("final/en_US/en_US.twitter.txt")
connection_en_US_blogs <- file("final/en_US/en_US.blogs.txt")
connection_en_US_news <- file("final/en_US/en_US.news.txt")

#retrieve text files data
data_en_US_twitter <- readLines(connection_en_US_twitter, skipNul = TRUE)
data_en_US_blogs <- readLines(connection_en_US_blogs, skipNul = TRUE)
data_en_US_news <- readLines(connection_en_US_news, skipNul = TRUE)

#close connections
closeAllConnections()

#counts number of lines
lines_twitter <- length(data_en_US_twitter)
lines_blogs <- length(data_en_US_blogs)
lines_news <- length(data_en_US_news)

#sample-training set
n <- 0.30
seed <- 4112
set.seed(seed)
train_twitter <- data_en_US_twitter[rbinom(lines_twitter*n, lines_twitter, 0.5)]
train_twitter <- paste(train_twitter, collapse = " ")
set.seed(seed)
train_blogs <- data_en_US_blogs[rbinom(lines_blogs*n, lines_blogs, 0.5)]
train_blogs <- paste(train_blogs, collapse = " ")
set.seed(seed)
train_news <- data_en_US_news[rbinom(lines_news*n, lines_news, 0.5)]
train_news <- paste(train_news, collapse = " ")
train_generic <- c(train_twitter, train_blogs, train_news)

#create corpus
corpus <- corpus(train_generic, docnames=c("twitter","blogs","news"))

#dfm corpus
#corpus_unigram <- dfm(corpus, what="word", tolower=TRUE, remove_numbers=TRUE, 
#                      remove_punct=TRUE, remove_symbols=TRUE, remove_twitter=TRUE, 
#                      concatenator=" ")
#corpus_unigram <- dfm_trim(corpus_unigram, min_count=2)
#corpus_dictionary <- featnames(corpus_unigram)
#saveRDS(corpus_dictionary, "corpus_dictionary.rds")

#creates results for fourgram
k <- 10
fourgram <- dfm(corpus, what="word", tolower=TRUE, remove_numbers=TRUE, remove_punct=TRUE, 
                remove_symbols=TRUE, remove_twitter=TRUE, ngrams=4, concatenator=" ")
fourgram <- dfm_trim(fourgram, sparsity = 0.66)
saveRDS(fourgram, file = "fourgram.rds")
fourgram_lda <- LDA(fourgram, k=k, control = list(seed=seed))
fourgram_lda_tibble <- tidy(fourgram_lda, matrix = "beta")
fourgram_lda_tibble <- fourgram_lda_tibble[order(-fourgram_lda_tibble$beta, fourgram_lda_tibble$term),]
saveRDS(fourgram_lda, file = "fourgram_lda.rds")
saveRDS(fourgram_lda_tibble, file = "fourgram_lda_tibble.rds")
#creates results for trigram
trigram <- dfm(corpus, what="word", tolower=TRUE, remove_numbers=TRUE, remove_punct=TRUE, 
              remove_symbols=TRUE, remove_twitter=TRUE, ngrams=3, concatenator=" ")
trigram <- dfm_trim(trigram, sparsity = 0.64)
saveRDS(trigram, file = "trigram.rds")
trigram_lda <- LDA(trigram, k=k, control = list(seed=seed))
trigram_lda_tibble <- tidy(trigram_lda, matrix = "beta")
trigram_lda_tibble <- trigram_lda_tibble[order(-trigram_lda_tibble$beta, trigram_lda_tibble$term),]
saveRDS(trigram_lda, file = "trigram_lda.rds")
saveRDS(trigram_lda_tibble, file = "trigram_lda_tibble.rds")
#creates results for bigram
bigram <- dfm(corpus, what="word", tolower=TRUE, remove_numbers=TRUE, remove_punct=TRUE, 
              remove_symbols=TRUE, remove_twitter=TRUE, ngrams=2, concatenator=" ")
bigram <- dfm_trim(bigram, sparsity=0.63)
saveRDS(bigram, file = "bigram.rds")
bigram_lda <- LDA(bigram, k=k, control = list(seed=seed))
bigram_lda_tibble <- tidy(bigram_lda, matrix = "beta")
bigram_lda_tibble <- bigram_lda_tibble[order(-bigram_lda_tibble$beta, bigram_lda_tibble$term),]
saveRDS(bigram_lda, file = "bigram_lda.rds")
saveRDS(bigram_lda_tibble, file = "bigram_lda_tibble.rds")