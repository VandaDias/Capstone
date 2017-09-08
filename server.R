library(shiny)
library(quanteda)
library(tidytext)
library(topicmodels)
library(dplyr)

shinyServer(function(input, output) {
  output$instructions <- renderText({
    paste("This application consists in predicting the next word of a given uncompleted sentence.",
          "If the model database includes the topic and last words of the given text, it results on a 1 to 3 expected words. If not, it will tell you it is not possible to predict.",
          "Instructions:",
          "1. In the Insert Text box, write down the uncompleted sentence (or text)",
          "2. Press submit",
          "3. The Expected Next Word box will show you 1 to 3 expected words to write next",
          "The process may take a few seconds...",
          sep = "\n")
  })
  output$nextwords <- renderText({
    text <- input$text
    nextword <- NULL
    if(text == "") {
      paste("")
    } else {
    token_text <- tokenize(text, what="sentence")
    sentence <- token_text[[1]][length(token_text[[1]])]
    sentence_lowercase <- char_tolower(sentence)
    tokens_word <- tokenize(sentence_lowercase, what = "word", remove_numbers=TRUE, remove_punct=TRUE,
                            remove_symbols=TRUE, remove_twitter=TRUE)
    t <- length(tokens_word[[1]])
    if(t > 3) {
      word4 <- paste(tokens_word[[1]][t-3], tokens_word[[1]][t-2], tokens_word[[1]][t-1], tokens_word[[1]][t])
      word3 <- paste(tokens_word[[1]][t-2], tokens_word[[1]][t-1], tokens_word[[1]][t])
      word2 <- paste(tokens_word[[1]][t-1], tokens_word[[1]][t])
      word1 <- paste(tokens_word[[1]][t])
    } else if(t == 3) {
      word4 <- ""
      word3 <- paste(tokens_word[[1]][t-2], tokens_word[[1]][t-1], tokens_word[[1]][t])
      word2 <- paste(tokens_word[[1]][t-1], tokens_word[[1]][t])
      word1 <- paste(tokens_word[[1]][t])
    } else if(t == 2) {
      word4 <- ""
      word3 <- ""
      word2 <- paste(tokens_word[[1]][t-1], tokens_word[[1]][t])
      word1 <- paste(tokens_word[[1]][t])
    } else if(t == 1) {
      word4 <- ""
      word3 <- ""
      word2 <- ""
      word1 <- paste(tokens_word[[1]][t])
    } else 
      stop("not possible to predict")
    if(word4 != "") {
      fourgram_lda <- readRDS("fourgram_lda.rds")
      fourgram_text <- dfm(text, what="word", tolower=TRUE, remove_numbers=TRUE, remove_punct=TRUE, 
                           remove_symbols=TRUE, remove_twitter=TRUE, ngrams=4, concatenator=" ")
      text_topic <- posterior(fourgram_lda, fourgram_text)
      text_topic <- which.max(text_topic$topic)
      fourgram_lda_tibble <- readRDS("fourgram_lda_tibble.rds")
      subset <- subset(fourgram_lda_tibble, fourgram_lda_tibble$topic == text_topic)
      subset <- subset(subset, grepl(paste0("^", word3, " "), subset$term))
      if(length(subset) > 0) {
        nextword <- top_n(subset,3, beta)
        nextword <- sub(paste0("^", word3, " "), "", nextword$term)
      }
    }
    if(length(nextword) == 0 & word3 != "") {
      trigram_lda <- readRDS("trigram_lda.rds")
      trigram_text <- dfm(text, what="word", tolower=TRUE, remove_numbers=TRUE, remove_punct=TRUE, 
                          remove_symbols=TRUE, remove_twitter=TRUE, ngrams=3, concatenator=" ")
      text_topic <- posterior(trigram_lda, trigram_text)
      text_topic <- which.max(text_topic$topic)
      trigram_lda_tibble <- readRDS("trigram_lda_tibble.rds")
      subset <- subset(trigram_lda_tibble, trigram_lda_tibble$topic == text_topic)
      subset <- subset(subset, grepl(paste0("^", word2, " "), subset$term))
      if(length(subset) > 0) {
        nextword <- top_n(subset, 3, beta)
        nextword <- sub(paste0("^", word2, " "), "", nextword$term)
      }
    }
    if(length(nextword) == 0 & word2 != "") {
      bigram_lda <- readRDS("bigram_lda.rds")
      bigram_text <- dfm(text, what="word", tolower=TRUE, remove_numbers=TRUE, remove_punct=TRUE, 
                         remove_symbols=TRUE, remove_twitter=TRUE, ngrams=2, concatenator=" ")
      text_topic <- posterior(bigram_lda, bigram_text)
      text_topic <- which.max(text_topic$topic)
      bigram_lda_tibble <- readRDS("bigram_lda_tibble.rds")
      subset <- subset(bigram_lda_tibble, bigram_lda_tibble$topic == text_topic)
      subset <- subset(subset, grepl(paste0("^", word1, " "), subset$term))
      if(length(subset) > 0) {
        nextword <- top_n(subset, 3, beta)
        nextword <- sub(paste0("^", word1, " "), "", nextword$term)
      } else {
        nextword <- "not possible to predict"
      }
    }
    }
    paste(nextword, collapse = "\n")
  })
})