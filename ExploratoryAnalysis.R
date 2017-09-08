
# http://rpubs.com/vanda_dias/296407

#packages
library(tm) #not used
library(knitr)
library(RWeka) #not used
library(openNLP) #not used
library(ggplot2)
library(reshape)
library(quanteda)

#opens connections
connection_en_US_twitter <- file("final/en_US/en_US.twitter.txt")
connection_en_US_blogs <- file("final/en_US/en_US.blogs.txt")
connection_en_US_news <- file("final/en_US/en_US.news.txt")

#retrieves text files in lines output
data_en_US_twitter <- readLines(connection_en_US_twitter)
data_en_US_blogs <- readLines(connection_en_US_blogs)
data_en_US_news <- readLines(connection_en_US_news)

#closes connections
close(connection_en_US_twitter)
close(connection_en_US_blogs)
close(connection_en_US_news)

#counts number of lines
lines_en_US_twitter <- length(data_en_US_twitter)
lines_en_US_blogs <- length(data_en_US_blogs)
lines_en_US_news <- length(data_en_US_news)

#counts number of characters
char_en_US_twitter <- sum(nchar(data_en_US_twitter))
char_en_US_blogs <- sum(nchar(data_en_US_blogs))
char_en_US_news <- sum(nchar(data_en_US_news))

#maximum number of characteres per line
maxchar_en_US_twitter <- max(char_en_US_twitter)
maxchar_en_US_blogs <- max(char_en_US_blogs)
maxchar_en_US_news <- max(char_en_US_news)

#files' size
size_en_US_twitter <- object.size(data_en_US_twitter)
size_en_US_blogs <- object.size(data_en_US_blogs)
size_en_US_news <- object.size(data_en_US_news)

#creates table
sumstatistics <- data.frame(Lines=c(lines_en_US_twitter, lines_en_US_blogs, lines_en_US_news), NCharacteres=c(char_en_US_twitter,char_en_US_blogs, char_en_US_news), MaxNCharacteresPerLine=c(maxchar_en_US_twitter, maxchar_en_US_blogs, maxchar_en_US_news), ObjectSize=c(size_en_US_twitter, size_en_US_blogs, size_en_US_news), row.names = c("en_US.twitter", "en_US.blogs", "en_US.news"))
kable(sumstatistics, caption = "Statistics on Lines and Characteres")

#sampling
n <- 0.50
set.seed(2017)
sample_en_US_twitter <- data_en_US_twitter[rbinom(lines_en_US_twitter*n, lines_en_US_twitter, 0.5)]
singlesample_en_US_twitter <- paste(sample_en_US_twitter, collapse = " ")
set.seed(2017)
sample_en_US_blogs <- data_en_US_blogs[rbinom(lines_en_US_blogs*n, lines_en_US_blogs, 0.5)]
singlesample_en_US_blogs <- paste(sample_en_US_blogs, collapse = " ")
set.seed(2017)
sample_en_US_news <- data_en_US_news[rbinom(lines_en_US_news*n, lines_en_US_news, 0.5)]
singlesample_en_US_news <- paste(sample_en_US_news, collapse = " ")
join_samples_en_US <- c(singlesample_en_US_twitter, singlesample_en_US_blogs, singlesample_en_US_news)

#creates document-feature matrix with the cleaning process incorporated
#incredibly faster
unigram <- dfm(join_samples_en_US, what="word", tolower=TRUE, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_twitter=TRUE, ngrams=1)
bigram <- dfm(join_samples_en_US, what="word", tolower=TRUE, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_twitter=TRUE, ngrams=2)
trigram <- dfm(join_samples_en_US, what="word", tolower=TRUE, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_twitter=TRUE, ngrams=3)

#subsets and plots most frequent terms
#unigram
unigram_df <- as.data.frame(t(unigram))
names(unigram_df) <- c("en_US.twitter", "en_US.blogs", "en_US.news")
unigram_df <- cbind(unigram_df, all=rowSums(unigram_df), term=row.names(unigram_df))
unigram_df <- unigram_df[order(-unigram_df$all),]
unigram_df_20 <- within(head(unigram_df, 20), rm(all))
unigram_df_20 <- melt(unigram_df_20, id=c("term"))
g_unigram <- ggplot(data = unigram_df_20, aes(x=reorder(term, value), y=value, fill=variable))
g_unigram + geom_bar(stat = "identity") + coord_flip() + scale_fill_brewer(palette = "Paired") + labs(y="Word Frequency", x="Words", title="Most frequent words", fill="document")

#bigram
bigram_df <- as.data.frame(t(bigram))
names(bigram_df) <- c("en_US.twitter", "en_US.blogs", "en_US.news")
bigram_df <- cbind(bigram_df, all=rowSums(bigram_df), term=row.names(bigram_df))
bigram_df <- bigram_df[order(-bigram_df$all),]
bigram_df_20 <- within(head(bigram_df, 20), rm(all))
bigram_df_20 <- melt(bigram_df_20, id=c("term"))
g_bigram <- ggplot(data = bigram_df_20, aes(x=reorder(term, value), y=value, fill=variable))
g_bigram + geom_bar(stat = "identity") + coord_flip() + scale_fill_brewer(palette = "Paired") + labs(y="Two Words Frequency", x="Two Words", title="Most frequent two words", fill="document")

#trigram
trigram_df <- as.data.frame(t(trigram))
names(trigram_df) <- c("en_US.twitter", "en_US.blogs", "en_US.news")
trigram_df <- cbind(trigram_df, all=rowSums(trigram_df), term=row.names(trigram_df))
trigram_df <- trigram_df[order(-trigram_df$all),]
trigram_df_20 <- within(head(trigram_df, 20), rm(all))
trigram_df_20 <- melt(trigram_df_20, id=c("term"))
g_trigram <- ggplot(data = trigram_df_20, aes(x=reorder(term, value), y=value, fill=variable))
g_trigram + geom_bar(stat = "identity") + coord_flip() + scale_fill_brewer(palette = "Paired") + labs(y="Three Words Frequency", x="Three Words", title="Most frequent three words", fill="document")






##not used commands
#creates corpus
corpus_en_US <- Corpus(VectorSource(join_samples_en_US), readerControl = list(language="en_US", load=TRUE))
#cleaning process of the data
clean_corpus_en_US <- tm_map(corpus_en_US, content_transformer(tolower)) #all data gets transformed to lower case
clean_corpus_en_US <- tm_map(clean_corpus_en_US, content_transformer(removePunctuation)) #all punctuation is removed from the data
clean_corpus_en_US <- tm_map(clean_corpus_en_US, content_transformer(removeNumbers)) #all numbers are removed from the data
clean_corpus_en_US <- tm_map(clean_corpus_en_US, content_transformer(stripWhitespace)) #multiple whitespace characters are collapsed to a single blank
#check corpus content
corpus[[1]]$content
#create TermDocument Matrix
TDM_en_US <- TermDocumentMatrix(clean_corpus_en_US) #uses default tokenizer for words
TDM_en_US_nosparce <- removeSparseTerms(TDM_en_US, 0.65) #removes terms less frequent
TDM_en_US$nrow
#analyse most frequent terms
mostfreqterms <- data.frame(findMostFreqTerms(TDM_en_US, 20))
names(mostfreqterms) <- c("en_US.twitter", "en_US.blogs", "en_US.news")
mostfreqterms <- cbind(mostfreqterms, terms=row.names(mostfreqterms))
mostfreqterms <- melt(mostfreqterms, id=c("terms"))
g <- ggplot(data = mostfreqterms, aes(x=reorder(terms,-value), y=value, fill=variable))
g + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Paired")



