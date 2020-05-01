# load libraries----
library(RWeka)
library(tm)

# connect to data source----
twit.path <- file("./final/en_US/en_US.twitter.txt", open = "rb")
blog.path <- file("./final/en_US/en_US.blogs.txt", open = "rb")
news.path <- file("./final/en_US/en_US.news.txt", open = "rb")
twitter <- readLines(twit.path, encoding = "UTF-8", skipNul = TRUE)
close(twit.path)
blogs <- readLines(blog.path, encoding = "UTF-8", skipNul = TRUE)
close(blog.path)
news <- readLines(news.path, encoding = "UTF-8", skipNul = TRUE)
close(news.path)

# sample data----
set.seed(12345)
sample.size <- 0.1 # 10% sample
data.sample <- c(sample(twitter, length(twitter)*sample.size, 
                        replace = FALSE),
                 sample(blogs, length(blogs)*sample.size, 
                        replace = FALSE),
                 sample(news, length(news)*sample.size, 
                        replace = FALSE))
data.sample <- sample(data.sample, length(data.sample), replace = FALSE)
#writeLines(data.sample, "./sample.txt")

# data cleaning----
cleaning <- function(corpus) {
    corpus <- tm_map(corpus, tolower) # convert to lower case
    corpus <- tm_map(corpus, removePunctuation) # remove punctuation
    corpus <- tm_map(corpus, removeNumbers) # remove numbers
    corpus <- tm_map(corpus, stripWhitespace) # remove white spaces
    corpus <- tm_map(corpus, PlainTextDocument) # convert to plain text
    corpus <- tm_map(corpus, removeWords, stopwords("english")) # remove stopwords
}

data.sample <- iconv(data.sample, "UTF-8", "ASCII", sub = "")
data.sample <- VCorpus(VectorSource(data.sample))
data.sample <- cleaning(data.sample)

# function to get n-grams----
getNgram <- function(corpus, n) {
    token <- function(x) NGramTokenizer(x, Weka_control(min=n, max=n))
    tdm <- TermDocumentMatrix(corpus, control=list(tokenize=token))
    tdm
}

# function to extract n-grams and sort by freq----
freqNgram <- function(tdm, freq) {
    tdm.freq <- findFreqTerms(tdm, lowfreq = freq)
    tdm.df <- rowSums(as.matrix(tdm[tdm.freq,]))
    tdm.df <- data.frame(word = names(tdm.df), frequency = tdm.df)
    tdm.df <- tdm.df[order(-tdm.df$frequency),]
    tdm.df
}

# calculate n-grams, remove sparse terms----
tdm1gram <- getNgram(data.sample, 1) %>% 
    removeSparseTerms(0.99)
tdm2gram <- getNgram(data.sample, 2) %>% 
    removeSparseTerms(0.999)
tdm3gram <- getNgram(data.sample, 3) %>% 
    removeSparseTerms(0.9999)
tdm4gram <- getNgram(data.sample, 4) %>% 
    removeSparseTerms(0.99999)

# count freq of n-grams----
freq1gram <- freqNgram(tdm1gram, 20)
freq2gram <- freqNgram(tdm2gram, 20)
freq3gram <- freqNgram(tdm3gram, 20)
freq4gram <- freqNgram(tdm4gram, 20)

# split n-grams into individual words----
unigram <- data.frame(rows = rownames(freq1gram), count = freq1gram$frequency)
unigram$rows <- as.character(unigram$rows)
uni.split <- strsplit(unigram$rows, split = " ")
unigram <- transform(unigram, first = sapply(uni.split,"[[",1))
unigram <- data.frame(unigram = unigram$first,
                      freq = unigram$count,
                      stringsAsFactors = FALSE)
write.csv(unigram[unigram$freq>1,], "unigram.csv",
          row.names = FALSE)
unigram <- read.csv("unigram.csv",
                    stringsAsFactors = FALSE)
saveRDS(unigram, "unigram.RData")

bigram <- data.frame(rows = rownames(freq2gram), count = freq2gram$frequency)
bigram$rows <- as.character(bigram$rows)
bi.split <- strsplit(bigram$rows, split = " ")
bigram <- transform(bigram, 
                    first = sapply(bi.split,"[[",1),
                    second = sapply(bi.split,"[[",2))
bigram <- data.frame(unigram = bigram$first,
                     bigram = bigram$second,
                      freq = bigram$count,
                      stringsAsFactors = FALSE)
write.csv(bigram[bigram$freq>1,], "bigram.csv",
          row.names = FALSE)
bigram <- read.csv("bigram.csv",
                    stringsAsFactors = FALSE)
saveRDS(bigram, "bigram.RData")

trigram <- data.frame(rows = rownames(freq3gram), count = freq3gram$frequency)
trigram$rows <- as.character(trigram$rows)
tri.split <- strsplit(trigram$rows, split = " ")
trigram <- transform(trigram, 
                    first = sapply(tri.split,"[[",1),
                    second = sapply(tri.split,"[[",2),
                    third = sapply(tri.split,"[[",3))
trigram <- data.frame(unigram = trigram$first,
                     bigram = trigram$second,
                     trigram = trigram$third,
                     freq = trigram$count,
                     stringsAsFactors = FALSE)
write.csv(trigram[trigram$freq>1,], "trigram.csv",
          row.names = FALSE)
trigram <- read.csv("trigram.csv",
                   stringsAsFactors = FALSE)
saveRDS(trigram, "trigram.RData")

quadgram <- data.frame(rows = rownames(freq4gram), count = freq4gram$frequency)
quadgram$rows <- as.character(quadgram$rows)
quad.split <- strsplit(quadgram$rows, split = " ")
quadgram <- transform(quadgram, 
                     first = sapply(quad.split,"[[",1),
                     second = sapply(quad.split,"[[",2),
                     third = sapply(quad.split,"[[",3),
                     fourth = sapply(quad.split,"[[",4))
quadgram <- data.frame(unigram = quadgram$first,
                      bigram = quadgram$second,
                      trigram = quadgram$third,
                      quadgram = quadgram$fourth,
                      freq = quadgram$count,
                      stringsAsFactors = FALSE)
write.csv(quadgram[quadgram$freq>1,], "quadgram.csv",
          row.names = FALSE)
quadgram <- read.csv("quadgram.csv",
                    stringsAsFactors = FALSE)
saveRDS(quadgram, "quadgram.RData")
