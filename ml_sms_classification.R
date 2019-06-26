#loading required packages
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(e1071)
library(caret)
library(gmodels)
library(NLP)

#loading data
sms_data<-read.csv("D:/sms_spam.csv",header=TRUE,col.names = c("type","text"), stringsAsFactors = FALSE)
attach(sms_data)

# explore the data
str(sms_data)
head(sms_data)
sms_data$type <- factor(sms_data$type)
str(sms_data$type)
table(sms_data$type)

#---text mining & data preprocessing---

#vectorsource create one document for each sms
#corpus collection of text documents

clean_corpus<-function(corpus){
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeNumbers)
corpus<-tm_map(corpus,content_transformer(tolower))
#corpus<-tm_map(corpus,content_transformer(replace_symbol))
corpus<-tm_map(corpus,removeWords,c(stopwords("en")))
corpus<-tm_map(corpus,stripWhitespace)
corpus<-tm_map(corpus, stemDocument)
return(corpus)
}
sms_corpus <- VCorpus(VectorSource(sms_data$text))
sms_clean<-clean_corpus(sms_corpus)

#DTM to split these text messages into individual words through tokenization
sms_dtm <- DocumentTermMatrix(sms_clean)

#creating test and train data
sms_dtm_train <- sms_dtm[1:4180,]
sms_dtm_test <- sms_dtm[4181:5574,]

sms_train_labels <- sms_data[1:4180,]$type
sms_test_labels <- sms_data[4181:5574,]$type

prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

#data visualization
wordcloud(sms_corpus_clean, max.words = 50, random.order = FALSE)

spam <- subset(sms_data, type == "spam")
ham <- subset(sms_data, type == "ham")
wordcloud(spam$text, max.words = 50, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 50, scale = c(3, 0.5))

sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

#naive bayes
sms_classifier <- naiveBayes(sms_train, sms_train_labels)
sms_test_pred <- predict(sms_classifier, sms_test)

#confusion matrix is to measure the performance the classifier
confuse<-confusionMatrix(sms_test_pred,sms_test_labels)

