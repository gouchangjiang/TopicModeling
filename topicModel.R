library(stringr)
library(tm)
library(text2vec)
#library(tidyr)
#setwd("~/Documents/courses/MachineLearning/TopicModeling/datasets")
setwd("C:/Users/JoAnn/Desktop/topicModeling")
rm(list=ls())

#Emails<-read.table("allEmails.txt",header = TRUE)
Emails<-Emailout$content
rm(list=setdiff(ls(), "Emails"))
Emails<-as.data.frame(Emails)
Emails$ID<-seq(1:nrow(Emails))
colnames(Emails)[1]<-"content"

#------------preprocess and tokenization---------
prep_fun = function(x){
  x %>%
    str_to_lower %>%
    str_replace_all("[^[:alpha:]]", " ") %>%
    str_replace_all("\\s+"," ") %>%
    str_replace_all("'"," ") %>%
    str_replace_all("[[:cntrl:]]", " ") %>%
    str_replace_all("^[[:space:]]+"," ") %>%
    str_replace_all("[[:space:]]+$"," ")
}

mystopwords = c(stopwords(kind = "SMART"),"dear", "best","regards",'"forward',"forwarded","by","on",
                "http","www","said")

emails.filterd<-prep_fun(Emails$content)
train_index<-sample(Emails$ID,0.9*nrow(Emails))
iter = itoken(emails.filterd[train_index],progressbar = FALSE)
vocab = create_vocabulary(iter,stopwords = mystopwords) %>%
  prune_vocabulary(doc_proportion_max = 0.1,term_count_min = 5)
vectorizer = vocab_vectorizer(vocab)
DTM = create_dtm(iter,vectorizer)#create Document-term matrix

tfidf = TfIdf$new()
lsa = LSA$new(n_topics=10)

doc_embeddings = DTM %>%
  fit_transform(tfidf) %>%
  fit_transform(lsa)

dim(doc_embeddings)

dim(lsa$components)

#------------------------LDA-------------------------------------
DTM=create_dtm(iter,vectorizer,type = "dgTMatrix")

lda_model = LDA$new(n_topics = 10, doc_topic_prior = 0.05, topic_word_prior = 0.001)
doc_topic_distr = lda_model$fit_transform(x = DTM, n_iter = 10000, 
                          convergence_tol = 0.001, n_check_convergence = 25, 
                          progressbar = FALSE)

barplot(doc_topic_distr[1, ], xlab = "topic", 
        ylab = "proportion", ylim = c(0, 1), 
        names.arg = 1:ncol(doc_topic_distr))

lda_model$get_top_words(n = 10, topic_number = c(1L, 5L, 10L), lambda = 1)

lda_model$get_top_words(n = 10, topic_number = c(1:10), lambda = 0.2)

#------perplexity
test_index = setdiff(Emails$ID, train_index)

DTM_TEST = itoken(emails.filterd[test_index], tolower, word_tokenizer, ids = test_index) %>% 
  create_dtm(vectorizer, type = "dgTMatrix")
new_doc_topic_distr = lda_model$transform(DTM_TEST)

perplexity(DTM_TEST, topic_word_distribution = lda_model$topic_word_distribution, doc_topic_distribution = new_doc_topic_distr)

GetPerplexity<-function(topic_number){
  lda_model = LDA$new(n_topics = topic_number, doc_topic_prior = 0.05, topic_word_prior = 0.001)
  
  doc_topic_distr = lda_model$fit_transform(x = DTM, n_iter = 10000, 
                                            convergence_tol = 0.001, n_check_convergence = 25, 
                                            progressbar = FALSE)
  
  new_doc_topic_distr = lda_model$transform(DTM_TEST)
  
  perpv<-perplexity(DTM_TEST, topic_word_distribution = lda_model$topic_word_distribution, doc_topic_distribution = new_doc_topic_distr)
  return(perpv)
}

Perp<-unlist(lapply(seq(10,30,2),GetPerplexity))

plot(seq(10,30,2),Perp,xlab = "topic amount", 
     ylab = "perplexity",type="l")
points(seq(10,30,2),Perp)


lda_model$plot()
