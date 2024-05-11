library(tidyverse)
library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(ggplot2)
library(ggpubr)
library(qdapRegex)
library(stringr)
library("DescTools")
library(arrow)

# ================
# Read the data
# ================


questions <- read.csv("data/questions.csv")

# Remove references 
questions$text <- gsub("\\[\\d+\\]", "", questions$text)
questions$text <- gsub("\n", "", questions$text)

#Probably, also drop links
questions$text <- gsub("https?://\\S+", "", questions$text)
questions$text[1:5]


# ==============================================================================
# BAG OF WORDS
# ==============================================================================

#Strip the text from....
texts<- questions$text

#Do some pre-processing
texts<- removePunctuation(                      #Commas again (sometimes doing it a second time helps)
  removeNumbers(                                #Numbers
    removePunctuation(                          #Commas
      tolower(                                  #Capital letters
        rm_non_words(                           #Non-nonsensical words
          replace_contraction(                  #Contractions
            removeNumbers(texts)))))))          #Numbers (although I'm not sure - should we keep numbers here?)

#Make a corpus of words
text_source<-VectorSource(texts)
text_corpus<- VCorpus(text_source)

# Stop word cleaning 
corpus_cleaned<- tm_map(text_corpus,
                        removeWords,
                        stopwords("en"))       #Remove stop words

#Create a Document-Term Matrix
text_DT_matrix<-DocumentTermMatrix(corpus_cleaned)

#Transform into a dataframe 
frequencies_table<-as.matrix(text_DT_matrix)
frequencies_table<-as.data.frame(frequencies_table)

# tf routine=============================================================

#Total terms in the document 
words_total<- apply(frequencies_table, MARGIN=1, FUN=sum)
z
write.csv(words_total, "bow_total_words.csv")

# Total frequency (per document)
frequencies_table<- frequencies_table/words_total

parquet = tempfile(fileext = ".parquet")

    #Still takes ages
    #write_parquet(frequencies_table, sink = parquet)


party_counts<- cbind(questions[, c("url", "party")], frequencies_table)

sub_data <- data_counts[1:5, c(1, 3, 70:90)]

table_of_count<- party_counts %>% 
  pivot_longer(-c("url", "party"), names_to="word",
               values_to="count") %>%
  group_by(party, word) %>%
  summarise(total_mentioned = sum(count))


# Stopped here 
table_of_count %>%
  group_by(party) %>%
  arrange(desc(total_mentioned))
