# library(tidyverse)
# library(qdap)
# library(dplyr)
# library(tm)
# library(wordcloud)
# library(plotrix)
# library(ggplot2)
# library(ggpubr)
# library(qdapRegex)
# library(stringr)
# library("DescTools")
# library(arrow)

library(tidyverse)
library(tidymodels)
library(stopwords)
library(tm)
library(textrecipes)
library(textclean)
library(textdata)
library(wordsalad)

# ================
# Read the data
# ================


questions <- read_csv("data/questions.csv")

clean_data <- questions %>% 
  mutate(text = str_replace_all(text, "\\[\\d+\\]|\n|https?://\\S+", " "),
         text = str_to_lower(text),
         text = replace_contraction(text), 
         text = removeNumbers(text),
         text = removePunctuation(text),
         text = str_replace_all(text, "\\s+", " "))

# # Remove references 
# questions$text <- gsub("\\[\\d+\\]", "", questions$text)
# questions$text <- gsub("\n", " ", questions$text)
# 
# #Probably, also drop links
# questions$text <- gsub("https?://\\S+", "", questions$text)
# questions$text <- replace_contraction(removeNumbers(text))
# questions$text[1:5]


# ==============================================================================
# BAG OF WORDS
# ==============================================================================

# #Strip the text from....
# texts<- questions$text
# 
# #Do some pre-processing
# texts<- removePunctuation(                      #Commas again (sometimes doing it a second time helps)
#   removeNumbers(                                #Numbers
#     removePunctuation(                          #Commas
#       tolower(                                  #Capital letters
#         rm_non_words(                           #Non-nonsensical words
#           replace_contraction(                  #Contractions
#             removeNumbers(texts)))))))          #Numbers (although I'm not sure - should we keep numbers here?)
# 
# #Make a corpus of words
# text_source<-VectorSource(texts)
# text_corpus<- VCorpus(text_source)
# 
# # Stop word cleaning 
# corpus_cleaned<- tm_map(text_corpus,
#                         removeWords,
#                         stopwords("en"))       #Remove stop words
# 
# #Create a Document-Term Matrix
# text_DT_matrix<-DocumentTermMatrix(corpus_cleaned)
# 
# #Transform into a dataframe 
# frequencies_table<-as.matrix(text_DT_matrix)
# frequencies_table<-as.data.frame(frequencies_table)
# 
# # tf routine=============================================================
# 
# #Total terms in the document 
# words_total<- apply(frequencies_table, MARGIN=1, FUN=sum)
# z
# write.csv(words_total, "bow_total_words.csv")
# 
# # Total frequency (per document)
# frequencies_table<- frequencies_table/words_total
# 
# parquet = tempfile(fileext = ".parquet")
# 
#     #Still takes ages
#     #write_parquet(frequencies_table, sink = parquet)
# 
# 
# party_counts<- cbind(questions[, c("url", "party")], frequencies_table)
# 
# sub_data <- data_counts[1:5, c(1, 3, 70:90)]
# 
# table_of_count<- party_counts %>% 
#   pivot_longer(-c("url", "party"), names_to="word",
#                values_to="count") %>%
#   group_by(party, word) %>%
#   summarise(total_mentioned = sum(count))
# 
# 
# # Stopped here 
# table_of_count %>%
#   group_by(party) %>%
#   arrange(desc(total_mentioned))

# ==============================================================================
# MODEL SETUP
# ==============================================================================

max_vocab_size <- 5000 # for BoW
embedding_dim <- 100 # for embeddings

recipe_base <- recipe(~ text, data = clean_data) %>%
  # convert to clean words (remove punctuation, convert to lowercase)
  step_tokenize(text) %>%
  # remove stopwords (Snowball lexicon)
  step_stopwords(text) %>% 
  # keep only the most frequent words (n=max_vocab_size)
  step_tokenfilter(text, max_tokens = max_vocab_size) 

# ==============================================================================
# ALTERNATIVE BAG OF WORDS
# ==============================================================================

# calculate term frequency (BoW)
recipe_bow <- recipe_base %>%
  step_tf(text)

# apply transformation to the data to get BoW matrix
bow_data <- bake(prep(recipe_bow), new_data = NULL, composition = "matrix")

# combine word frequencies with party labels
party_counts <- bind_cols(party = clean_data$party, as_tibble(bow_data)) %>% 
  group_by(party) %>% 
  # sum term frequencies across documents per party
  summarize(across(everything(), sum)) %>% 
  pivot_longer(-party, names_to = "word", values_to = "count") %>% 
  # clean word names
  mutate(word = str_remove(word, "^tf_text_"),
         # word's share in total words used by party
         prop_in_party = count / sum(count))

# ==============================================================================
# PRE-TRAINED EMBEDDINGS (GLOVE)
# ==============================================================================

# get embeddings
glove <- glove(clean_data$text, dim = embedding_dim, 
               stopwords = data_stopwords_snowball$en)

# add GloVe embeddings
recipe_glove <- recipe_base %>%
  step_word_embeddings(text, embeddings = glove)

# calculate GloVe embeddings per document
glove_data <- bake(prep(recipe_glove), new_data = NULL, composition = "matrix")

# ==============================================================================
# PRE-TRAINED EMBEDDINGS (word2vec)
# ==============================================================================

# get embeddings
word2vec <- word2vec(clean_data$text, dim = embedding_dim, 
                     stopwords = data_stopwords_snowball$en)

# add word2vec embeddings
recipe_word2vec <- recipe_base %>%
  step_word_embeddings(text, embeddings = word2vec)

# calculate word2vec embeddings per document
word2vec_data <- bake(prep(recipe_word2vec), new_data = NULL, composition = "matrix")
