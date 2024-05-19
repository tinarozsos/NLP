library(tidyverse)
library(tidytext)
library(tm)
library(textclean)
library(stopwords)
library(topicmodels)
library(patchwork)

# Tokenization ------------------------------------------------------------
# improve with phrases, etc.

questions <- read_csv("data/questions.csv")

words <- questions %>% 
  mutate(text = str_replace_all(text, "\\[\\d+\\]|\n|https?://\\S+", " "),
         text = replace_contraction(text), 
         text = removeNumbers(text),
         text = str_replace_all(text, "\\s+", " ")) %>% 
  unnest_tokens(word, text) %>%
  filter(!word %in% c(data_stopwords_snowball$en, 
                      "eu", "european", "commission", "member", 
                      "can", "view", "submitted", "rights", "states", 
                      "state", "ensure" ))

# Fit LDA -----------------------------------------------------------------

# get document-term matrix
dtm <- words %>% 
  count(url, word) %>% 
  bind_tf_idf(word, url, n) %>% 
  # keep only words that appear in less than 90% of documents
  slice_max(idf, prop = 0.9) %>%
  cast_dtm(url, word, n)

# LDA with different number of topics
for (k in 2:10) {
  # LKA with k topics
  lda <- LDA(dtm, k = k, control = list(seed = 1))
  
  # word allocation to topics
  word_topics <- tidy(lda, matrix = "beta")
  
  # document allocation to topics
  doc_topics <- tidy(lda, matrix = "gamma") %>% 
    group_by(document) %>% 
    slice_max(gamma, n = 1)
  
  # main words per topic
  p1 <- word_topics %>% 
    mutate(topic = paste("Topic", topic)) %>%
    group_by(topic) %>% 
    slice_max(beta, n = 10) %>% 
    ggplot(aes(beta, reorder(term, beta), fill = topic)) + 
    geom_col() +
    labs(y = "", x = "Per-topic-per-word probability",
         fill = "Topic") +
    scale_x_continuous(n.breaks = 3) +
    facet_wrap(~topic, scales = "free_y") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # topic distribution per party
  p2 <- doc_topics %>% 
    mutate(topic = paste("Topic", topic)) %>%
    left_join(select(questions, url, party), by = c("document" = "url")) %>%
    ggplot(aes(y = party, fill = topic)) +
    geom_bar(position = "fill") +
    scale_x_continuous(labels = scales::percent) +
    labs(y = "Political group", x = "Proportion of questions",
         fill = "Topic") +
    theme_minimal()
  
  # topic distribution per region
  p3 <- doc_topics %>% 
    mutate(topic = paste("Topic", topic)) %>%
    left_join(select(questions, url, region), by = c("document" = "url")) %>%
    ggplot(aes(y = region, fill = topic)) +
    geom_bar(position = "fill") +
    scale_x_continuous(labels = scales::percent) +
    labs(y = "Region", x = "Proportion of questions",
         fill = "Topic") +
    theme_minimal()
  
  p1 / (p2 + p3) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
  ggsave(paste0("results/lda_", k, ".png"), width = 12, height = 12)
}
