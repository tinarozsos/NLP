library(tidyverse)
library(viridis)

# questions and trained LDA
questions <- read_csv("data/questions.csv")
lda <- read_rds("lda_model.rds")

# topic probabilities per document
topics <- lda$theta %>% 
  as.data.frame() %>% 
  rownames_to_column("url") %>%
  inner_join(questions, by = "url") %>% 
  mutate(across(c(party, region), factor))

# word probabilities per topic
topic_words <- lda$gamma %>% 
  as_tibble() %>% 
  setNames(colnames(lda$data)) %>% 
  mutate(topic = paste("t", row_number(), sep = "_")) %>% 
  pivot_longer(-topic, names_to = "word", values_to = "prob")

# give topics titles based on n most probable words
topic_titles <- topic_words %>% 
  group_by(topic) %>% 
  slice_max(prob, n = 10) %>% 
  mutate(id = row_number()) %>%
  select(-prob) %>% 
  pivot_wider(names_from = id, values_from = word) %>%
  unite(word, -topic, sep = ", ") %>% 
  ungroup() %>% 
  mutate(title = c("Mediterranean agriculture", "asylum", "pollution",
                   "animal agriculture", "Commission President + Erasmus program",
                   "energy", "entrepreneurship", "cohesion", "legal jargon", "health care",
                   "privacy and AI", "equality", "Middle East", "Turkey vs Cyprus",
                   "nature", "transport", "elections", "finance", "COVID",
                   "EU expansion"))

# plot average topic probabilities per group and region
topics %>% 
  pivot_longer(matches("^t_\\d+$"), names_to = "topic", values_to = "prob") %>%
  pivot_longer(c(party, region), names_to = "var", values_to = "group") %>%
  group_by(topic, group, var) %>% 
  summarize(mean_prob = mean(prob)) %>%
  mutate(var = ifelse(var == "party", "Group", "Region")) %>% 
  left_join(topic_titles) %>% 
  mutate(topic = paste(str_remove(topic, "t_"), title, sep = ": ")) %>%
  ggplot(aes(topic, group, fill = mean_prob)) +
  geom_tile() +
  geom_hline(yintercept = 1.5:8.5, color = "white", linewidth = 1) +
  labs(x = "Topic", y = NULL, fill = "Average topic probability") +
  scale_fill_viridis(option = "mako") +
  facet_wrap(~ var, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
