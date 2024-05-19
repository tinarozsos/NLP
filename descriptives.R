library(tidyverse)
library(patchwork)

questions <- read_csv("data/questions.csv")

p1 <- questions %>% 
  mutate(length = str_count(text, "\\s+") + 1) %>%
  ggplot(aes(length, fill = party)) +
  geom_density(alpha = 0.4) +
  labs(x = "Number of words per question",
       y = "Density",
       fill = "Group") +
  guides(fill = guide_legend(override.aes = list(alpha = 1, color = NULL))) +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.6),
        legend.background = element_rect(fill = "white"))

p2 <- questions %>% 
  mutate(length = str_count(text, "\\s+") + 1) %>%
  ggplot(aes(length, fill = region)) +
  geom_density(alpha = 0.4) +
  labs(x = "Number of words per question",
       y = "Density",
       fill = "Region") +
  guides(fill = guide_legend(override.aes = list(alpha = 1, color = NULL))) +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.6),
        legend.background = element_rect(fill = "white"))

p3 <- questions %>% 
  count(party) %>% 
  ggplot(aes(n, reorder(party, n), fill = party)) +
  geom_col() +
  labs(x = "Number of questions",
       y = "Group") +
  theme_minimal() +
  theme(legend.position = "none")

p4 <- questions %>% 
  count(region) %>% 
  ggplot(aes(n, reorder(region, n), fill = region)) +
  geom_col() +
  labs(x = "Number of questions",
       y = "Region") +
  theme_minimal() +
  theme(legend.position = "none")

(p1 + p2) /  (p3 + p4)
ggsave("results/counts.png", width = 10, height = 7)
