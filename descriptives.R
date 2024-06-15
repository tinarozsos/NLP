library(tidyverse)
library(readxl)
library(tidytext)
library(patchwork)

# main data
questions <- read_csv("data/questions.csv")
# number of MEPs per party
parties <- read_excel("data/party_descriptions.xlsx")
# number of MEPs per country
countries <- read_csv("data/mep_party.csv") %>% count(country) 

# list of countries per region
regions <- list(
  "Northern Europe" = c("Denmark", "Finland", "Sweden", "Estonia", "Latvia", "Lithuania", "Ireland", "United Kingdom"),
  "Southern Europe" = c("Cyprus", "Italy", "Malta", "Greece"),
  "Southwestern Europe" = c("Portugal", "Spain"),
  "Western Europe" = c("Belgium", "Luxembourg", "Netherlands", "France"),
  "Central Europe" = c("Austria","Germany"),
  "Eastern Europe" = c("Bulgaria", "Czechia", "Croatia", "Hungary", "Poland", "Romania", "Slovakia", "Slovenia")
) %>% 
  unlist() %>% 
  tibble(country = .,
         region = names(.)) %>% 
  mutate(region = factor(str_remove_all(region, "\\d")))

# p1 <- questions %>% 
#   mutate(length = str_count(text, "\\s+") + 1) %>%
#   ggplot(aes(length, fill = party)) +
#   geom_density(alpha = 0.4) +
#   labs(x = "Number of words per question",
#        y = "Density",
#        fill = "Group") +
#   guides(fill = guide_legend(override.aes = list(alpha = 1, color = NULL))) +
#   theme_minimal() +
#   theme(legend.position = c(0.8, 0.6),
#         legend.background = element_rect(fill = "white"))
# 
# p2 <- questions %>% 
#   mutate(length = str_count(text, "\\s+") + 1) %>%
#   ggplot(aes(length, fill = region)) +
#   geom_density(alpha = 0.4) +
#   labs(x = "Number of words per question",
#        y = "Density",
#        fill = "Region") +
#   guides(fill = guide_legend(override.aes = list(alpha = 1, color = NULL))) +
#   theme_minimal() +
#   theme(legend.position = c(0.8, 0.6),
#         legend.background = element_rect(fill = "white"))

# merge counts of questions per party and per region
bind_rows(
  questions %>%
    count(party) %>% 
    # add number of seats per party
    left_join(select(parties, party, seats)) %>% 
    rename(group = party) %>% 
    # add id variable for facetting
    mutate(type = "group"),
  questions %>% 
    count(region) %>% 
    # add number of seats per region
    left_join(countries %>% left_join(regions) %>% group_by(region) %>% summarise(seats = sum(n))) %>% 
    rename(group = region) %>% 
    # add id variable for facetting
    mutate(type = "region")
) %>% 
  # calculate number of questions per seat per group
  mutate(n_per_group = n / seats) %>%
  pivot_longer(cols = c(n, n_per_group)) %>%
  # nicer labels
  mutate(name = ifelse(name == "n", "Number of questions", "Number of questions per MEP"),
         name = paste(name, type, sep = " per ")) %>% 
  # plot with bars rearranged within panels
  ggplot(aes(value, reorder_within(group, value, name), fill = group)) +
  geom_col() +
  labs(x = NULL,
       y = NULL) +
  scale_y_reordered() +
  facet_wrap(~name, dir = "v", scales = "free", strip.position = "bottom") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.placement = "outside",
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))
ggsave("results/counts.png", width = 10, height = 5)


# test heatmap with black-white text
questions %>% 
  count(party, region) %>% 
  mutate(n = n / sum(n) * 100,
         lab = paste(round(n), "%")) %>%
  ggplot(aes(region, party, fill = n)) +
  geom_tile() +
  shadowtext::geom_shadowtext(aes(label = lab), size = 4) +
  scale_fill_viridis_c() +
  theme_minimal()
