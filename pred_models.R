library(tidyverse)
library(tidymodels)
library(viridis)
library(patchwork)

# Data --------------------------------------------------------------------

# load questions and corresponding pooled embeddings
questions <- read_csv("data/questions.csv")
embeddings <- read_csv("data/embeddings_python.csv")

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
  tibble(mep_citizenship = .,
         region = names(.)) %>% 
  mutate(region = factor(str_remove_all(region, "\\d")))

# merge questions and embeddings
data <- bind_cols(questions, embeddings) %>% 
  rename_with( ~ paste0("embed", .x), matches("^\\d+$")) %>% 
  mutate(party = factor(ifelse(is.na(party), "unknown/multiple", party)),
         date = decimal_date(document_date)) %>% 
  left_join(regions, by = "mep_citizenship")

# formula: party ~ embeddings
f_party <- as.formula(paste("party ~", paste(grep("^embed", names(data), value = TRUE), 
                                             collapse = " + ")))
# formula: date ~ embeddings
f_date <- as.formula(paste("date ~", paste(grep("^embed", names(data), value = TRUE),
                                                      collapse = " + ")))
# formula: region ~ embeddings
f_region <- as.formula(paste("region ~", paste(grep("^embed", names(data), value = TRUE), 
                                             collapse = " + ")))

# Model definition --------------------------------------------------------

# multinomial logit
model_multilogit <- multinom_reg()
# linear regression
model_linreg <- linear_reg()

pred_model <- function(formula, model, data, pred_name = ".pred") {
  workflow() %>% 
    add_model(model) %>% 
    add_formula(formula) %>% 
    fit(data) %>% 
    predict(data) %>% 
    bind_cols(select(data, url)) %>% 
    setNames(c(pred_name, "url"))
}

# Model fitting -----------------------------------------------------------

# party ~ embedding
pred_party <- pred_model(f_party, model_multilogit, data, "pred_party")
# date ~ embedding
pred_date <- pred_model(f_date, model_linreg, data, "pred_date")
# region ~ embedding
pred_region <- pred_model(f_region, model_multilogit, data, "pred_region")
# date ~ embedding per party
pred_date_party <- map_df(
  unique(data$party), 
  ~ pred_model(f_date, model_linreg, filter(data, party == .x), "pred_date_party")
)
# date ~ embedding per region
pred_date_region <- map_df(
  unique(data$region), 
  ~ pred_model(f_date, model_linreg, filter(data, region == .x), "pred_date_region")
)

# merge predictions with questions
preds <- data %>% 
  select(url, party, region, date) %>% 
  left_join(pred_party, by = "url") %>% 
  left_join(pred_region, by = "url") %>% 
  left_join(pred_date, by = "url") %>% 
  left_join(pred_date_party, by = "url") %>%
  left_join(pred_date_region, by = "url")
write_csv(preds, "results/predictions.csv")

# Evaluation --------------------------------------------------------------

# party classification confusion matrix
preds %>% 
  pivot_longer(c("party", "region", "pred_party", "pred_region"),
               names_to = "model", values_to = "pred") %>% 
  separate(model, c("model", "var"), sep = "_", fill = "left") %>% 
  mutate(model = ifelse(is.na(model), "Truth", "Prediction")) %>%
  pivot_wider(names_from = model, values_from = pred) %>%
  group_by(var, Truth) %>% 
  mutate(n_truth = n()) %>% 
  group_by(var, Truth, Prediction) %>% 
  summarize(prop = n()/mean(n_truth)*100) %>% 
  ggplot(aes(Truth, Prediction, fill = prop)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(prop), "%")), color = "white") +
  scale_fill_viridis(option = "mako") +
  labs(fill = "% of truth") +
  facet_wrap(~ var, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
        strip.text = element_text(size = 12))
ggsave("results/confusion_matrix.png", width = 12, height = 6)

# date predictions
p1 <- preds %>%
  pivot_longer(c("pred_date", "pred_date_party"),
               names_to = "model", values_to = "pred_date") %>%
  mutate(model = case_when(
    model == "pred_date" ~ "Model with all questions",
    model == "pred_date_party" ~ "Separate models per party"
  )) %>%
  ggplot(aes(date, pred_date, color = party)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(se = FALSE) +
  labs(x = "True date", y = "Predicted date", color = "Party") +
  scale_x_continuous(breaks = 2019:2024) +
  facet_wrap(~ model) +
  theme_minimal()
p2 <- preds %>%
  pivot_longer(c("pred_date", "pred_date_region"),
               names_to = "model", values_to = "pred_date") %>%
  mutate(model = case_when(
    model == "pred_date" ~ "Model with all questions",
    model == "pred_date_region" ~ "Separate models per region"
  )) %>%
  ggplot(aes(date, pred_date, color = region)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(se = FALSE) +
  labs(x = "True date", y = "Predicted date", color = "Region",
       caption = "Dashed line is 45-degree line") +
  scale_x_continuous(breaks = 2019:2024) +
  facet_wrap(~ model) +
  theme_minimal()
p1 / p2
ggsave("results/date_pred.png", width = 10, height = 8)

# party/region accuracy over time
preds %>% 
  pivot_longer(c("party", "region", "pred_party", "pred_region"),
               names_to = "model", values_to = "pred") %>% 
  separate(model, c("model", "var"), sep = "_", fill = "left") %>% 
  mutate(model = ifelse(is.na(model), "Truth", "Prediction")) %>% 
  pivot_wider(names_from = model, values_from = pred) %>%
  mutate(month = floor_date(date_decimal(date), "month")) %>%
  group_by(month, var, Truth) %>% 
  mutate(n_truth = n()) %>% 
  group_by(month, var, Truth, Prediction) %>% 
  summarize(prop = n()/mean(n_truth)*100) %>% 
  ggplot(aes(month, prop, color = Truth)) +
  geom_smooth(se = FALSE) +
  labs(x = "Date", y = "% of truth correctly predicted", color = "Truth") +
  facet_wrap(~ var) +
  theme_minimal()
ggsave("results/accuracy_date.png", width = 10, height = 6)
