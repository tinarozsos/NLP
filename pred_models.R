library(tidyverse)
library(tidymodels)
library(viridis)

# Data --------------------------------------------------------------------

# load questions and corresponding pooled embeddings
questions <- read_csv("data/questions.csv")
embeddings <- read_csv("data/embeddings_python.csv")

# merge questions and embeddings
data <- bind_cols(questions, embeddings) %>% 
  rename_with( ~ paste0("embed", .x), matches("^\\d+$")) %>% 
  mutate(party = ifelse(is.na(party), "unknown/multiple", party),
         party = factor(party))

# define formula: party ~ embeddings
formula <- as.formula(paste("party ~", paste(grep("^embed", names(data), value = TRUE), 
                                             collapse = " + ")))

# Model definition --------------------------------------------------------

# multinomial logit
model_multilogit <- multinom_reg()
# multinomial gradient boosting
model_gb <- boost_tree(mtry = tune(), trees = 500, tree_depth = 1, learn_rate = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# tuning grids per model
grid_gb <- expand_grid(mtry = c(1, 10, 25, 50), learn_rate = 10^(-3:-1))

# prediction function with optional tuning and cross-validation
predict_class <- function(formula, model, data, tune_params = NULL, 
                          nfolds = 5, tune_metric = "accuracy") {
  # define main workflow
  w <- workflow() %>% 
    add_formula(formula) %>% 
    add_model(model) 
  
  # workflow without tuning parameters
  if (is.null(tune_params)) {
    fit <- w %>% fit(data)
  } else {
    # workflow with tuning parameters
    
    # v-fold cross-validation split
    cv <- vfold_cv(data, nfolds)
    
    # choose hyperparameters based on chosen metric (default: accuracy)
    b <- w %>% 
      tune_grid(resamples = cv, grid = tune_params) %>% 
      select_best(tune_metric)
    
    # estimate final model on full data
    fit <- w %>% finalize_workflow(b) %>% fit(data)
  }
  
  # predict dependent variable
  bind_cols(fit %>% predict(data, "prob"),
            fit %>% predict(data, "class"))
}

# Model fitting -----------------------------------------------------------

# get predictions per model
multilogit_pred <- predict_class(formula, model_multilogit, data)
gb_pred <- predict_class(formula, model_gb, data, grid_gb)

# merge predictions with questions
preds <- data %>% 
  select(url, party) %>% 
  bind_cols(multilogit_pred %>% rename_with(~ paste0(.x, "_multilogit")),
            gb_pred %>% rename_with(~ paste0(.x, "_gb"))
            )
write_csv(preds, "results/predictions.csv")

# Evaluation --------------------------------------------------------------

# party classification confusion matrix
preds %>% 
  pivot_longer(starts_with(".pred_class"), names_to = "model", values_to = "pred_class") %>%  
  mutate(model = case_when(str_detect(model, "multilogit") ~ "Multinomial logit",
                           str_detect(model, "gb") ~ "Gradient boosting")) %>%
  group_by(model, party) %>% 
  mutate(n_truth = n()) %>% 
  group_by(model, party, pred_class) %>% 
  summarize(prop = n()/mean(n_truth)*100) %>% 
  ggplot(aes(party, pred_class, fill = prop)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(prop), "%")), color = "white") +
  scale_fill_viridis(option = "mako") +
  labs(x = "True party", y = "Predicted party", 
       fill = "% of true party") +
  facet_wrap(~ model) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
ggsave("results/confusion_matrix.png", width = 10, height = 6)

# distribution of predicted probabilities
preds %>% 
  select(!starts_with(".pred_class")) %>%
  pivot_longer(starts_with(".pred"), values_to = "pred") %>% 
  separate(name, into = c("x", "pred_party", "model"), sep = "_") %>%
  mutate(model = case_when(str_detect(model, "multilogit") ~ "Multinomial logit",
                           str_detect(model, "gb") ~ "Gradient boosting")) %>%
  ggplot(aes(pred, fct_rev(pred_party), fill = party)) +
  geom_violin() +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c(0, 0.5, 1)) +
  facet_grid(~ model ~ party) +
  labs(x = "Predicted probability", y = "Predicted party", title = "True party") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 12))
ggsave("results/party_probabilities.png", width = 10, height = 8)