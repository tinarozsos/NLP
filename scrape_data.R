library(tidyverse)
library(rvest)

# Get list of questions from CSV files, extract URLs
# data source: https://data.europarl.europa.eu/en/datasets?language=en&order=RELEVANCE&dataThemeFamily=dataset.theme.EP_QUEST_DOC
url <- map_df(list.files("data/url_list", full.names = TRUE), 
                      read_csv, show_col_types = FALSE) %>% 
  mutate(url = paste0("https://www.europarl.europa.eu/doceo/document",
                      str_extract(document_URI, "/[^/]*$"),
                      ("_EN.html"))) %>%
  # keep only questions from 9th parliamentary term (2019-2024)
  filter(document_parliamentary_term == 9) %>% 
  # remove institutions/committees as authors
  mutate(document_creator_person = str_remove(document_creator_person, "^(([A-Z]{2,}|Renew|Verts/ALE); )+"))

# Get personal details of MEPs
# data source: https://data.europarl.europa.eu/en/datasets/members-of-the-european-parliament-meps-parliamentary-term9
mepinfo <- read_csv("data/_meps_9_56_en.csv") %>% 
  # clean variables
  mutate(author1 = str_to_title(paste(mep_given_name, mep_family_name)),
         author1 = str_replace_all(author1, "ß", "ss"),
         mep_birthday = as.Date(str_extract(mep_birthday, "[\\d-]*"), "%Y-%m-%d")) %>% 
  distinct(author1, .keep_all = TRUE)

# Read questions from URLs
get_question <- function(url) {
  read_html(url) %>% 
    html_nodes("main") %>% 
    html_text2() %>% 
    tibble(url = url, text_raw = .)
}
questions_raw <- map_df(url$url, possibly(get_question, tibble(url = NA, text_raw = NA)))
write_csv(questions_raw, "data/questions_raw.csv")

# Clean questions
questions <- questions_raw %>%
  # remove unnecessary text (possible link to answer, footer)
  mutate(text = str_remove_all(text_raw, "\\n\\nAnswer.*"),
         text = str_remove_all(text, "\\nLast updated(.|\\n)*$")) %>% 
  # separate metadata from raw text
  separate(text, into = c("title", "date", "info", "text"), 
           sep = "\n\n", extra = "merge") %>% 
  # extract party family and question recipient
  mutate(party = str_extract(info, "\\(\\S*\\)$"),
         party = str_remove_all(party, "[()]"),
         party = ifelse(party == "EPP", "PPE", party), # unify French and English abbreviation
         recipient = str_extract(info, "(?<=to the ).*(?=\\n)")) %>% 
  select(url, text, party, recipient) %>% 
  # add clean metadata from CSV files
  left_join(select(url, url, document_identifier, document_title,
                   document_type, document_date, document_creator_person), by = "url") %>% 
  # remove one row with broken link
  drop_na(url) %>% 
  # add MEP information
  mutate(author1 = str_to_title(str_remove(document_creator_person, ";.*$")),
         author1 = str_replace(author1, "Glueck", "Glück"), # person's name spelled in multiple ways
         n_authors = str_count(document_creator_person, ";") + 1) %>%
  left_join(mepinfo) %>% 
  # remove irrelevant variables
  select(url:mep_identifier, mep_gender, mep_citizenship, mep_birthday)

# Save question text and metadata
write_csv(questions, "data/questions.csv")
