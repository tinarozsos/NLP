library(tidyverse)
library(XML)

# national party info from ParlGov
# https://parlgov.org/data-info/
parlgov <- read_csv("data/parlgov.csv") %>% 
  # clean special characters, different spacing, etc.
  mutate(across(c(party_name, party_name_short, party_name_english),
                ~ str_replace_all(iconv(str_to_lower(.x), 
                                        from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                                  "[^[:alnum:]]", "")))

# Political groups and national parties of MEPs: 
# https://www.europarl.europa.eu/meps/en/search/advanced?name=&euPoliticalGroupBodyRefNum=&countryCode=&bodyType=ALL
mep_party <- map_df(
  list.files("data/mep_party", full.names = TRUE),
  ~ xmlParse(.x) %>% xmlToDataFrame() %>% mutate(party = basename(.x))
) %>% 
  # clean special characters, different spacing, etc.
  mutate(party = str_remove(party, ".xml"),
         party = str_to_upper(str_replace(party, "-", "/")),
         nationalPoliticalGroup = str_replace_all(iconv(str_to_lower(nationalPoliticalGroup), 
                                                        from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                                                  "[^[:alnum:]]", ""),
         # fix individual discrepancies (only if party has >5 MEPs, otherwise just drop as it won't affect averages much)
         nationalPoliticalGroup = case_when(
           nationalPoliticalGroup == "christlichdemokratischeuniondeutschlands" ~ "christlichdemokratischeunion",
           nationalPoliticalGroup == "lega" ~ "legadazionemeridionale",
           nationalPoliticalGroup == "partidopopular" ~ "alianzapartidopopular",
           nationalPoliticalGroup == "europeecologie" ~ "generationecologie",
           nationalPoliticalGroup == "forzaitalia" ~ "forzaitaliailpopolodellaliberta",
           nationalPoliticalGroup == "listerenaissance" ~ "larepubliqueenmarcherenaissance",
           nationalPoliticalGroup == "lesrepublicains" ~ "unionpourunmouvementpopulairelesrepublicains",
           nationalPoliticalGroup == "mouvementdemocrate" ~ "unionpourlademocratiefrancaisemouvementdemocrate",
           nationalPoliticalGroup == "neademokratia" ~ "neadimokratia",
           nationalPoliticalGroup == "christlichsozialeunioninbayernev" ~ "christlichsozialeunion",
           nationalPoliticalGroup == "arbetarepartietsocialdemokraterna" ~ "socialdemokraterna",
           TRUE ~ nationalPoliticalGroup
         ))

# get political group orientations as weighted mean of national party orientations
orientations <- mep_party %>% 
  count(party, nationalPoliticalGroup, country) %>%
  filter(nationalPoliticalGroup != "independent") %>%
  # look for matches with any of the ParlGov party names columns
  left_join(select(parlgov, party_name, country_name, left_right, eu_anti_pro, state_market, liberty_authority), 
            by = c("nationalPoliticalGroup" = "party_name",
                   "country" = "country_name")) %>% 
  left_join(select(parlgov, party_name_short, country_name, left_right, eu_anti_pro, state_market, liberty_authority), 
            by = c("nationalPoliticalGroup" = "party_name_short",
                   "country" = "country_name")) %>% 
  left_join(select(parlgov, party_name_english, country_name, left_right, eu_anti_pro, state_market, liberty_authority), 
            by = c("nationalPoliticalGroup" = "party_name_english",
                   "country" = "country_name")) %>% 
  # combine data from different name matches
  rowwise() %>% 
  mutate(left_right = mean(c(left_right.x, left_right.y, left_right), na.rm = TRUE),
         eu_anti_pro = mean(c(eu_anti_pro.x, eu_anti_pro.y, eu_anti_pro), na.rm = TRUE),
         state_market = mean(c(state_market.x, state_market.y, state_market), na.rm = TRUE),
         liberty_authority = mean(c(liberty_authority.x, liberty_authority.y, liberty_authority), na.rm = TRUE)) %>%
  # calculate group orientation as weighted mean of national party orientations
  # weights are number of MEPs from each national party
  group_by(party) %>% 
  summarize(left_right = weighted.mean(left_right, n, na.rm = TRUE),
            eu_anti_pro = weighted.mean(eu_anti_pro, n, na.rm = TRUE),
            state_market = weighted.mean(state_market, n, na.rm = TRUE),
            liberty_authority = weighted.mean(liberty_authority, n, na.rm = TRUE))
write_csv(orientations, "data/party_orientations.csv")
