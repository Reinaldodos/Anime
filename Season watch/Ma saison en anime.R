pacman::p_load(rvest, tidyverse, data.table)
Liste_Anime =
  "https://myanimelist.net/anime/season" %>%
  read_html()
source("Season watch/FONCTIONS.R")

safe_read = safely(read_html)

# SCRIPT ------------------------------------------------------------------
print("FETCHEZ LA VACHE!")

ANIME =
  Liste_Anime %>%
  ANIMATION()

input =
  ANIME$URL %>% as.character %>%
  set_names %>%
  map(safe_read)

output = input %>% purrr::transpose()  %>% map(compact)
input = output$result

while (length(output$error)) {
  output = output$error %>% names %>% set_names() %>% map(safe_read)
  input = list(input, output$result)
}

input = input %>% flatten
Sequels = input %>% map(PREQUEL_DESUKA)
Mangas = input %>% map(SOURCING) %>% compact

Scores =
  Mangas %>% map(.f = ~ str_c("https://myanimelist.net", .)) %>%
  map(safely(SCORE_SOURCE))

Scores = Scores %>% purrr::transpose() %>% .$result

output =
  list(URL = ANIME$URL %>% as.character %>% set_names,
       output = Scores,
       Sequel = Sequels) %>%
  purrr::transpose() %>%
  map_df(as.data.table) %>%
  mutate_all(.funs = trimws) %>%
  left_join(x = ANIME, by = "URL") %>%
  mutate(output.Score = output.Score %>% as.numeric,
         Sequel = Sequel %>% as.logical)

# Sequels -----------------------------------------------------------------

Sequels = output %>% filter(Sequel)

Sequels %>% pull(Title) %>% write.table(file =  "Season watch/Season sequels.csv", row.names = F, quote = F)
Sequels = "Season watch/Season sequels.csv" %>% readLines()

output =
  output %>%
  filter(Title %in% Sequels) %>%
  anti_join(x = output)
