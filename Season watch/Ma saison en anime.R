pacman::p_load(rvest, tidyverse, data.table)
Liste_Anime =
  "https://myanimelist.net/anime/season" %>%
  read_html()
source("FONCTIONS.R")

# SCRIPT ------------------------------------------------------------------
print("FETCHEZ LA VACHE!")
ANIME =
  Liste_Anime %>%
  ANIMATION()
safe_read = safely(read_html)
input =
  ANIME$URL %>% as.character %>%
  set_names %>%
  map(safe_read)

output = input %>% purrr::transpose()  %>% map(compact)
input = output$result

Sequels = input %>% map(PREQUEL_DESUKA)
Mangas = input %>% map(SOURCING) %>% compact

Scores =
  Mangas %>% map(.f = ~ str_c("https://myanimelist.net", .)) %>%
  map(safely(SCORE_SOURCE))

Scores = Scores %>% purrr::transpose() %>% .$result

output =
  list(URL=ANIME$URL %>% as.character %>% set_names,
       output=Scores,
       Sequel = Sequels) %>%
  purrr::transpose() %>%
  map_df(as.data.table) %>%
  mutate_all(.funs = trimws) %>%
  left_join(x = ANIME, by = "URL") %>%
  mutate(output.Score=output.Score %>% as.numeric,
        Sequel=Sequel %>% as.logical)