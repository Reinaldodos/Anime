pacman::p_load(tidyverse, data.table, rvest)
# Fetching ----------------------------------------------------------------
source(file = "anidb jugement comparatif/FONCTIONS.R")
source(file = "MAL modelisation/Fonctions.R")

url = "http://graph.anime.plus/Altermedia/list,anime"
data = url %>% LIST()


# Fetching MAL ------------------------------------------------------------
safe_read = safely(read_html)

# create the progress bar with a dplyr function.
input =
  data$url %>% set_names() %>%
  map(.f = read_with_progress,
      pb = progress_estimated(length(data$url)))

output = input %>% purrr::transpose() %>% map(compact)
input = output$result

while (length(output$error)) {
  output$error %>% length %>% str_c(., " errors left") %>% cat(sep = "\n")
  output =
    output$error %>% names %>% set_names() %>%
    map(.f = read_with_progress,
        pb = progress_estimated(length(output$error))) %>%
    purrr::transpose() %>% map(compact)

  input = append(input, output$result)
}

input %>% saveRDS(file = "MAL modelisation/Main infos.rds")

# Fetch infos main page ---------------------------------------------------
Seasons = input %>% map(.f = read_CSS, CSS = ".season a")
Types = input %>% map(.f = read_CSS, CSS = ".type a")
Studios = input %>% map(.f = read_CSS, CSS = ".author a")
Source = input %>% map(.f = read_INFO, INFO = "Source")
Genres = input %>% map(.f = read_INFO, INFO = "Genre")

DATA =
  list(
    "Season" = Seasons,
    "Type" = Types,
    "Genre" = Genres,
    "Source" = Source,
    "Studio" = Studios
  ) %>%
  purrr::transpose() %>% map(compact) %>%
  map(cbind.data.frame)  %>%
  dplyr::bind_rows(.id = "url") %>%
  left_join(x = data)

DATA %>% saveRDS(file = "MAL modelisation/DATA main.rds")


# Fetching seiyuus & staff ------------------------------------------------


input_Chara =
  input %>% map(Fetch_Chara) %>%
  map(.f = read_with_progress,
      pb = progress_estimated(length(input)))

output = input_Chara %>% purrr::transpose() %>% map(compact)
input_Chara = output$result

output = output$error

while (length(output$error)) {
  output$error %>% length %>% str_c(., " errors left") %>% cat(sep = "\n")
  output =
    output$error %>% names %>%
    map(Fetch_Chara) %>%
    map(.f = read_with_progress,
        pb = progress_estimated(length(output$error))) %>%
    purrr::transpose() %>% map(compact)

  input_Chara = append(input_Chara, output$result)
}

input_Chara %>% saveRDS(file = "MAL modelisation/Chara infos.rds")



Seiyuus = input_Chara %>% map(.f = read_Seiyuus)

input_Chara[[11]] %>%
  read_CSS(CSS = ".js-scrollfix-bottom-rel .borderClass") %>%
  str_trim() %>%
  str_split_fixed(n = 2, pattern = "\n") %>%
  data.table() %>%
  mutate_all(.funs = str_trim) %>%
  filter(nchar(V2)>0)


input_Chara[[11]] %>% read_html() %>%
  html_nodes(css = ".js-scrollfix-bottom-rel .borderClass") %>%
  html_text()
