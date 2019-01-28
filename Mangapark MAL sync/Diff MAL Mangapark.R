pacman::p_load(rvest, tidyverse, data.table, rio)
Mangapark =
  "Mangapark MAL sync/Mangapark.txt" %>%
  import(fill = T) %>%
  mutate_all(trimws) %>%
  filter(
    nchar(V1) > 0,!str_detect(string = V1, pattern = "ch\\."),!str_detect(string = V1, pattern = "vol\\.")
  )  %>%
  as.list() %>% purrr::transpose() %>%
  map(.f = paste, collapse = " ") %>% flatten_chr() %>% str_trim()

MAL_mangas =
  c(1, 6) %>%
  map_chr(.f = ~str_c("https://myanimelist.net/mangalist/Altermedia?status=", .)) %>%
  map(.f = read_html) %>%
  map(.f = html_nodes, css = ".animetitle span") %>%
  map(.f = html_text) %>% flatten_chr()

Correspondance =
  list(
  MAL_mangas %>%
    set_names() %>% map(.f = agrep, x = Mangapark, value = T) %>%
    map(as.data.table) %>%
    bind_rows(.id = "MAL") %>%
    rename(Mangapark = V1) ,
  Mangapark %>%
    set_names() %>% map(.f = agrep, x = MAL_mangas, value = T) %>%
    map(as.data.table) %>%
    bind_rows(.id = "Mangapark") %>%
    rename(MAL = V1)
) %>% reduce(full_join, by = c("MAL", "Mangapark"))

Doublons =
  list(Correspondance %>% count(MAL),
       Correspondance %>% count(Mangapark)) %>%
  map(.f = filter, n > 1) %>% map(select,-n) %>%
  map(.f = inner_join, y = Correspondance) %>%
  reduce(full_join, by = c("MAL", "Mangapark")) %>%
  filter(MAL == Mangapark)

Correspondance =
  bind_rows(
    inner_join(Correspondance, Doublons %>% distinct(MAL)) %>% anti_join(Doublons),
    inner_join(Correspondance, Doublons %>% distinct(Mangapark)) %>% anti_join(Doublons)
  ) %>%
  anti_join(x = Correspondance)

setdiff(MAL_mangas, Correspondance$MAL)
