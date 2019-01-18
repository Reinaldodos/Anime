pacman::p_load(rvest, data.table, tidyverse, igraph)

url = "http://graph.anime.plus/Altermedia/list,anime"
Selection =
  url %>% read_html %>% html_table %>% rbindlist() %>%
  filter(R %>% as.numeric() %>% is.na()) %>% .$Title

Dropped =
  c("https://myanimelist.net/animelist/Altermedia?status=1&tag=",
    "https://myanimelist.net/animelist/Altermedia?status=2&tag=",
    "https://myanimelist.net/animelist/Altermedia?status=3&tag=",
    "https://myanimelist.net/animelist/Altermedia?status=4&tag="
  ) %>%
  # .[3:4] %>%
  map(read_html) %>%
  map(.f = ~ html_nodes(x = ., css = ".animetitle"))

Select = (Dropped %>% map(html_text) %>% map(trimws) %>% unlist()) %in% Selection

Dropped =
  Dropped %>%
  map(.f = ~ html_attr(x = ., name = "href")) %>%
  unlist() %>%
  paste("http://myanimelist.net", ., sep = "")

Dropped = Dropped[Select]

safe_read=safely(read_html)

RElated =
  Dropped  %>%
  map(.f = safe_read) %>%
  map(.f = ~.$result)

safe_node = safely(html_nodes)
FINAL =
  RElated %>%
  map(.f = ~ safe_node(x = ., css = ".anime_detail_related_anime")) %>%
  map(.f = ~ .$result) %>% map(.f = safely(html_table)) %>%
  map(.f = ~ .$result) %>%
  map(rbindlist) %>% bind_rows()

FINAL %>%
  filter(grepl(pattern = "Summary", x = X1)) %>%
  distinct(X2) %>%
  write_csv(path = "../What to watch/Recaps.csv")

FINAL %>% split(x = .$X2, f = .$X1)
