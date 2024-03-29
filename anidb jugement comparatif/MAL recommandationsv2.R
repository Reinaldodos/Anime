pacman::p_load(rvest, data.table, tidyverse)
source("Mangapark MAL sync/Fonctions.R")

url = "https://myanimelist.net/animelist/Altermedia"

MAList =
  url %>% read_html() %>%
  html_nodes(css = ".animetitle")

MAL_names = MAList %>% html_text() %>% map_chr(trimws)

MAL_url = MAList %>% html_attr(name = "href") %>% str_c("https://myanimelist.net", .)

output = MAL_url %>% sort %>% SAFE_FETCH()

# Franchises --------------------------------------------------------------

Related =
  output %>% map(Fetch_related) %>%
  map(data.table) %>% bind_rows(.id = "url") %>%
  rename(Title = V1) %>%
  mutate(Recs = 999)

saveRDS(object = Related, file = "anidb jugement comparatif/Related.rds")
# Recommandations ---------------------------------------------------------
Recs =
  output %>%
  map(
    .f = ~ html_nodes(x = ., css = "#horiznav_nav a") %>%
      html_attr(name = "href") %>% str_subset(pattern = "userrecs")
  ) %>%
  compact() %>%
  map(data.table) %>% bind_rows(.id = "url") %>% rename(url_recs = V1)

url_recs = Recs$url_recs %>% sort %>% SAFE_FETCH()

Recs =
  url_recs %>% map(FETCH_Recs) %>% bind_rows(.id = "url_recs") %>%
  inner_join(Recs, by = "url_recs")

saveRDS(object = Related, file = "anidb jugement comparatif/Recommended.rds")

# Wrapping up -------------------------------------------------------------

FINAL =
  list(
    "anidb jugement comparatif/Related.rds",
    "anidb jugement comparatif/Recommended.rds"
  ) %>%
  map(.f = read_rds) %>%
  bind_rows(Related, Recs) %>%
  inner_join(cbind.data.frame(url = MAL_url, Ref = MAL_names), by = "url") %>%
  select(Ref, Title, Recs)

FINAL %>% saveRDS(file = "anidb jugement comparatif/Reseau")
