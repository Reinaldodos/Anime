pacman::p_load(rvest, data.table, tidyverse)
source("Mangapark MAL sync/Fonctions.R")

url = "https://myanimelist.net/animelist/Altermedia"

MAList =
  url %>% read_html() %>%
  html_nodes(css = ".animetitle")

MAL_names = MAList %>% html_text() %>% map_chr(trimws)

MAL_url = MAList %>% html_attr(name = "href") %>% str_c("https://myanimelist.net", .)

output = SAFE_FETCH(input = MAL_url)
# Franchises --------------------------------------------------------------

Fetch_related <- function(url) {
  Relations =
    url %>%
    html_nodes(css = ".anime_detail_related_anime a")

  toto = Relations %>%
    html_attr(name = "href") %>% str_detect(pattern = "anime")
  Relations %>% html_text() %>% .[toto] %>%
    return()
}

Related =
  output %>% map(Fetch_related) %>% map(data.table) %>% bind_rows(.id = "url") %>% rename(Title = V1) %>%
  mutate(Recs = 999)

# Recommandations ---------------------------------------------------------
Recs =
  output %>%
  map(
    .f = ~ html_nodes(x = ., css = "#horiznav_nav a") %>%
      html_attr(name = "href") %>% str_subset(pattern = "userrecs")
  ) %>%
  compact() %>%
  map(data.table) %>% bind_rows(.id = "url") %>% rename(url_recs = V1)

url_recs = Recs$url_recs %>% SAFE_FETCH()

FETCH_Recs <- function(url) {
  Strong =
    url %>%
    html_nodes(css = "strong") %>% html_text()

  LOG = Strong %>% as.integer %>% map_lgl(is.na)
  OK = data.table(Title = Strong[LOG])
  OK$Recs = Strong[!LOG] %>% as.integer() %>% c(., rep(0, nrow(OK))) %>% .[1:nrow(OK)]

  OK %>% mutate(Recs = Recs + 1) %>% return()
}

Recs =
  url_recs %>% map(FETCH_Recs) %>% bind_rows(.id = "url_recs") %>%
  inner_join(Recs, by = "url_recs")

FINAL =
  bind_rows(Related, Recs) %>%
  inner_join(cbind.data.frame(url = MAL_url, Ref = MAL_names), by = "url") %>%
  select(Ref, Title, Recs)

FINAL %>% saveRDS(file = "anidb jugement comparatif/Reseau")
