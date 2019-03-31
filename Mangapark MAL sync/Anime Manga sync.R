pacman::p_load(rvest, tidyverse, data.table, rio)
source("Season watch/FONCTIONS.R")
source("Mangapark MAL sync/Fonctions.R")

Listes =
  list(MAL_mangas = "https://myanimelist.net/mangalist/Altermedia",
       MAL_anime = "https://myanimelist.net/animelist/Altermedia") %>%
  map(read_html)


# Fetch anime list --------------------------------------------------------
ANIME =
  Listes$MAL_anime %>%
  html_nodes(css = ".animetitle")
ANIME =
  cbind.data.frame(
    Title = ANIME %>% html_text() %>% trimws(),
    url = ANIME %>% html_attr(name = "href") %>% str_c("https://myanimelist.net", .)
  )


# Fetch manga sources -----------------------------------------------------
safe_read = safely(read_html)
MangaList = ANIME$url %>% as.character() %>% SAFE_FETCH()

MAL_sources = MangaList %>% map(SOURCING) %>% flatten_chr() %>% unique()

# Fetch manga list --------------------------------------------------------
MAL_Mangas =
  Listes$MAL_mangas %>%
  html_nodes(css = ".animetitle") %>% html_attr(name = "href")

Manga_Selection = setdiff(MAL_sources, MAL_Mangas) %>%
  str_c("https://myanimelist.net", .)


# Manga or novel? ---------------------------------------------------------
Manga_Selection = Manga_Selection %>% SAFE_FETCH()

Manga_desuka = Manga_Selection %>% map(.f = Manga_ka) %>% compact() %>%
  names

Manga_desuka %>% cat(sep = "\n")
