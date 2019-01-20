url = "http://graph.anime.plus/Altermedia/list,anime"
pacman::p_load(tidyverse, rio, data.table)
source("anidb jugement comparatif/FONCTIONS.R")
LIST <- function(url){
  pacman::p_load(rvest)
  pacman::p_load(data.table)
  Table = url %>%
    read_html() %>%
    html_table()

  pacman::p_load(dplyr)
  Table = Table[[1]] %>%
    mutate(
      R = suppressWarnings(as.integer(R)),
      Diff = as.numeric(Diff),
      Note = R - Diff
    ) %>%
    data.table()

  Table$url = url %>% read_html() %>% html_nodes(css = "#main a") %>% html_attr(name = "href")
  Table$Title = url %>% read_html() %>% html_nodes(css = "#main a") %>% html_text()
  Table = Table[!is.na(Table$R)]

  colnames(Table) = c("S", "Player", "Rating", "Diff", "Note", "url")
  STAR = "★"
  Table$Player = gsub(pattern = STAR,
                      replacement = " ",
                      x = Table$Player)
  Table$Player = gsub(
    pattern = Table[grep(pattern = "Ghoul ", Table$Player)]$Player,
    replacement = "Tokyo Ghoul vA",
    x = Table$Player
  )
  return(Table[, .(Player, Rating, Note, url)])
}
# source(file = "MAL recommandationsv2.R")
# Chargement des données ------------------------------------------------------------------
Table = SCORING(url)

toto =
  readRDS(file =
            "anidb jugement comparatif/Reseau")

franchises =
  readRDS(file = "anidb jugement comparatif/Franchises")

foo =
  rename(toto, An1 = Ref, An2 = Title) %>%
  bind_rows(rename(toto, An2 = Ref, An1 = Title)) %>%
  select(An1, An2) %>%
  bind_rows(franchises) %>%
  inner_join(LISTER(Table)) %>%
  unique()

# SCRIPT ------------------------------------------------------------------
source(file = "anidb jugement comparatif/Purger les combats.R", encoding = "UTF-8")
SCRIPT()
SCRIPT2(url = url)

FINAL = url %>% CHANGES
FINAL %>% arrange(Note, Avant) %>% View()
