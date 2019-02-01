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

  return(Table[, .(Player, Rating, Note, url)])
}

JOB <- function(url) {
  "Purger les combats" %>% cat(sep = "\n")
  source(file = "anidb jugement comparatif/Purger les combats.R", encoding = "UTF-8")

  EGUENNE = readline(prompt = "Encore? o/n \t")
  while (EGUENNE == "o") {
    SCRIPT2(url = url)
    EGUENNE = readline(prompt = "Encore? o/n \t")
  }

  FINAL = url %>% CHANGES
  return(FINAL)
}

FINAL = JOB(url = url)
