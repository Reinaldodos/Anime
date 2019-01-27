require(tidyverse)
require(rvest)
require(data.table)
LIST <- function(url)
{
  require(data.table)
  require(rvest)
  Table = url %>%
    read_html() %>%
    html_table()

  Table = Table[[1]] %>% data.table()
  Table = Table[, .(Title, R)]
  Table$R = as.integer(Table$R)

  Table$url = url %>% read_html() %>% html_nodes(css = "#main a") %>% html_attr(name = "href")
  Table$Title = url %>% read_html() %>% html_nodes(css = "#main a") %>% html_text()
  # Table = Table[!is.na(Table$R)]
  return(Table)
}

SimiScrap <- function(toto)
{
  toto = toto %>% data.table()
  test = toto$url %>%
    read_html() %>% html_nodes(css = ".horiznav_active") %>% html_attr(name = "href")
  test = paste(test, "/userrecs", sep = "")

  noms = test %>%
    read_html() %>% html_nodes(css = "#content div:nth-child(2) strong") %>% html_text()
  nombres = test %>%
    read_html() %>% html_nodes(css = ".js-similar-recommendations-button strong") %>%
    html_text() %>%
    c(rep(x = 0, times = length(noms)))

  if (length(noms) > 0)
  {
    Simi = cbind.data.frame(
      Ref = toto$Title,
      Title = noms[1:length(nombres)],
      Recs = as.numeric(nombres) + 1,
      Rate = toto$R
    ) %>% data.table()
    # head(Simi) %>% print()
    return(Simi[!is.na(Title)])
  }
}

Recommandation <- function(url)
{
  url = "http://graph.anime.plus/Altermedia/list,anime"
  Table = LIST(url)

  require(foreach)
  output = foreach(n = 1:nrow(Table), .combine = rbind) %do%
  {
    paste(paste(n, nrow(Table), sep = "/"),
          Table[n]$Title, sep = ": ") %>%
      print()
    # while Simiscrap merde do Simiscrap
    foo = try(SimiScrap(Table[n]))
    while (class(foo) ==  "try-error")
    {
      foo = try(SimiScrap(Table[n]))
    }
    return(foo)
  }

  colnames(output) = c("Ref", "Title", "Recs", "Rate")
  output$Recs = as.integer(output$Recs)
  output$Rate = as.integer(output$Rate)
  output$weight = output$Recs * (as.integer(output$Rate) - mean(Table$R))

  condition = output$Title %in% Table$Title
  FINAL2 = output[condition] %>%
    data.table()
  return(list(output, FINAL2))
}

source("anidb jugement comparatif/Franchises.R")

toto = Recommandation("http://graph.anime.plus/Altermedia/list,anime")

MAL = "http://graph.anime.plus/Altermedia/list,anime" %>% LIST() %>% .$Title

franchises %>%
  rename(Ref = An1,
         Title = An2) %>%
  anti_join(y = toto[[1]], by = c("Ref", "Title")) %>%
  mutate(Recs = 999) %>%
  bind_rows(toto[[1]]) %>%
  unique() %>%
  filter(Ref %in% MAL) %>%
  saveRDS(file = "anidb jugement comparatif/Reseau")
