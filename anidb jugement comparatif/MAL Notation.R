pacman::p_load(tidyverse, rio, data.table)
source("anidb jugement comparatif/FONCTIONS.R", encoding = "UTF-8")

# récupérer les données
url = "http://graph.anime.plus/Altermedia/list,anime"
data = url %>% LIST() %>% Get_results()
# récupérer les relations
Reseau =
  "anidb jugement comparatif/Reseau" %>% read_rds() %>%
  filter(Ref%in%data$Table$Player)

Franchise = Reseau %>% filter(Recs == max(Recs))
Reseau = anti_join(x = Reseau, y = Franchise)

# graphisation
Franchise =
  Franchise %>% filter(Title != "") %>%
  tidygraph::as_tbl_graph(directed = F)%>%
  igraph::simplify(remove.multiple = T, remove.loops = T)

Reseau =
  Reseau %>%
  tidygraph::as_tbl_graph(directed = F)%>%
  igraph::simplify(remove.multiple = T, remove.loops = T)

Reseaux =
  igraph::union(Franchise, Reseau) %>%
  igraph::simplify(remove.multiple = T, remove.loops = T)

source(file = "anidb jugement comparatif/Full franchises.R")

Newbies = data.table(test = "test")

while (nrow(Newbies) > 0) {
  # Newbies -----------------------------------------------------------------
  Newbies =
    data$Results %>% select(An1, An2) %>%
    as.list() %>% flatten_chr() %>% data.table(Player = .) %>%
    count(Player) %>%
    anti_join(x = data$Table %>% distinct(Player), by = "Player")

  if (nrow(Newbies) > 0) {
    Newbies_Franchise =
      tidyr::crossing(Ref = Newbies$Player,
                      Player = data$Table$Player) %>%
      ToGraph() %>%
      igraph::intersection(Reseaux) %>%
      Graph_To_Table()
  } else{
    Newbies_Franchise = Newbies
  }

  if (nrow(Newbies_Franchise) > 0) {
    Newbies_Franchise %>% BATTLE()
  }

  if (nrow(Newbies) > 1) {
    Newbies %>%
      tidyr::crossing(Player1 = Newbies$Player) %>%
      select(An1 = Player, An2 = Player1) %>% split(f = .$An1) %>%
      map(ToGraph) %>%
      map(SELECT) %>%
      rbindlist() %>%
      ToGraph() %>% simplify(remove.multiple = T, remove.loops = T) %>%
      Graph_To_Table() %>%
      BATTLE()
  }

  if (nrow(Newbies) == 1) {
    Newbies %>%
      tidyr::crossing(Player1 = data$Table$Player) %>%
      select(An1 = Player, An2 = Player1) %>% split(f = .$An1) %>%
      map(ToGraph) %>%
      map(SELECT) %>%
      rbindlist() %>%
      ToGraph() %>% simplify(remove.multiple = T, remove.loops = T) %>%
      Graph_To_Table() %>%
      BATTLE()
  }
  data = data$Table %>% Get_results()
}

repeat {
  # Adaptive Comparative Judgement ------------------------------------------
  data = data$Table %>% Get_results()
  output = ELO(Table = data$Table, Results = data$Results)
  output %>% arrange(-se.theta) %>% view()

  source("anidb jugement comparatif/Nouvelles batailles.R")

}

source("anidb jugement comparatif/Score final.R")
# source("anidb jugement comparatif/Purger les combats.R")
