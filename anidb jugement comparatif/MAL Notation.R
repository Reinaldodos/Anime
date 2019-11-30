
pacman::p_load(tidyverse, rio, data.table)
source("anidb jugement comparatif/FONCTIONS.R", encoding = "UTF-8")

# récupérer les données
url = "http://graph.anime.plus/Altermedia/list,anime"
data = url %>% LIST() %>% Get_results()
# récupérer les relations
Reseau = "anidb jugement comparatif/Reseau" %>% read_rds()
Franchise = Reseau %>% filter(Recs == max(Recs))
Reseau = anti_join(x = Reseau, y = Franchise)

# graphisation
Franchise =
  Franchise %>% filter(Title != "") %>%
  select(-Recs) %>%
  ToGraph()

Reseau =
  Reseau %>%
  select(-Recs) %>%
  ToGraph()

Newbies = data.table(test = "test")
while(nrow(Newbies) > 0) {
  # Newbies -----------------------------------------------------------------
  Newbies =
    data$Results %>% select(An1, An2) %>%
    as.list() %>% flatten_chr() %>% data.table(Player = .) %>%
    count(Player) %>%
    anti_join(x = data$Table %>% distinct(Player), by = "Player")

  if (nrow(Newbies) > 1) {
    Newbies %>%
      tidyr::crossing(Player1=Newbies$Player) %>%
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

  Voisinage =
    output %>%
    group_by(Player) %>%
    summarise(MIN = Rating - se.theta,
              MAX = Rating + se.theta)

  Batch =
    output %>%
    top_n(n = 1, wt = se.theta) %>% droplevels()
  if (Batch$se.theta <= 2) {
    Batch %>%
      pull(Player) %>%
      levels() %>%
      set_names() %>%
      map(.f = NEIGHBOUR, Voisinage = Voisinage) %>%
      bind_rows(.id = "Ref") %>% split(f = .$Ref) %>%
      map(ToGraph) %>%
      map(SELECT2) %>%
      rbindlist() %>% BATTLE()
  } else {
    source("anidb jugement comparatif/Premiers combats.R",
           encoding = "UTF-8")
  }

  source("anidb jugement comparatif/Score final.R")

}
