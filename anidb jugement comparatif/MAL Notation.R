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

# Newbies -----------------------------------------------------------------
Newbies =
  data$Results %>% select(An1, An2) %>%
  as.list() %>% flatten_chr() %>% data.table(Player = .) %>%
  count(Player) %>%
  anti_join(x = data$Table %>% distinct(Player), by = "Player")

while(nrow(Newbies) > 0) {
  Newbies %>%
    tidyr::crossing(data$Table) %>%
    select(An1 = Player, An2 = Player1) %>%
    ToGraph() %>%
    SELECT() %>%
    BATTLE()

  data = data$Table %>% Get_results()
  Newbies =
    data$Results %>% select(An1, An2) %>%
    as.list() %>% flatten_chr() %>% data.table(Player = .) %>%
    count(Player) %>%
    anti_join(x = data$Table %>% distinct(Player), by = "Player")
}


# Adaptive Comaprative Judgement ------------------------------------------
repeat{
  data = data$Table %>% Get_results()
  output = ELO(Table = data$Table, Results = data$Results)

  Voisinage =
    output %>%
    group_by(Player) %>%
    summarise(MIN = Rating - se.theta,
              MAX = Rating + se.theta)

  Batch =
    output %>%
    filter(se.theta == max(se.theta)) %>%
    pull(Player) %>%
    as.character() %>%
    set_names() %>%
    map(.f = NEIGHBOUR, Voisinage = Voisinage) %>%
    bind_rows(.id = "Ref") %>%
    ToGraph() %>%
    SELECT()

  Batch %>% BATTLE()
}
