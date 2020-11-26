
Resultats =
  data$Results %>%
  select(An1, An2) %>%
  tidygraph::as_tbl_graph(directed = F)

Selection = igraph::difference(Reseaux, Resultats)
Batch = output %>% top_n(n = 1, wt = se.theta) %>% droplevels()

Battlerezo =
  tidyr::crossing(Batch$Player, output$Player) %>%
  tidygraph::as_tbl_graph(directed = F) %>%
  intersection(Selection) %>%
  Graph_To_Table()

if(nrow(Battlerezo)==0) {
  Battlerezo =
    Batch %>% NEIGHBOUR(output = output) %>%
    ToGraph() %>% Graph_To_Table()
}

if(nrow(Battlerezo)>0) {
  Battlerezo %>% BATTLE()
} else {
  break
}


