Results =
  data$Results %>% select(An1, An2) %>% ToGraph() %>%
  simplify(remove.multiple = T, remove.loops = T)

Franchise=
  expand_grid(Player=data$Table$Player, Ref = data$Table$Player) %>%
  ToGraph() %>%
  intersection(Franchise)

Franchise %>% igraph::difference(Results) %>%
  Graph_To_Table() %>%
  BATTLE()
