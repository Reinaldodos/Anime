Results =
  data$Results %>% select(An1, An2) %>% ToGraph() %>%
  igraph::simplify(remove.multiple = T, remove.loops = T)

Franchise %>% igraph::difference(Results) %>%
  Graph_To_Table() %>%
  BATTLE()

data = url %>% LIST() %>% Get_results()
