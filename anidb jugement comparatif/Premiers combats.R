
choix =
  output %>% filter(N1 == 0) %>%
  droplevels() %>%
  distinct(Player)

if (nrow(choix) > 1) {
  choix %>%
    tidyr::crossing(Voisinage %>% rename(Player1=Player)) %>%
    group_by(Player) %>%
    filter(MIN == min(MIN)) %>% ungroup %>%
    select(contains("Player")) %>%
    split(f = .$Player) %>%
    map(ToGraph) %>%
    map(SELECT) %>%
    rbindlist() %>% BATTLE()
}

choix =
  output %>% filter(N0 == 0) %>%
  droplevels() %>%
  distinct(Player)

if (nrow(choix) > 1) {
  choix %>%
    tidyr::crossing(Voisinage %>% rename(Player1=Player)) %>%
    group_by(Player) %>%
    filter(MAX == max(MAX)) %>% ungroup %>%
    select(contains("Player")) %>%
    split(f = .$Player) %>%
    map(ToGraph) %>%
    map(SELECT) %>%
    rbindlist() %>% BATTLE()
}

non_fit =
  output %>% filter(is.na(infit) | is.na(outfit)) %>%
  droplevels() %>%
  pull(Player) %>%
  levels()

# data$Results %>%
#   filter(!An1 %in% non_fit, !An2 %in% non_fit) %>%
#   select(AN1, AN2, Score) %>%
#   write.table(
#     fileEncoding = "UTF-8",
#     file = "anidb jugement comparatif/Combats menes.csv",
#     append = F,
#     quote = F,
#     sep = "|",
#     row.names = F,
#     col.names = T
#   )

output %>%
  filter(se.theta > 2) %>% droplevels() %>%
  pull(Player) %>%
  levels() %>%
  set_names() %>%
  map(.f = NEIGHBOUR, Voisinage = Voisinage) %>%
  bind_rows(.id = "Ref") %>% split(f = .$Ref) %>%
  map(ToGraph) %>%
  map(SELECT) %>%
  rbindlist() %>%
  ToGraph() %>% simplify(remove.multiple = T, remove.loops = T) %>%
  Graph_To_Table() %>%
  BATTLE()

data = data$Table %>% Get_results()
