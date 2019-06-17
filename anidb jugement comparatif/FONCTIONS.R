BASE64ENCODE = function(string) {
  pacman::p_load(base64enc)
  string %>% charToRaw() %>% base64encode() %>% return()
}

BATTLE <- function(Table, Clusters, Results) {
  safe_COMBAT = safely(COMBATTANTS)

  Bataille_Clusters =
    Table %>%
    inner_join(Clusters, by = "Player") %>% split(f = .$Groupe) %>%
    map(.f = safe_COMBAT, Results = Results) %>% compact %>%
    map_df(.f = ~ .$result) %>% unique()

  Bataille = Table %>% COMBATTANTS(Results = Results)

  bind_rows(Bataille, Bataille_Clusters) %>%
    select(contains("An"), contains("Base")) %>% unique() %>%
    return()
}

CHANGES <- function(url) {
  TABLE = url %>% SCORING()
  FINAL =
    c("sd",
      "equal",
      "pretty",
      "hclust",
      "bclust",
      "quantile",
      "kmeans",
      "jenks",
      "fisher") %>%
    set_names() %>%
    map(.f = SCORE_FINAL, TABLE = TABLE) %>%
    bind_rows(.id = "style")  %>%
    count(Player,Rating, Note) %>%
    group_by(Player) %>% filter(n == max(n)) %>% ungroup %>%
    inner_join(LIST(url) %>% select(Player, Avant = Rating)) %>%
    mutate(Gap = abs(Note - Avant)) %>%
    select(-n)

  FINAL = FINAL %>% group_by(Player) %>% filter(Note==max(Note)) %>% ungroup

  To_Change = FINAL %>% filter(Gap != 0)

  plot =
    FINAL %>%
    ggplot() +
    geom_histogram(mapping = aes(x = Note)) +
    geom_histogram(mapping = aes(x = Avant),
                   fill = "red",
                   alpha = 0.2)
  print(plot)

  plot =
    To_Change %>%
    count(Avant, Note) %>%
    ggplot() +
    geom_point(mapping = aes(x = Avant, y = Note, size = n)) +
    geom_abline() +
    xlim(0, 10) + ylim(0, 10)
  print(plot)

  To_Change %>% View()

  FINAL %>% return()
}

CLUSTERING = function(Table, url){
  pacman::p_load(igraph)
  table_graphe =
    readRDS(file =
              "anidb jugement comparatif/Reseau") %>%
    filter(Title %in% Table$Player)

  graphe =
    table_graphe %>% select(Ref, Title) %>% as.matrix() %>%
    graph.edgelist(directed = F)

  E(graphe)$weight = table_graphe$Recs

  Clusters =
    graphe %>%
    cluster_louvain() %>%
    communities() %>%
    map(.f = ~ data.table(Player = .)) %>%
    bind_rows(.id = "Groupe") %>%
    left_join(x = Table, by = "Player") %>%
    mutate(Groupe = case_when(is.na(Groupe) ~ 0,
                              TRUE ~ Groupe %>% as.numeric())) %>%
    inner_join(LIST(url) %>% select(Player), by = "Player")

  Clusters %>% select(Groupe, Player) %>% return()
}

COMBATTANTS <- function(Table, Results) {
  pacman::p_load(tidyr)
  Combats =
    tidyr::crossing(Table, Table) %>% data.table %>%
    rename(An1 = Player, An2 = Player1) %>%
    inner_join(LISTER(Table), by = c("An1", "An2")) %>%
    mutate(Delta = abs(Rating1 - Rating)) %>%
    arrange(Delta)

  Table$Player %>% set_names() %>%
    map(.f = ~filter(.data = Combats, An1==.| An2==.)) %>%
    map(.f = ~mutate(.data = .,N = dense_rank(Delta))) %>%
    bind_rows%>%
    anti_join(y = Results, by = c("An1", "An2")) %>%
    filter(N==min(N)) %>%
    unique() %>%
    return()
}

COMPARAISON <- function(Scores, SEUIL, Table) {
  input =
    bind_rows(Scores$`1` %>%
                rename(Win = An1, Lose = An2),
              Scores$`2` %>%
                rename(Win = An2, Lose = An1)) %>%
    group_by(Win, Lose) %>%
    count() %>% ungroup() %>%
    left_join(x = expand.grid(Win = Table$Player,
                              Lose = Table$Player),
              by = c("Win", "Lose")) %>%
    filter(Win != Lose) %>%
    arrange(Win, Lose) %>%
    mutate(n = if_else(
      condition = is.na(n),
      true = 0,
      false = SEUIL * as.numeric(n)
    )) %>%
    spread(key = Lose, value = n) %>%
    data.table()

  pacman::p_load(eba)
  Rank =
    # eba(M = input %>% select(-Win))
    thurstone(M = input %>% select(-Win))
  return(Rank)
}

ELO <- function(Table, Results) {
  pacman::p_load(data.table)
  pacman::p_load(dplyr)

  N = 1
  "Nouveaux scores" %>% cat(sep = "\n")

  Rank = COMPARAISON(Scores = Results %>%
                       split(f = .$Score), SEUIL = N, Table = Table)

  Rank$goodness.of.fit %>% print()

  foo =
    Rank$estimate %>% as.data.frame()

  foo =
    row.names(foo) %>%
    cbind.data.frame(foo$.) %>%
    left_join(x = Table, by = c("Player" = ".")) %>%
    select(Player, `foo$.`) %>%
    rename(Rating = `foo$.`) %>%
    data.table()

  foo[is.na(Rating)]$Rating = 0
  return(foo)
}

FIGHT <- function(Match) {
  Match = data.table(Match)
  Match$Score = readline(paste(Match$An1, "VS", Match$An2, ": ", sep = " "))
  return(Match)
}

LISTER <- function(Table) {
  pacman::p_load(dplyr)
  Table = arrange(Table, Player)
  if (nrow(Table) <= 1)
  {
    return(NULL)
  } else {
    Liste = Table$Player

    pacman::p_load(foreach)
    Coupe = foreach(anime = Liste, .combine = rbind) %do%
      cbind.data.frame(anime, Liste[match(anime, table = Liste):length(Liste)])  %>%
      data.table()
    colnames(Coupe) = c("An1", "An2")

    condition = Coupe$An1 == Coupe$An2

    Draw = Coupe[!condition]
  }
  return(Draw)
  # return(Draw[1:min(nrow(Table), nrow(Draw))])
}

New_Round = function(Sous_Liste) {
  Sous_Liste =
    Sous_Liste %>%
    filter(!is.na(An1)) %>%
    sample_n(size = 1) %>%
    data.table()

  pacman::p_load(rvest)
  # Récupérer les scores de chaque match
  pacman::p_load(foreach)
  Resultats = foreach(match = 1:nrow(Sous_Liste)) %do%
    FIGHT(Match = Sous_Liste[match]) %>%
    rbindlist()

  # les accoler à la réserve
  # sauver la réserve
  Resultats %>%
    select(AN1, AN2, Score) %>%
    write.table(
    fileEncoding = "UTF-8",
    file = "anidb jugement comparatif/Combats menes.csv",
    append = T,
    quote = F,
    sep = "|",
    row.names = F,
    col.names = T
  )
}


SCORE_FINAL <- function(TABLE, STYLE) {
  pacman::p_load(classInt)

  Classes = classIntervals(var = TABLE$Rating,
                           n = 10,
                           style = STYLE)

  mutate(.data = TABLE,
           Groupe = cut(x = Rating,
                                   breaks = Classes$brks,
                                   include.lowest = T)) %>%
          mutate(Note = dense_rank(Groupe)) %>%
          mutate(Note = Note + 10 - max(Note)) %>%
    return()
}

SCORING <- function(url) {
  pacman::p_load(data.table)
  Table = LIST(url)
  input = Table %>% Get_results()

  # Calculer le nouveau Elo
  ELO(Table = Table, Results = input$Results) %>%
    return()
}

SCRIPT2 <- function(url) {
  Table = LIST(url)
  input = Table %>% Get_results()

  Table =
    ELO(input$Table, input$Results) %>%
    mutate(Rating = scale(x = Rating,
                          center = TRUE,
                          scale = TRUE)) %>%
    left_join(x = input$Table %>% select(-Rating), by = "Player") %>%
    data.table() %>%
    mutate(Rating = case_when(is.na(Rating) ~ 0 ,
                              TRUE ~ Rating))

  BATTLE(Table = Table,
         Clusters = CLUSTERING(Table = Table,
                               url = url),
         Results = input$Results) %>%
    rename(AN1 = Base64, AN2 = Base641) %>%
    split(f = paste(.$An1, .$An2)) %>%
    sample(size = length(.)) %>%
    walk(New_Round)
}

Get_results <- function(Table) {
  Table$Base64 =
    Table$Player %>% map_chr(BASE64ENCODE)

  "Import des résultats" %>% cat(sep = "\n")

  Results =
    fread(input = "anidb jugement comparatif/Combats menes.csv",
          sep = "|",
          encoding = "UTF-8",
          colClasses = "character") %>%
    mutate_all(trimws)  %>%
    inner_join(y = Table %>% select(AN1 = Base64, An1 = Player),
               by = "AN1") %>%
    inner_join(y = Table %>% select(AN2 = Base64, An2 = Player),
               by = "AN2")

  list(Table = Table, Results = Results) %>%
    return()
}


NEWBIES <- function(input) {
  Newbies =
    input$Results %>% select(An1, An2) %>%
    as.list() %>% flatten_chr() %>% data.table(Player = .) %>%
    count(Player) %>%
    left_join(x = input$Table %>% distinct(Player), by = "Player") %>%
    filter(is.na(n))

  BATTLE(
    Table = input$Table,
    Results = input$Results,
    Clusters = CLUSTERING(Table = input$Table, url = url)) %>%
    filter(An1 %in% Newbies$Player | An2 %in% Newbies$Player) %>%
    rename(AN1 = Base64, AN2 = Base641) %>%
    split(f = paste(.$An1, .$An2)) %>%
    sample(size = length(.)) %>%
    walk(New_Round)

}
