# Fonctions ---------------------------------------------------------------
LIST <- function(url)
{
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
  STAR = "★"
  Table$Player = gsub(pattern = STAR,
                      replacement = " ",
                      x = Table$Player)
  Table$Player = gsub(
    pattern = Table[grep(pattern = "Ghoul ", Table$Player)]$Player,
    replacement = "Tokyo Ghoul vA",
    x = Table$Player
  )
  return(Table[, .(Player, Rating, Note, url)])
}

LISTER <- function(Table)
{
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

FIGHT <- function(Match)
{
  Match = data.table(Match)
  Match$Score = readline(paste(Match$An1, "VS", Match$An2, ": ", sep = " "))
  return(Match)
}

COMPARAISON <- function(Scores, SEUIL, Table)
{
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

ELO <- function(Table, Results)
{
  pacman::p_load(data.table)
  pacman::p_load(dplyr)

  N = 1
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

Standardisation_Score = function(Table)
{
  Table$Rating =
    qnorm(
      rank(Table$Rating, ties.method = "average") / (length(Table$Rating) + 1),
      mean = mean(Table$Rating),
      sd = sd(Table$Rating)
    )

  Table %>% Standardisation_Score_2() %>% return()
}

Standardisation_Score_2 <- function(Table)
{
  Table %>%
    mutate(Rating = Rating - min(Rating)) %>%
    mutate(Rating = 9.99 / max(Rating) * Rating) %>%
    mutate(Rating = (Rating + 1) %>% floor()) %>%
    return()
}


Order_past_matches <- function(Results)
{
  Fichier = Results %>%
    split(f = Results$Score)

  pacman::p_load(dplyr)
  Fichier1 = Fichier$`1`
  Fichier1$Score = Fichier1$Score %>% as.numeric()

  Fichier1_inverse =
    Fichier$`1` %>%
    select(An2, An1) %>%
    mutate(Score = 0) %>%
    data.table()


  Fichier2 =
    Fichier$`2` %>%
    select(An2, An1) %>%
    mutate(Score = 1) %>%
    data.table()

  Fichier2_inverse =
    Fichier$`2` %>%
    select(An1, An2) %>%
    mutate(Score = 0) %>%
    data.table()

  names(Fichier1) = names(Results)
  names(Fichier1_inverse) = names(Results)
  names(Fichier2) = names(Results)
  names(Fichier2_inverse) = names(Results)

  output =
    bind_rows(Fichier1, Fichier1_inverse, Fichier2, Fichier2_inverse) %>%
    data.table()

  return(output)
}

# Récupérer la liste
SCORING <- function(url)
{
  print("Nouveaux scores")
  pacman::p_load(data.table)
  Table = LIST(url)
  colnames(Table) = c("Player", "Rating", "Note", "url")

  Results = fread(
    input = "anidb jugement comparatif/Combats menes.csv",
    sep = "|",
    encoding = "UTF-8",
    colClasses = "character"
  ) %>%
    mutate_all(trimws) %>%
    filter(Score %in% 1:2,
           An1%in%Table$Player,
           An2%in%Table$Player)


  # Calculer le nouveau Elo
  Table = ELO(Table, Results)
  nom_Table = names(Table)
  return(Table)
}

SOULIST <- function(Table, Sous_Liste)
{
  nom_Table = names(Table)
  pacman::p_load(dplyr)
  toto = right_join(x = Table,
                    y = LIST(url),
                    by = "Player") %>%
    data.table()

  toto[is.na(toto$Rating.x)]$Rating.x = 0

  # Découper la liste par note
  Table = toto[order(Player), .(Player, Rating.x)]
  names(Table) = nom_Table

  Results = fread(
    input = "anidb jugement comparatif/Combats menes.csv",
    sep = "|",
    encoding = "UTF-8",
    colClasses = "character"
  ) %>% unique()

  Sous_Liste =
    Sous_Liste %>%
    anti_join(Results, by = c("An1", "An2")) %>%
    inner_join(y = Table, by = c("An1" = "Player")) %>%
    inner_join(y = Table, by = c("An2" = "Player")) %>%
    mutate(
      Rating.x = (Rating.x - mean(Rating.x)) / sd(Rating.x),
      Rating.y = (Rating.y - mean(Rating.y)) / sd(Rating.y)
    ) %>%
    mutate(Gap = abs(Rating.x - Rating.y)) %>%
    arrange(Gap) %>%
    # slice(1:10) %>%
    select(An1, An2) %>%
    data.table()
  return(Sous_Liste)
}

New_Round = function(Sous_Liste)
{
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
  write.table(
    x = Resultats,
    file = "anidb jugement comparatif/Combats menes.csv",
    append = T,
    quote = F,
    sep = "|",
    row.names = F,
    col.names = T,
    fileEncoding = "UTF-8"
  )
}

FINALE <- function(url)
{
  Table =
    url %>%
    SCORING()

  output =
    Table %>% Standardisation_Score() %>%
    inner_join(LIST(url) %>% select(Player, Rating), by = "Player") %>%
    filter(Rating.x != Rating.y)

  output %>%
    print()

}

Enough_desuka <- function(Table)
{
  pacman::p_load(data.table)

  LISTES =
    Table %>%
    mutate(Groupe =
             cut(x = Table$Rating,
                 breaks = 10))

  LISTES =
    LISTES %>%
    group_by(Groupe) %>%
    count() %>%
    filter(n > 1) %>%
    inner_join(y = LISTES, by = "Groupe")

  pacman::p_load(foreach)
  Sous_Liste =
    foreach(liste = split(x = LISTES, f = as.character(LISTES$Groupe))) %do%
    {
      SOULIST(Table = Table[, .(Player, Rating)], Sous_Liste = LISTER(liste)) %>%
        slice(1:length(liste))
    } %>%
    rbindlist()

  return(Sous_Liste)
}

SCRIPT = function()
{
  Related =
    fread(
      input = "anidb jugement comparatif/Combats menes.csv",
      sep = "|",
      encoding = "UTF-8",
      colClasses = "character"
    ) %>% anti_join(x = foo)

  while (Related %>% nrow() > 0)
  {
    New_Round(Sous_Liste = Related %>% sample_n(1))
    Related =
      fread(
        input = "anidb jugement comparatif/Combats menes.csv",
        sep = "|",
        encoding = "UTF-8",
        colClasses = "character"
      ) %>% anti_join(x = foo)
  }

  Oldies =
    fread(
      input = "anidb jugement comparatif/Combats menes.csv",
      sep = "|",
      encoding = "UTF-8",
      colClasses = "character"
    ) %>% select(An1, An2) %>% as.list() %>% flatten_chr() %>% unique()

  Newbies =
    Table %>%
    filter(!Player %in% Oldies)

  if (nrow(Newbies) > 1) {
    Newbies = Newbies %>% LISTER()
  } else{
    Newbies = NULL %>% as.data.table()
  }

  while (Newbies %>% nrow > 0)
  {
    Newbies %>% New_Round()
    Newbies =
      fread(
        input = "anidb jugement comparatif/Combats menes.csv",
        sep = "|",
        encoding = "UTF-8",
        colClasses = "character"
      ) %>% anti_join(x = Newbies)

  }
}

SCORE_FINAL <- function(url) {
  TABLE = url %>% SCORING()

  Rating =
    TABLE %>% mutate(Rank = min_rank(Rating) / (1+nrow(.)))  %>%
    .$Rank %>% qnorm(mean = 0, sd = 1)

  Rating = Rating - min(Rating)
  Rating = (Rating / max(Rating)) * 9.99

  TABLE$Note = (1 + Rating) %>% floor()

  TABLE %>% return()
}

SCRIPT2 <- function(url) {
  Table = SCORING(url)

  "Import des résultats" %>% print

  Results = fread(
    input = "anidb jugement comparatif/Combats menes.csv",
    sep = "|",
    encoding = "UTF-8",
    colClasses = "character"
  ) %>%
    mutate_all(trimws) %>%
    filter(Score %in% 1:2,
           An1%in%Table$Player,
           An2 %in% Table$Player) %>%
    data.table()

  Combats =
    tidyr::crossing(Table, Table) %>%
    rename(An1 = Player, An2 = Player1) %>%
    inner_join(LISTER(Table), by = c("An1", "An2")) %>%
    mutate(Delta = abs(Rating - Rating1)) %>%
    select(-contains("Rating")) %>%
    anti_join(y = Results, by = c("An1", "An2")) %>%
    arrange(Delta) %>%
    data.table()

  "Choix de la sélection" %>% print
  Selection =
    bind_rows(
      Results %>%
        group_by(Title = An1) %>% count(),
      Results %>%
        group_by(Title = An2) %>% count()
    ) %>%
    summarise(n = sum(n) %>% as.double()) %>%
    left_join(x = Table %>% select(Title = Player)) %>%
    mutate(n = case_when(is.na(n) ~ 0 ,
                         TRUE ~ n)) %>%
    arrange(n) %>%
    mutate(Goal = lead(n))


  Nb_matchs =
    Selection %>% mutate(Match = Goal - n) %>%
    filter(n == min(n)) %>%
    summarise(Match = max(Match)) %>% pull(Match)

  Selection = Selection %>% filter(n == min(n)) %>% pull(Title)

  Selection %>%
    map(.f = ~ filter(.data = Combats,
                      An1%in%. | An2 %in% .)) %>%
    map(.f = arrange, Delta) %>%
    map(.f = slice, 1:Nb_matchs) %>%
    bind_rows() %>% unique %>% split(f = .$Delta) %>%
    map(.f = select, -Delta) %>%
    map(New_Round)

}

CHANGES <- function(url) {
  FINAL =
    url %>% SCORE_FINAL() %>%
    inner_join(LIST(url) %>% select(Player, Avant = Rating)) %>%
    mutate(Gap = abs(Note - Avant))

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
    geom_abline()
  print(plot)

  To_Change %>% return()
}
