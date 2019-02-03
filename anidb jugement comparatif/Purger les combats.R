# Import des données ------------------------------------------------------
Table = SCORING(url)

Table$Base64 =
  Table$Player %>% map_chr(BASE64ENCODE)

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

Combats =
  tidyr::crossing(Table, Table) %>%
  mutate(Delta = abs(Rating - Rating1)) %>%
  inner_join(x = LISTER(Table),
             by = c("An1" = "Player", "An2" = "Player1"))

# Exclusion des combats incohérents ---------------------------------------
SELECAO =
  Results %>%
  inner_join(y = Combats, by = c("An1", "An2")) %>%
  mutate(PREV = case_when(Rating > Rating1 ~ 1,
                          Rating1 > Rating ~ 2)) %>%
  filter(Score == PREV) %>% unique()

"A exclure: " %>% cat(sep = "\n")
anti_join(x = Results, y = SELECAO, by = c("An1", "An2", "Score")) %>%
  select(An1, An2, Score) %>%
  print

SELECAO %>% select(AN1, AN2, Score) %>%
  write.table(
    file = "anidb jugement comparatif/Combats menes.csv",
    append = F,
    quote = F,
    sep = "|",
    row.names = F,
    col.names = T,
    fileEncoding = "UTF-8"
  )
