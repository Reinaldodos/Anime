# Import des données ------------------------------------------------------
Table = SCORING(url)

Results = fread(
  input = "anidb jugement comparatif/Combats menes.csv",
  sep = "|",
  encoding = "UTF-8",
  colClasses = "character"
) %>%
  mutate_all(trimws) %>%
  filter(Score %in% 1:2,
         An1 %in% Table$Player,
         An2 %in% Table$Player) %>%
  data.table()

Combats =
  crossing(Table, Table) %>%
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
anti_join(x = Results, y = SELECAO, by = c("An1", "An2", "Score")) %>% print

SELECAO %>% select(names(Results)) %>%
  write.table(
    file = "anidb jugement comparatif/Combats menes.csv",
    append = F,
    quote = F,
    sep = "|",
    row.names = F,
    col.names = T,
    fileEncoding = "UTF-8"
  )
