# Import des données ------------------------------------------------------
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

Relation = inner_join(Results, foo, by = c("An1", "An2"))
Results = anti_join(x = Results,
                    y = foo,
                    by = c("An1", "An2"))

Combats =
  crossing(Table, Table) %>%
  mutate(Delta = abs(Rating - Rating1)) %>%
  inner_join(x = LISTER(Table),
             by = c("An1" = "Player", "An2" = "Player1"))

TEST_KA = readline(prompt = "Réduire la voilure? o/n \t")

# Nettoyage des combats menés hors Relations ------------------------------
if (TEST_KA == "o") {
  test =
    Combats %>%
    inner_join(y = Results, by = c("An1", "An2"))

  test =
    Table$Player %>% set_names() %>%
    map(.f = ~ filter(.data = test,
                      An1 %in% . | An2 %in% .)) %>%
    map(.f = mutate, Cut = mean(Delta, na.rm = TRUE)) %>%
    map(.f = filter, Delta < Cut) %>%
    bind_rows() %>%
    select(names(test))

  Results = test %>% distinct(An1, An2, Score)
}

# Exclusion des combats incohérents ---------------------------------------
SELECAO =
  Results %>%
  bind_rows(Relation) %>%
  inner_join(y = Combats, by = c("An1", "An2")) %>%
  mutate(PREV = case_when(Rating > Rating1 ~ 1,
                          Rating1 > Rating ~ 2)) %>%
  filter(Score == PREV) %>% unique()

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
