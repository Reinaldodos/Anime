# Import des données ------------------------------------------------------
Table = LIST(url)
input = Table %>% Get_results()
Table = ELO(Table = input$Table, Results = input$Results)

Combats =
  Table %>% select(Player, Rating) %>%
  tidyr::crossing(Table %>% select(Player, Rating)) %>%
  # tidyr::expand_grid(Table %>% select(Player1 = Player, Rating1 = Rating))%>%
  inner_join(x = LISTER(Table),
             by = c("An1" = "Player", "An2" = "Player1")) %>%
  mutate(PREV = case_when(Rating > Rating1 ~ 1,
                          Rating1 > Rating ~ 2))

# Exclusion des combats incohérents ---------------------------------------
SELECAO =
  input$Results %>%
  inner_join(y = Combats, by = c("An1", "An2"))  %>%
  filter(Score == PREV) %>% unique()

"A exclure: " %>% cat(sep = "\n")
anti_join(x = input$Results,
          y = SELECAO,
          by = c("An1", "An2", "Score")) %>%
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


