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

Relation =
  bind_rows(foo %>%
              group_by(Title = An1) %>% count(),
            foo %>%
              group_by(Title = An2) %>% count()) %>%
  summarise(Nb = sum(n)) %>%
  filter(Title %in% Table$Player)

Selection =
  bind_rows(Results %>%
      group_by(Title = An1) %>% count(),
    Results %>%
      group_by(Title = An2) %>% count()) %>%
  summarise(n = sum(n)) %>%
  filter(Title %in% Table$Player)

SELECAO =
  Table %>% left_join(y = Selection, by = c("Player" = "Title")) %>%
  split(f = .$Player) %>%
  map(
    .f = ~ tidyr::crossing(., Table) %>%
      filter(Player != Player1) %>%
      mutate(Delta = abs(Rating - Rating1)) %>%
      mutate(Rank = dense_rank(Delta)) %>%
      filter(Rank <= max(Relation$Nb))
  ) %>% bind_rows()


test =
  bind_rows(semi_join(x = Results,
                      y = SELECAO,
                      by = c("An1" = "Player", "An2" = "Player1")),
            semi_join(x = Results,
                      y = SELECAO,
                      by = c("An2" = "Player", "An1" = "Player1")),
            inner_join(foo, Results)) %>% unique()

bind_rows(test %>%
            group_by(Title = An1) %>% count(),
          test %>%
            group_by(Title = An2) %>% count()) %>%
  summarise(n = sum(n)) %>%
  filter(Title %in% Table$Player) %>%
  arrange(n) %>% View()

test %>%
  write.table(
    file = "anidb jugement comparatif/Combats menes.csv",
    append = F,
    quote = F,
    sep = "|",
    row.names = F,
    col.names = T,
    fileEncoding = "UTF-8"
  )
