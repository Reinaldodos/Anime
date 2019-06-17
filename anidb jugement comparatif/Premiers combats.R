Table = url %>% CHANGES()

Selection =
  Table %>% count(Gap) %>% filter(n<10) %>%
  inner_join(Table)%>% pull(Player)

input = Table %>% Get_results()

Table =
  input$Table %>%
  mutate(Rating = scale(x = Rating,
                        center = TRUE,
                        scale = TRUE)) %>%
  # left_join(x = input$Table %>% select(-Rating), by = "Player") %>%
  data.table() %>%
  mutate(Rating = case_when(is.na(Rating) ~ 0 ,
                            TRUE ~ Rating))


Premiers_combats =
  BATTLE(Table = Table,
       Clusters = CLUSTERING(Table = Table ,
                             url = url),
       Results = input$Results) %>%
  rename(AN1 = Base64, AN2 = Base641) %>%
  filter(An1 %in% Selection | An2 %in% Selection) %>%
  split(f = paste(.$An1, .$An2)) %>%
  sample(size = length(.))

Premiers_combats %>% walk(New_Round)
