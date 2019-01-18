pacman::p_load(tidyverse, data.table)
read_excel_sheets = function(file){
  require(readxl)
  require(purrr)
  Onglets = excel_sheets(path = file)
  Onglets %>%
    set_names %>%
    map(.f = ~read_excel(path = file, sheet = .)) %>%
    return
}

input = "TEST EBA.xlsx" %>% read_excel_sheets
Liste = input$Feuil1 %>% filter(!is.na(Vote)) %>% pull(Title)

JUDGING <- function(test) {
  test %>% select(Player = Title, Rating = Vote) %>%
    mutate(Rating = case_when(is.na(Rating) ~ 0,
                              TRUE ~ Rating %>% as.double)) %>%
    crossing(.,.) %>%
    filter(Player != Player1,
           Rating > Rating1) %>%
    mutate(Score = 1) %>%
    select(-contains("Rating")) %>%
    return
}

output =
  input %>% map(.f = filter, Title %in% Liste) %>%
  map_df(JUDGING) %>%
  group_by(Player, Player1) %>% summarise(Score = sum(Score)) %>%
  # filter(Player %in% Liste & Player1 %in% Liste) %>%
  data.table

output =
  cbind.data.frame(Player = Liste)%>%
  left_join(output, by = "Player") %>%
  spread(key = Player1, value = Score, fill = 0) %>%
  select(-`<NA>`)

pacman::p_load(eba)
Rank =
  output %>% select(-Player) %>%
  thurstone

foo = Rank$estimate %>% as.data.frame

foo =
  row.names(foo) %>%
  cbind.data.frame(Title = ., Rating = foo$.) %>%
  mutate(Rank = dense_rank(desc(Rating)),
         Score = Rating)

foo$Rating = foo$Rating - min(foo$Rating)
foo$Rating = foo$Rating / max(foo$Rating) * 9
foo$Rating = foo$Rating + 1
foo$Rating = foo$Rating %>% round(digits = 0)

foo$Rating %>% hist
foo %>% split(x = .$Title %>% as.character, f = .$Rating)
