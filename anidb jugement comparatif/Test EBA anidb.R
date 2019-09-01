pacman::p_load(tidyverse, data.table)
read_excel_sheets = function(file) {
  require(readxl)
  require(purrr)
  Onglets = excel_sheets(path = file)
  Onglets %>%
    set_names %>%
    map(.f = ~ read_excel(path = file, sheet = .)) %>%
    return
}

input =
  "anidb jugement comparatif/TEST EBA.xlsx" %>% read_excel_sheets %>%
  bind_rows(.id = "Judge")

Liste = input %>% filter(Judge == "Feuil1") %>% unnest() %>% pull(Title)

JUDGING <- function(test) {
  test %>% select(Title, Vote) %>%
    mutate(id = dense_rank(Title)) %>%
    tidyr::crossing(., .) %>%
    filter(id < id1) %>%
    mutate(Score = case_when(Vote > Vote1 ~ 1,
                             Vote < Vote1 ~ 0,
                             Vote == Vote1 ~ .5)) %>%
    select(Player = Title, Player1 = Title1, Score) %>%
    return()
}

input =
  input %>%
  group_by(Judge) %>% nest %>%
  mutate(Jugements = map(.x = data, .f = JUDGING))

output = input %>% select(Judge, Jugements) %>% unnest() %>%
  filter(!is.na(Score))

Selection =
  tidyr::crossing(Player = Liste, Player1 = Liste) %>%
  inner_join(x = output, by = c("Player", "Player1"))

pacman::p_load(sirt)
FINAL =
  sirt::btm(
    data = Selection %>% select(-Judge) %>% data.table(),
    # judge = Selection$Judge,
    fix.eta = 0,
    conv = 1e-05,
    # maxiter = 100000,
    ignore.ties = F
  )

FINAL %>% summary()

FINAL$effects %>%
  select(Player = individual, Rating = theta) %>%
  mutate(Rating = scale(Rating)) %>%
  pull(Rating) %>% hist

"anidb jugement comparatif/FONCTIONS.R" %>% source()
COmparaison =
  FINAL$effects %>%
  select(Player = individual, Rating = theta) %>%
  SCORE_FINAL(STYLE = "sd", N = nrow(FINAL$effects) / 2) %>%
  mutate(Note = round(Note / max(Note) * 10, digits = 2)) %>%
  inner_join(y =
               input %>% select(data) %>% unnest %>%
               group_by(Player = Title) %>%
               summarise(Score = mean(as.numeric(Vote), na.rm = T)))

COmparaison %>%
  gather(key = VAR, value = VAL, Score, Note) %>%
  ggplot(mapping = aes(x = scale(VAL), fill = VAR)) +
  geom_density(alpha = .3)

plot =
  COmparaison %>%
  ggplot(mapping = aes(x = Note, y = Score, label = Player)) +
  geom_point() +
  geom_smooth()

pacman::p_load(plotly)
ggplotly(plot)
