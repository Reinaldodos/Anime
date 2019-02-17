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

Sparse =
  "anidb jugement comparatif/TEST EBA.xlsx" %>% read_excel_sheets %>%
  bind_rows(.id = "User") %>%
  mutate(Vote = Vote %>% as.numeric) %>%
  filter(!is.na(Vote)) %>%
  group_by(User) %>%
  mutate(Note = scale(x = Vote, center = T, scale = T)) %>%
  select(User, Title, Note, Vote) %>%
  data.table()

Liste =
  Sparse %>%
  split(x = .$Title, f = .$User) %>% .$Feuil1

DATA =
  Sparse %>%
  # count(Title) %>%
  # filter(n >= 3) %>% select(-n) %>%
  # inner_join(x=Sparse) %>%
  filter(Title %in% Liste) %>%
  select(User, Title, Rating = Vote)

FUNKYSVD <- function(K, Table) {
  pacman::p_load(recommenderlab)
  DATA=
    Table %>%
    spread(key = User, value = Rating) %>%
    data.table()

  fsvd =
    DATA %>% select(-Title) %>%
    as.matrix() %>%
    funkSVD(
      verbose = F,
      k = K,
      min_improvement = 1e-05,
      max_epochs = 1000
    )

  Recommender = tcrossprod(fsvd$U, fsvd$V) %>%  data.table()
  names(Recommender) = Table$User %>% unique()
  Recommender$Title = Table$Title %>% unique()

  Recommender =
    Recommender %>%
    gather(key = User, value = Score, -Title)

  Recommender %>%
    inner_join(y = Table,
               by = c("Title", "User")) %>%
    summarise(RMSE = RMSE(true = Rating, predicted = Score)) %>% as.numeric() %>%
    cat(K, ": RMSE = ", ., "\n", sep = "")

  Recommender %>%
    left_join(y = Table,
               by = c("Title", "User")) %>%
    return()
}

Recommenders =
  1:8 %>% set_names() %>%
  map(.f = FUNKYSVD, Table = DATA)


Recommenders$`8` %>%
  left_join(y = Sparse, by = c("Title", "User")) %>%
  ggplot(mapping = aes(
    x = Vote,
    y = Score,
    colour = User
  )) +
  geom_point() +
  geom_abline() +
  facet_wrap(User ~ .)

