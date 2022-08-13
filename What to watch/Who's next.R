pacman::p_load(tidyverse, ggrepel, lubridate)
source(file = "What to watch/Fonctions.R", encoding = "UTF-8")
# Fetching ----------------------------------------------------------------
Pages =
  list(
    # Planned = "https://myanimelist.net/animelist/Altermedia?status=7&tag=",
    # OnHold = "https://myanimelist.net/animelist/Altermedia?status=3&tag=",
    # Dropped = "https://myanimelist.net/animelist/Altermedia?status=4&tag=",
    Watching = "https://myanimelist.net/animelist/Altermedia?status=1&tag="
       )

input =
  Pages %>%
  map(CLINNE) %>% bind_rows(.id = "Status") %>%
  mutate(Progress = Nb / Eps)

input %>%
  mutate(Stop = Eps / 26 * 5) %>%
  filter(Nb < Stop,
         !is.na(as.numeric(Score))) %>%
  mutate(Rest = ceiling(Stop)-Nb) %>%
  arrange(Rest)

# Data pre-processing -----------------------------------------------------
Liste =
  input %>%
  mutate(Saisons = (Eps / 12) %>% round(digits = 0)) %>%
  mutate(Saisons = case_when(Saisons == 0 ~ 1,
                             TRUE ~ Saisons)) %>%
  mutate(Eps_Saison = (Eps / Saisons) %>% ceiling()) %>%
  mutate(Stop = (1 + floor(Nb / Eps_Saison)) * Eps_Saison) %>%
  mutate(Eps = pmin(Stop, Eps)) %>%
  mutate(Reste = Eps - Nb) %>%
  FILTRAGE(condition_new = "y", condition_crap = "n")

# Modelization ------------------------------------------------------------
Torrents =
  c(
    "Slime",
    "Chihaya",
    "Psycho",
    "Kono"
  )

# Liste =
#   Torrents %>%
#   map_df(.f = ~filter(.data = Liste,
#                    str_detect(string = `Anime Title`, pattern = .))) %>%
#   anti_join(x = Liste)

map(
  .x = c("(Next-Nb)/(Eps-Nb)", "1/(Eps-Next)", "Next/Eps"),
  .f = ~ TIRAGE(FORMULE = ., input = Liste)
) %>%
  unlist() %>%
  table() %>% data.table() %>% split(x = .$., f = .$N) %>%
  .[length(.)] %>%
  print()

Liste %>%
  split(x = paste(.$`Anime Title`, " n°", .$Nb + 1, sep = ""),
        f = .$Reste) %>% .[1] %>%
  print()
