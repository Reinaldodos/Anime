FINAL =
  output %>%
  SCORE_FINAL(STYLE = "sd", N = 10) %>%
  select(Player, Apres = Note) %>%
  left_join(x = data$Table) %>%
  select(Player, Apres, Avant = Rating) %>%
  mutate(Apres = case_when(Apres > 10 ~ 10,
                           TRUE ~ as.double(Apres)))

FINAL %>%
  filter(Apres != Avant) %>% view()

FINAL %>%
  gather(key = QUAND, value = COMBIEN, -Player) %>%
  ggplot(mapping = aes(x = COMBIEN, fill = QUAND)) +
  geom_histogram(position = "dodge")

FINAL %>%
  count(Avant, Apres) %>%
  ggplot(mapping = aes(x = Avant, y = Apres, size = n)) +
  geom_point() +
  geom_abline()

FINAL %>%
  gather(key = QUAND, value = COMBIEN, -Player) %>%
  group_by(QUAND) %>%
  summarise(M = mean(COMBIEN, na.rm = T),
            SD = sd(COMBIEN, na.rm = T))
