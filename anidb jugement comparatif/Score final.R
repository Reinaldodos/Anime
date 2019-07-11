FINAL =
  output %>%
  SCORE_FINAL(STYLE = "sd") %>%
  select(Player, Apres = Note) %>%
  left_join(x = data$Table) %>%
  select(Player, Apres, Avant = Rating)

FINAL %>%
  filter(Apres != Avant) %>% view()

FINAL %>%
  gather(key = QUAND, value = COMBIEN,-Player) %>%
  ggplot(mapping = aes(x = COMBIEN, fill = QUAND)) +
  geom_histogram(position = "dodge")

FINAL %>%
  count(Avant, Apres) %>%
  ggplot(mapping = aes(x = Avant, y = Apres, size = n))+
  geom_point()+
  geom_abline()
