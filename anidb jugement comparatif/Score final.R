
FINAL =
  output %>%
  SCORE_FINAL(STYLE = "jenks", N = 10) %>%
  transmute(Player, Apres = ceiling(Note / 1)) %>%
  left_join(x = data$Table) %>%
  select(Player, Apres, Avant = Rating)

FINAL %>%
  filter(Avant == 10, Apres > 10) %>%
  anti_join(x = FINAL, by = "Player") %>%
  filter(Apres != Avant) %>%
  view()

FINAL %>%
  gather(key = QUAND, value = COMBIEN,-Player) %>%
  ggplot(mapping = aes(x = COMBIEN, fill = QUAND)) +
  geom_histogram(position = "dodge")

FINAL %>%
  count(Avant, Apres) %>%
  ggplot(mapping = aes(x = Avant, y = Apres, size = n)) +
  geom_point() +
  geom_abline()

FINAL %>%
  select_if(is.numeric) %>%
  psych::describe() %>%
  print()

