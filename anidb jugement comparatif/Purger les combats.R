# Import des données ------------------------------------------------------
Table = LIST(url)
input = Table %>% Get_results()
Table = ELO(Table = input$Table, Results = input$Results)

# Exclusion des combats incohérents ---------------------------------------
OUT =
  Table %>% drop_na() %>%
  anti_join(x = Table) %>%
  pull(Player) %>% as.character()

input$Results %>%
  filter(An1 %in% OUT |
           An2 %in% OUT) %>%
  anti_join(x = input$Results) %>%
  select(AN1, AN2, Score) %>%
  write.table(
    file = "anidb jugement comparatif/Combats menes.csv",
    append = F,
    quote = F,
    sep = "|",
    row.names = F,
    col.names = T,
    fileEncoding = "UTF-8"
  )

input = Table %>% Get_results()
output = ELO(Table = input$Table, Results = input$Results)
source("anidb jugement comparatif/Score final.R", echo = TRUE)
