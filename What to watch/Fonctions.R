mixedToFloat <- function(x){
  x <- sub(' ', '+', x, fixed = TRUE)
  return(unlist(lapply(x, function(x)
    eval(parse(
      text = x
    )))))
}

CLINNE <- function(url){
  paste("Fetching: ", url, sep = "") %>% print()
  require(rvest)
  require(data.table)
  input =
    url %>%
    read_html() %>%
    html_table() %>%
    rbindlist(fill = TRUE)

  names(input) = c("Anime Title",
                   "Score",
                   "Type",
                   "Progress",
                   "Tags",
                   "Rated")

  input =
    input %>%
    mutate(
      `Anime Title` = gsub(
        pattern = "Add\n                                -\n                More",
        replacement = "",
        x = `Anime Title`
      ),
      Progress = gsub(
        pattern = "-",
        replacement = 0,
        x = Progress
      )
    ) %>%
    filter(grepl(pattern = "/", x = Progress) &
             !grepl(pattern = "Airing", x = `Anime Title`))

  test =
    lapply(input$Progress, function(x)
      strsplit(x, split = "/")) %>%
    rbindlist()

  input$Nb = test[seq(1, nrow(test), 2)]$V1 %>% as.numeric()
  input$Eps = test[seq(2, nrow(test), 2)]$V1 %>% as.numeric()
  input$Progress = input$Progress %>% mixedToFloat()

  input %>%
    mutate_if(.predicate = is.character,
              .funs = trimws) %>%
    return()
}

FILTRAGE <- function(input, condition_new, condition_crap){

  if (condition_new == "n")
  {
    input = input %>% filter(Reste!=Eps_Saison)
  }

  if (condition_crap == "y")
  {
    input =
      input %>%
      mutate(Progress = Nb / Eps) %>%
      filter(Progress > mean(Progress, na.rm = TRUE))
  }

  # if (input %>% filter(Cut > Nb) %>% nrow() > 0)
  # {
  #   input =
  #     input %>%
  #     filter(Cut > Nb) %>%
  #     mutate(Eps = Cut,
  #            Progress = Nb / Eps)
  # }
  return(input)
}

TIRAGE <- function(input, FORMULE){
  POACH =
    input$`Anime Title` %>%
    lapply(function(anime)
    {
      toto =
        input %>%
        filter(`Anime Title` == anime) %>%
        select(`Anime Title`, Nb, Eps)

      cbind.data.frame(toto, Next = (toto$Nb + 1):toto$Eps) %>%
        mutate_(Progress = FORMULE) %>%
        return()
    }) %>%
    rbindlist() %>%
    arrange(Progress, `Anime Title`) %>%
    group_by(`Anime Title`) %>%
    mutate(Index = dense_rank(Progress)) %>% ungroup()



  SEUIL = POACH %>% filter(Index == 2) %>% .$Progress %>% min()

  POACH %>%
    filter(Progress < SEUIL) %>%
    split(x = paste(.$`Anime Title`, " n°", .$Next, sep = ""),
          f = .$Progress) %>% unlist(use.names = FALSE) %>% return()
}

