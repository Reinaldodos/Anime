RELATIONS <- function(anime)
{
  print(anime)
  relations =
    anime %>%
    read_html() %>%
    html_nodes(css = ".anime_detail_related_anime a")

  relations =
    cbind.data.frame(ANIMEka = relations %>% html_attr(name = "href"),
                   Titre = relations %>% html_text()) %>%
    filter(grepl(pattern = "anime", x = .$ANIMEka)) %>%
    .$Titre %>% as.character()

  Title =
    anime %>%
    read_html() %>%
    html_nodes(css = ".h1 span") %>%
    html_text()

  cbind.data.frame(Title, relations) %>%
    return()
}

FRANCHISES <- function(input)
{
  input = paste("https://myanimelist.net", input, sep = "")


  Relations =
    lapply(input, function(anime)
      try(expr = RELATIONS(anime), silent = TRUE))

  require(foreach)
  relations =
    foreach(struc = Relations, .combine = rbind)  %do%
    if (length(struc)  ==  2)
    {
      struc
    } %>%
    mutate_if(.predicate = is.factor, .funs = as.character) %>%
    filter(Title != relations) %>%
    filter(relations != "")

  require(igraph)
  Groupes =
    graph_from_edgelist(el = as.matrix(relations), directed = FALSE) %>%
    simplify(remove.multiple = TRUE, remove.loops = TRUE) %>%
    igraph::decompose.graph()

  Groupes =
    lapply(Groupes,
           function(groupe)
           {
             noms = V(groupe)$name
             # noms = noms[noms %in% relations$Title]
             if (length(noms) > 1)
             {
               expand.grid(noms, noms) %>% return()
             }
           }) %>%
    rbindlist()

  names(Groupes) = c("An1", "An2")

  return(Groupes)
}

franchises =
  "https://myanimelist.net/animelist/Altermedia" %>%
  read_html() %>%
  html_nodes(css = ".animetitle") %>%
  html_attr(name = "href") %>%
  FRANCHISES() %>%
  filter(An1 != An2)

saveRDS(object = franchises, file = "anidb jugement comparatif/Franchises")
