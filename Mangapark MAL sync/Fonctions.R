FETCH_Recs <- function(url) {
  Strong =
    url %>%
    html_nodes(css = "strong") %>% html_text()

  LOG = Strong %>% as.integer %>% map_lgl(is.na)
  OK = data.table(Title = Strong[LOG])
  OK$Recs = Strong[!LOG] %>% as.integer() %>% c(., rep(0, nrow(OK))) %>% .[1:nrow(OK)]

  OK %>% mutate(Recs = Recs + 1) %>% return()
}

Fetch_related <- function(url) {
  Relations =
    url %>%
    html_nodes(css = ".anime_detail_related_anime a")

  toto = Relations %>%
    html_attr(name = "href") %>% str_detect(pattern = "anime")
  Relations %>% html_text() %>% .[toto] %>%
    return()
}

Manga_ka <- function(url) {
  Manga_desuka =
    url %>%
    html_nodes(css = ".js-scrollfix-bottom div") %>%
    html_text() %>% grep(pattern = "Type", value = T) %>%
    str_detect(pattern = "Manga")
  if (Manga_desuka)
    return(Manga_desuka)
}

safe_read = function(url) {
  print(url)
  RIDE = safely(.f = read_html, quiet = FALSE)
  Sys.sleep(time = 2)
  return(RIDE(url))
}

SAFE_FETCH <- function(input) {
  pacman::p_load(rvest, purrr)
  input = input %>% set_names %>% map(safe_read)

  output = input %>% purrr::transpose() %>% map(compact)
  input = output$result

  while (length(output$error)) {
    Sys.sleep(time = 600)
    output$error %>% length %>% str_c(., " errors left") %>% cat(sep = "\n")
    output =
      output$error %>% names %>% set_names() %>% map(safe_read) %>%
      purrr::transpose() %>% map(compact)

    input = list(input, output$result) %>% flatten
  }
  return(input)
}
