SAFE_FETCH <- function(input) {
  pacman::p_load(rvest, purrr)
  safe_read = safely(.f = read_html, quiet = FALSE)
  input = input %>% set_names %>% map(safe_read)

  output = input %>% purrr::transpose() %>% map(compact)
  input = output$result

  while (length(output$error)) {
    output$error %>% length %>% str_c(., " errors left") %>% cat(sep = "\n")
    output =
      output$error %>% names %>% set_names() %>% map(safe_read) %>%
      purrr::transpose() %>% map(compact)

    input = list(input, output$result) %>% flatten
  }
  return(input)
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
