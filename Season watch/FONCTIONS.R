# Fetchez la vache --------------------------------------------------------
ANIMATION <- function(Liste_Anime)
{
  Title_Anime =
    Liste_Anime %>%
    html_nodes(css = ".link-title") %>%
    html_text()

  Url_Anime =
    Liste_Anime %>%
    html_nodes(css = ".link-title") %>%
    html_attr(name = "href")

  cbind.data.frame(Title = Title_Anime,
                   URL = Url_Anime) %>%
    return()
}
# Sequel or OVA? ----------------------------------------------------------
PREQUEL_DESUKA <- function(url)
{
  # print(url)
  Related =
    url %>%
    html_nodes(css = ".anime_detail_related_anime .borderClass") %>%
    html_text()

  condition = any(grepl(pattern = "Prequel", x = Related))
  condition2 = any(grepl(pattern = "Parent story", x = Related))

  return(condition | condition2)
}
# Is adaptation? ----------------------------------------------------------
SOURCING <- function(url)
{
  # print(url)
  Sources =
    url %>%
    html_nodes(css = ".anime_detail_related_anime .borderClass")

  Source_desuka =
    grepl(pattern = "Adaptation",
          x = Sources) %>% any()

  if (Source_desuka)
  {
    Source = Sources[grep(pattern = "Adaptation",
                          x = Sources) + 1]
    Source = html_attr(html_children(Source), name = "href")
    Source %>%
      return()
  }
}
# Get sources’ scores -----------------------------------------------------
SCORE_SOURCE = function(url)
{
  Score =
    url %>%
    read_html() %>%
    html_nodes(css = ".score") %>%
    html_text()

  cbind.data.frame(Source=url, Score) %>%
    return()
}
