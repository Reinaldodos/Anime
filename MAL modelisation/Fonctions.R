read_with_progress <- function(filename, pb) {
  pb$tick()$print()
  data_read <- safe_read(filename)
  # you can add additional operations on data_read, or
  # decide on entirely different task that this function should do.
}

read_CSS <- function(XML, CSS) {
  XML %>%
    html_nodes(css = CSS) %>% html_text() %>%
    return()
}


read_INFO <- function(XML, INFO) {
  XML %>% read_CSS(CSS = ".js-scrollfix-bottom div") %>%
    str_trim() %>%
    str_subset(pattern = INFO) %>%
    str_split_fixed(pattern = "\n", n = 2) %>% .[,2] %>%
    return()
}

Fetch_Chara = function(XML){
  XML %>%
    html_nodes(css = "#horiznav_nav a") %>%
    html_attr(name = "href") %>%
    str_subset(pattern = "chara") %>%
    return()
}

read_Seiyuus = function(XML) {
  XML %>% read_CSS(CSS = ".js-scrollfix-bottom-rel .borderClass td") %>%
    str_trim() %>%
    str_subset(pattern = "Japanese") %>% str_remove_all(pattern = "Japanese") %>%
    return()
}

