library(tfse)
library(rvest)
library(tidyverse)

## scrape initial page/get links
x <- xml2::read_html("https://sna.stanford.edu/rlabs.php")
x %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr("href") %>%
  grep("l=\\d", ., value = TRUE) %>%
  paste0("http://sna.stanford.edu/", .) %>%
  purrr::map(xml2::read_html) -> l

## function to get content/lab links
get_links <- function(x) {
  u <- rvest::html_nodes(x, "a") %>%
    rvest::html_attr("href") %>%
    rvest::url_absolute(rvest::xml_url(x))
  grep("\\.php$|\\.html$|/$|content_main$", unique(u),
    value = TRUE, invert = TRUE)
}

## apply to each lab page
r <- map(l, f)

## convert each to a tibble along with lab numbers
for (i in seq_along(r)) {
  r[[i]] <- tibble::tibble(lab = i, link = r[[i]])
}

## function to build lab dir
build_lab <- function(x) {
  l <- x$lab[1]
  u <- x$link
  lab_folder <- paste0("lab_", l)
  dir.create(lab_folder)
  for (i in u) {
    download.file(i, file.path(lab_folder, basename(i)))
  }
}

## apply function to build and fill lab folders 1-9
purrr::map(r, build_lab)
