library(rtweet)
library(tfse)
x <- xml2::read_html("https://sna.stanford.edu/rlabs.php")
library(rvest)
library(tidyverse)
x %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  grep("l=\\d", ., value = TRUE) %>%
  paste0("http://sna.stanford.edu/", .) -> u

map(u, xml2::read_html) -> l

l <- map2(l, u, ~ {
  attr(.x, "base_url") <- .y
  .x
})



f <- function(x) {
  u <- rvest::html_nodes(x, "a") %>% 
    html_attr("href") %>% 
    url_absolute(xml_url(x))
  grep("\\.php$|\\.html$|/$|content_main$", unique(u), value = TRUE, invert = TRUE)
}

r <- map(l, f)
for (i in seq_along(r)) {
  r[[i]] <- tibble::tibble(lab = i, link = r[[i]])
}
r <- bind_rows(r)

dir.create("~/R/stanford-sna")
setwd("~/R/stanford-sna")

build_course <- function(x) {
  l <- x$lab[1]
  u <- x$link
  lab_folder <- paste0("lab_", l)
  dir.create(lab_folder)
  for (i in u) {
    download.file(i, file.path(lab_folder, basename(i)))
  }
}


map(split(r, r$lab)[-1], build_course)
build_course(filter(r, lab== 1))
