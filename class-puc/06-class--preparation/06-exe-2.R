library(tidyverse)
library(rvest)

h <- read_html(paste0("https://m.imdb.com/chart/bestpicture"))

nodes <- h %>% html_nodes(".best-picture-item-title")
nodes[1]
