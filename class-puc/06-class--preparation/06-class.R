# Web Scraping / Web Harvesting
# Obter dados da p??gina da web

library(tidyverse)
library(rvest)

header <- read_html(paste0("https://en.wikipedia.org/w/index.php?title=",
                                               "Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"))

class(header)

tab <- header %>% html_nodes("table")

#Node
tab[[1]]
tab[[2]]
tab[[3]]

# Node set (has more informations)
tab[1]

length(tab[[2]])

tab <- tab[[2]] %>% html_table

class(tab)
tab

head(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murder_rate"))
head(tab)

murders_raw <- tab

murders_raw$population[1:3]

commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))

murders_raw <- tab[,1:4]
murders_raw$population[1:3]

murders_new <- murders_raw %>% mutate_at(1:3, parse_number)
head(murders_new)


