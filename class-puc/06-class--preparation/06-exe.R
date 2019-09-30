library(tidyverse)
library(rvest)

h <- read_html(paste0("http://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"))

class(h)

nodes <- h %>% html_nodes("table")

nodes[[1]]
nodes[[2]]
nodes[[3]]
nodes[[4]]

tab1 <- nodes[[1]] %>% html_table
tab2 <- nodes[[2]] %>% html_table
tab3 <- nodes[[3]] %>% html_table
tab4 <- nodes[[4]] %>% html_table

nodes <- nodes[2:3]


tables <- sapply(nodes, html_table)
tables

firstTable <- tables[[1]]
secondTable <- tables[[2]]

secondTable <- secondTable %>% setNames(c("rank", "team", "man", "disabledList", "totalPayroll"))

# Remove first line of the table
secondTable <- secondTable[-1,]



