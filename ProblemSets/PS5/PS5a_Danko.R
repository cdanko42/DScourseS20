library(tidyverse)
library(rvest)
#read html into table
tchamp <- read_html("https://en.wikipedia.org/wiki/2019%E2%80%9320_NCAA_Division_I_men%27s_basketball_season")
tchamp %>%
  html_nodes("#mw-content-text > div > table:nth-child(31)") %>%
  html_table(fill=TRUE)
#convert table to df

tchampdf <-
  tchamp %>%
  html_nodes("#mw-content-text > div > table:nth-child(31)") %>%
  html_table(fill=TRUE)
tchampdf <-
  tchampdf %>%
  bind_rows() %>%
  as_tibble()
tchampdf
