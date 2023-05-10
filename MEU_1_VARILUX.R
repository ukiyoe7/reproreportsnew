library(DBI)
library(tidyverse)
library(readr)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "reproreplica")


query1 <- dbGetQuery(con2, statement = read_file('SQL/MEU_1_VARILUX.sql')) %>% 

View(query1)

query1 %>% .[,c(1,5,6,7,9,10,11,2,3)] %>% View()