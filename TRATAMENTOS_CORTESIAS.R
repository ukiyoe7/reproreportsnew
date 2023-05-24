

library(DBI)
library(tidyverse)
library(readr)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "reproreplica")


query_trat_cortesias <- dbGetQuery(con2, statement = read_file('SQL/TRATAMENTOS_CORTESIAS.sql'))

query_trat_bonif <- dbGetQuery(con2, statement = read_file('SQL/TRATAMENTOS_BONIFICADOS.sql'))

query_promo <- dbGetQuery(con2, statement = read_file('SQL/PROMO.sql'))

query_clients <- dbGetQuery(con2, statement = read_file('SQL/CLIENTS.sql')) %>% 
                  select(CLICODIGO,CLICNPJCPF,CLIENTE) %>% 
                   anti_join(.,dbGetQuery(con2, statement = read_file('SQL/INATIVOS.sql')),by="CLICODIGO")

View(query_trat_cortesias)

View(query_trat_bonif)

View(query_promo)

View(query_clients)

cortesias_trat <-
 left_join(query_trat_cortesias,query_promo,by="ID_PEDIDO") %>%  
  left_join(.,query_trat_bonif %>% mutate(TRAT_BONIF_CLI=1),by="CLICODIGO") %>% 
    mutate(TRAT_BONIF=if_else(VRVENDA>0.01 & VRVENDA<=1,1,0)) %>% 
     mutate(TRAT_BONIF_CLI = coalesce(TRAT_BONIF_CLI, 0)) %>% 
      mutate(PROMO = coalesce(PROMO, 0))

View(cortesias_trat)

write.csv2(cortesias_trat,file = "C:\\Users\\Repro\\Documents\\REPORTS\\2023\\MAI\\cortesias_trat.csv")

