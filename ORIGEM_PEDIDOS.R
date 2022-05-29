## Bibliotecas necessárias

library(DBI)
library(dplyr)
library(reshape2)
library(googlesheets4)


## conexão com banco replica 

con2 <- dbConnect(odbc::odbc(), "reproreplica")



library(tidyverse)

#CURRENT MONTH

pedidosorigem <- dbGetQuery(con2,"
    
    WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),
    
    SETOR AS (SELECT ZOCODIGO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,28)),
    
    ENDE AS (SELECT ENDCODIGO,CLICODIGO,SETOR FROM ENDCLI E
    INNER JOIN (SELECT ZOCODIGO,ZODESCRICAO SETOR FROM ZONA)Z ON E.ZOCODIGO=Z.ZOCODIGO 
    WHERE ENDFAT='S')
    
    SELECT ID_PEDIDO,PEDDTBAIXA,PEDID.CLICODIGO,PEDORIGEM,SETOR
    FROM PEDID INNER JOIN FIS ON PEDID.FISCODIGO1=FIS.FISCODIGO
     LEFT JOIN ENDE ON PEDID.CLICODIGO=ENDE.CLICODIGO AND PEDID.ENDCODIGO=ENDE.ENDCODIGO
      WHERE PEDDTBAIXA BETWEEN '01.01.2022' AND 'YESTERDAY' AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N')
    ")  


View(pedidosorigem)

pedidosorigem2 <- pedidosorigem %>% filter(PEDORIGEM %in% c('D','W')) %>% group_by(SETOR,PEDORIGEM) %>% 
  summarize(QTD=n_distinct(ID_PEDIDO)) %>% 
  dcast(SETOR ~ PEDORIGEM)


range_write("12kQR0IlFKjMf1JTz0cPgQwaqxss4oTfRsF9wUky1gQs",data=pedidosorigem2,sheet = "DADOS",
            range = "A1",reformat = FALSE) 




pedidosorigem %>% .[duplicated(.$ID_PEDIDO),]


### LISTAGEM CLIENTES


#CURRENT MONTH

pedidosorigem2 <- dbGetQuery(con2,"
    
    WITH FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),
    
 CLI AS (SELECT DISTINCT C.CLICODIGO,CLINOMEFANT,GCLCODIGO,SETOR
  FROM CLIEN C
  LEFT JOIN (SELECT CLICODIGO,D.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI D
  INNER JOIN ZONA Z ON D.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
   WHERE CLICLIENTE='S')
    
    SELECT ID_PEDIDO,PEDDTBAIXA,P.CLICODIGO,CLINOMEFANT,PEDORIGEM,SETOR
    FROM PEDID P
    INNER JOIN FIS ON P.FISCODIGO1=FIS.FISCODIGO
     LEFT JOIN CLI C ON C.CLICODIGO=P.CLICODIGO
      WHERE PEDDTBAIXA>= DATEADD(-30 DAY TO CURRENT_DATE) 
      AND PEDSITPED<>'C' AND PEDLCFINANC IN ('S', 'L','N')
    ")  %>%  filter(PEDORIGEM %in% c('D','W')) %>%
  filter(SETOR=='SETOR 3 - CHAPECO-PLANAL-OESTE')



View(pedidosorigem2)

pedidosorigem3 <- pedidosorigem2 %>%  group_by(CLICODIGO,CLINOMEFANT,SETOR,PEDORIGEM) %>% 
  summarize(QTD=n_distinct(ID_PEDIDO)) %>% 
  dcast(CLICODIGO + CLINOMEFANT ~ PEDORIGEM) %>%
  arrange(desc(W)) %>% as.data.frame() %>% 
  mutate(PERC_W=round((W)/(W+D)*100,2)) %>%
  mutate(PERC_D=round((D)/(W+D)*100,2)) 


range_write("1RG5AmpKQuI7Sv49oRrMhvaskCJV4qA8nwgLicjyhiaI",data=pedidosorigem3,sheet = "RESUMO",
            range = "A1",reformat = FALSE) 

pedidosorigem4 <- pedidosorigem2 %>% 
  group_by(CLICODIGO,CLINOMEFANT,SETOR,PEDORIGEM) %>% 
  summarize(QTD=n_distinct(ID_PEDIDO)) %>% 
  dcast(CLICODIGO + CLINOMEFANT ~ PEDORIGEM) %>% 
  filter(is.na(W)) %>% 
  arrange(desc(D))

range_write("1RG5AmpKQuI7Sv49oRrMhvaskCJV4qA8nwgLicjyhiaI",data=pedidosorigem4,sheet = "SOMENTE DIGITADOS",
            range = "A1",reformat = FALSE) 

range_write("1RG5AmpKQuI7Sv49oRrMhvaskCJV4qA8nwgLicjyhiaI",data=pedidosorigem2,sheet = "DADOS",
            range = "A1",reformat = FALSE) 




pedidosorigem2 %>% .[duplicated(.$ID_PEDIDO),]



