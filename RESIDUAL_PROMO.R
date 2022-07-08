
## Bibliotecas necessárias

library(DBI)
library(dplyr)
library(reshape2)
library(googlesheets4)


## conexão com banco replica 2629925

con2 <- dbConnect(odbc::odbc(), "reproreplica")




residual_promo1 <- dbGetQuery(con2,"
    WITH PED AS (SELECT ID_PEDIDO FROM PEDID WHERE ID_PEDIDO IN (12323058,12324108)),
    
    LEN AS (SELECT PROCODIGO,
                    PRODESCRICAO,
                     IIF(PROCODIGO2 IS NULL,
                      PROCODIGO,PROCODIGO2)CHAVE FROM PRODU WHERE PROTIPO NOT IN ('T','M')),
    
    TRAT AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE PROTIPO='T'),
  
    PED1 AS (SELECT DISTINCT P.ID_PEDIDO,
                      CHAVE ID1_LENTE_COD,
                       (SELECT PRODESCRICAO FROM PRODU WHERE PROCODIGO=CHAVE) ID1_LENTE
     FROM PDPRD P
      INNER JOIN PED ON P.ID_PEDIDO=PED.ID_PEDIDO
       INNER JOIN LEN L ON L.PROCODIGO=P.PROCODIGO),
       
    PED2 AS (SELECT DISTINCT P.ID_PEDIDO,
                      P.PROCODIGO ID1_TRAT_COD,
                        (SELECT PRODESCRICAO FROM PRODU WHERE P.PROCODIGO=PROCODIGO) ID1_TRAT
        FROM PDPRD P
         INNER JOIN PED ON P.ID_PEDIDO=PED.ID_PEDIDO
          INNER JOIN TRAT T ON T.PROCODIGO=P.PROCODIGO)
    
    SELECT P.ID_PEDIDO,
            ID1_LENTE_COD,
             ID1_LENTE,
              ID1_TRAT_COD,
               ID2_TRAT
                FROM PED P
                 LEFT JOIN PED1 ON  PED1.ID_PEDIDO=P.ID_PEDIDO
                  LEFT JOIN PED2 ON  PED2.ID_PEDIDO=P.ID_PEDIDO")  


View(residual_promo1)


residual_promo2 <- dbGetQuery(con2,"
     
    ")  


View(residual_promo)


RESIDUAL2022 %>% .[,2] %>% paste0(.,collapse = ",")

data.frame(RESIDUAL2022 %>% select(`ID 1`) %>% mutate(`ID 1`=trimws(`ID 1`))) %>% paste0(.,collapse = ",")

data.frame(A=c(12619950,12614554,12636672)) 



