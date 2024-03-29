
## Bibliotecas necessárias

library(DBI)
library(dplyr)
library(reshape2)
library(googlesheets4)
library(glue)

residual <- range_read("15Or0FoTTDZBBSGuH_zPGYO9jD3QV97QNYtt9336u30w",sheet = "DADOS" , range = "B:C") %>% 
             mutate(HASH=paste0(ID1,ID2))

residual_id1 <- residual %>% select(ID1) %>% rename("ID_PEDIDO"="ID1") %>% filter(!is.na(.))

residual_id2 <- residual %>% select(ID2) %>% rename("ID_PEDIDO"="ID2") %>% filter(!is.na(.))



test_pedid <- dbGetQuery(con2,"SELECT ID_PEDIDO,PEDSITPED FROM PEDID WHERE PEDDTEMIS>= DATEADD(-360 DAY TO CURRENT_DATE)")

inner_join(residual_id1,test_pedid,by="ID_PEDIDO") %>% View(.)
 
 residual_promo1_sql <- glue_sql("
    WITH PED AS (SELECT ID_PEDIDO,PEDVRTOTAL FROM PEDID WHERE 
    ID_PEDIDO IN ({residual_id1$ID_PEDIDO*})
    AND PEDSITPED<>'C'),
    
    LEN AS (SELECT PROCODIGO,
                    PRODESCRICAO,
                     IIF(PROCODIGO2 IS NULL,
                      PROCODIGO,PROCODIGO2)CHAVE FROM PRODU WHERE PROTIPO NOT IN ('T','M','C','S','X','K')),
    
    TRAT AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE PROTIPO='T'),
    
    COL AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE PROTIPO='C'),
  
    PED1 AS (SELECT DISTINCT P.ID_PEDIDO,
                      CHAVE LENTE_COD,
                       (SELECT PRODESCRICAO FROM PRODU WHERE PROCODIGO=CHAVE) LENTE
     FROM PDPRD P
      INNER JOIN PED ON P.ID_PEDIDO=PED.ID_PEDIDO
       INNER JOIN LEN L ON L.PROCODIGO=P.PROCODIGO),
       
    PED2 AS (SELECT DISTINCT P.ID_PEDIDO,
                      P.PROCODIGO TRAT_COD,
                        (SELECT PRODESCRICAO FROM PRODU WHERE P.PROCODIGO=PROCODIGO) TRAT
        FROM PDPRD P
         INNER JOIN PED ON P.ID_PEDIDO=PED.ID_PEDIDO
          INNER JOIN TRAT T ON T.PROCODIGO=P.PROCODIGO),
          
    PED3 AS (SELECT DISTINCT P.ID_PEDIDO,
                      P.PROCODIGO COL_COD,
                        (SELECT PRODESCRICAO FROM PRODU WHERE P.PROCODIGO=PROCODIGO) COL
        FROM PDPRD P
         INNER JOIN PED ON P.ID_PEDIDO=PED.ID_PEDIDO
          INNER JOIN COL C ON C.PROCODIGO=P.PROCODIGO)
    
    SELECT P.ID_PEDIDO,
             LENTE_COD,
              LENTE,
               TRAT_COD,
                TRAT,
                 COL_COD,
                  COL,
                   PEDVRTOTAL
                    FROM PED P
                     LEFT JOIN PED1 ON  PED1.ID_PEDIDO=P.ID_PEDIDO
                      LEFT JOIN PED2 ON  PED2.ID_PEDIDO=P.ID_PEDIDO
                       LEFT JOIN PED3 ON  PED3.ID_PEDIDO=P.ID_PEDIDO
                        WHERE LENTE_COD IS NOT NULL")  
 
 
 residual_promo1 <-  dbGetQuery(con2,residual_promo1_sql)
 
 
 View(residual_promo1)
 
 
 
 
 residual_promo2_sql <- glue_sql("
    WITH PED AS (SELECT ID_PEDIDO,PEDVRTOTAL FROM PEDID WHERE 
    ID_PEDIDO IN ({residual_id2$ID_PEDIDO*})
    AND PEDSITPED<>'C'),
    
    LEN AS (SELECT PROCODIGO,
                    PRODESCRICAO,
                     IIF(PROCODIGO2 IS NULL,
                      PROCODIGO,PROCODIGO2)CHAVE FROM PRODU WHERE PROTIPO NOT IN ('T','M','C','S','X','K')),
    
    TRAT AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE PROTIPO='T'),
    
    COL AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE PROTIPO='C'),
  
    PED1 AS (SELECT DISTINCT P.ID_PEDIDO,
                      CHAVE LENTE_COD,
                       (SELECT PRODESCRICAO FROM PRODU WHERE PROCODIGO=CHAVE) LENTE
     FROM PDPRD P
      INNER JOIN PED ON P.ID_PEDIDO=PED.ID_PEDIDO
       INNER JOIN LEN L ON L.PROCODIGO=P.PROCODIGO),
       
    PED2 AS (SELECT DISTINCT P.ID_PEDIDO,
                      P.PROCODIGO TRAT_COD,
                        (SELECT PRODESCRICAO FROM PRODU WHERE P.PROCODIGO=PROCODIGO) TRAT
        FROM PDPRD P
         INNER JOIN PED ON P.ID_PEDIDO=PED.ID_PEDIDO
          INNER JOIN TRAT T ON T.PROCODIGO=P.PROCODIGO),
          
    PED3 AS (SELECT DISTINCT P.ID_PEDIDO,
                      P.PROCODIGO COL_COD,
                        (SELECT PRODESCRICAO FROM PRODU WHERE P.PROCODIGO=PROCODIGO) COL
        FROM PDPRD P
         INNER JOIN PED ON P.ID_PEDIDO=PED.ID_PEDIDO
          INNER JOIN COL C ON C.PROCODIGO=P.PROCODIGO)
    
    SELECT P.ID_PEDIDO,
             LENTE_COD,
              LENTE,
               TRAT_COD,
                TRAT,
                 COL_COD,
                  COL,
                   PEDVRTOTAL
                    FROM PED P
                     LEFT JOIN PED1 ON  PED1.ID_PEDIDO=P.ID_PEDIDO
                      LEFT JOIN PED2 ON  PED2.ID_PEDIDO=P.ID_PEDIDO
                       LEFT JOIN PED3 ON  PED3.ID_PEDIDO=P.ID_PEDIDO
                        WHERE LENTE_COD IS NOT NULL")  
 
 
 residual_promo2 <-  dbGetQuery(con2,residual_promo2_sql)
 
 
 
 
 
 residual_id1_sheets <- inner_join(residual_promo1 %>% 
                                     mutate(ID_PEDIDO=as.character(ID_PEDIDO)),residual %>% 
                                       rename("ID_PEDIDO"="ID1") %>%
                                         mutate(ID_PEDIDO=trimws(ID_PEDIDO)),by="ID_PEDIDO")

 
 
 residual_id2_sheets <- inner_join(residual_promo2 %>% 
                          mutate(ID_PEDIDO=as.character(ID_PEDIDO)),residual %>% 
                           rename("ID_PEDIDO"="ID2") %>%
                             mutate(ID_PEDIDO=trimws(ID_PEDIDO)),by="ID_PEDIDO") 
 
 
 
residual_sheets <- left_join(residual_id1_sheets,residual_id2_sheets, by="HASH") 


sheet_write(residual_sheets %>% .[,c(-9,-10,-19)],ss="15Or0FoTTDZBBSGuH_zPGYO9jD3QV97QNYtt9336u30w",sheet="RESIDUAL")


## IDS PEDIDOS CONTROL

residual_id1_control <- anti_join(residual_id1,residual_promo1,by="ID_PEDIDO")


residual_promo_control_sql <- glue_sql("
    WITH PEDCONTROL AS (SELECT CLICODIGO, 
                          REPLACE(PEDCODIGO,'.000','.001') PEDCODIGO,
                           PEDDTEMIS FROM PEDID 
                            WHERE ID_PEDIDO IN ({residual_id1_control$ID_PEDIDO*})
                            ),
                   
    PED AS (SELECT ID_PEDIDO,PEDVRTOTAL FROM PEDID P 
                   INNER JOIN PEDCONTROL PE ON P.PEDCODIGO=PE.PEDCODIGO AND 
                    P.CLICODIGO=PE.CLICODIGO AND
                     P.PEDDTEMIS=PE.PEDDTEMIS),  
                        
    
    LEN AS (SELECT PROCODIGO,
                    PRODESCRICAO,
                     IIF(PROCODIGO2 IS NULL,
                      PROCODIGO,PROCODIGO2)CHAVE FROM PRODU WHERE PROTIPO NOT IN ('T','M','C','S','X','K')),
    
    TRAT AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE PROTIPO='T'),
    
    COL AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE PROTIPO='C'),
  
    PED1 AS (SELECT DISTINCT P.ID_PEDIDO,
                      CHAVE LENTE_COD,
                       (SELECT PRODESCRICAO FROM PRODU WHERE PROCODIGO=CHAVE) LENTE
     FROM PDPRD P
       INNER JOIN LEN L ON L.PROCODIGO=P.PROCODIGO),
       
    PED2 AS (SELECT DISTINCT P.ID_PEDIDO,
                      P.PROCODIGO TRAT_COD,
                        (SELECT PRODESCRICAO FROM PRODU WHERE P.PROCODIGO=PROCODIGO) TRAT
        FROM PDPRD P
          INNER JOIN TRAT T ON T.PROCODIGO=P.PROCODIGO),
          
    PED3 AS (SELECT DISTINCT P.ID_PEDIDO,
                      P.PROCODIGO COL_COD,
                        (SELECT PRODESCRICAO FROM PRODU WHERE P.PROCODIGO=PROCODIGO) COL
        FROM PDPRD P
          INNER JOIN COL C ON C.PROCODIGO=P.PROCODIGO)
    
    SELECT P.ID_PEDIDO,
             LENTE_COD,
              LENTE,
               TRAT_COD,
                TRAT,
                 COL_COD,
                  COL,
                   SUM(PEDVRTOTAL) PEDVRTOTAL
                    FROM PDPRD P
                     INNER JOIN PED PE ON P.ID_PEDIDO=PE.ID_PEDIDO
                      LEFT JOIN PED1 ON  PED1.ID_PEDIDO=P.ID_PEDIDO
                       LEFT JOIN PED2 ON  PED2.ID_PEDIDO=P.ID_PEDIDO
                        LEFT JOIN PED3 ON  PED3.ID_PEDIDO=P.ID_PEDIDO
                         GROUP BY 1,2,3,4,5,6,7
                        ")  


residual_promo_control<-  dbGetQuery(con2,residual_promo_control_sql)


residual_promo_control_sql2 <- glue_sql("
    WITH PEDCONTROL AS (SELECT CLICODIGO, 
                          REPLACE(PEDCODIGO,'.000','.001') PEDCODIGO,ID_PEDIDO ID_PEDIDO2,
                           PEDDTEMIS FROM PEDID 
                            WHERE ID_PEDIDO IN ({residual_id1_control$ID_PEDIDO*})
                            )
                   
    SELECT P.ID_PEDIDO,ID_PEDIDO2 FROM PEDID P 
                   INNER JOIN PEDCONTROL PE ON P.PEDCODIGO=PE.PEDCODIGO AND 
                    P.CLICODIGO=PE.CLICODIGO AND
                     P.PEDDTEMIS=PE.PEDDTEMIS 
                        ")  


residual_promo_control2<-  dbGetQuery(con2,residual_promo_control_sql2)


View(residual_promo_control2)

sheet_write(left_join(residual_promo_control,residual_promo_control2,by="ID_PEDIDO"),ss="15Or0FoTTDZBBSGuH_zPGYO9jD3QV97QNYtt9336u30w",sheet="CONTROL")






