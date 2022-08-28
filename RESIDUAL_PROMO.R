
## Bibliotecas necessárias

library(DBI)
library(dplyr)
library(reshape2)
library(googlesheets4)


## conexão com banco replica 2629925

con2 <- dbConnect(odbc::odbc(), "reproreplica")



residual_promo1 <- dbGetQuery(con2,"
    WITH PED AS (SELECT ID_PEDIDO,PEDVRTOTAL FROM PEDID WHERE 
    ID_PEDIDO IN (12725418)
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


View(residual_promo1)


dbGetQuery(con2,"WITH PED AS (SELECT REPLACE(PEDCODIGO,'.000','.001') PEDCODIGO FROM PEDID 
                   WHERE ID_PEDIDO=12342113)
           SELECT ID_PEDIDO FROM PEDID P
           INNER JOIN PED PE ON P.PEDCODIGO=PE.PEDCODIGO
           
           ")

residual_promo_control <- dbGetQuery(con2,"
    WITH PEDCONTROL AS (SELECT CLICODIGO, 
                          REPLACE(PEDCODIGO,'.000','.001') PEDCODIGO,
                           PEDDTEMIS FROM PEDID 
                            WHERE ID_PEDIDO=12708144),
                   
    PED AS (SELECT ID_PEDIDO,PEDVRTOTAL FROM PEDID P 
                   INNER JOIN PEDCONTROL PE ON P.PEDCODIGO=PE.PEDCODIGO AND 
                    P.CLICODIGO=PE.CLICODIGO AND
                     P.PEDDTEMIS=PE.PEDDTEMIS),  
                     
    PEDID2 AS (SELECT ID_PEDIDO,PEDVRTOTAL FROM PEDID P 
                          WHERE ID_PEDIDO=12708144),
                          
    MONT_PDPRD AS (SELECT P.ID_PEDIDO,
                           PROCODIGO,
                            PDPDESCRICAO,
                             PEDVRTOTAL FROM PDPRD P
                                 INNER JOIN PED PE ON P.ID_PEDIDO=PE.ID_PEDIDO UNION
                                   SELECT (SELECT ID_PEDIDO FROM PED) ID_PEDIDO,
                                           PROCODIGO,
                                            PDPDESCRICAO,
                                             PEDVRTOTAL
                                              FROM PDPRD P2
                                               INNER JOIN PEDID2 PE2 ON P2.ID_PEDIDO=PE2.ID_PEDIDO),                      
    
    LEN AS (SELECT PROCODIGO,
                    PRODESCRICAO,
                     IIF(PROCODIGO2 IS NULL,
                      PROCODIGO,PROCODIGO2)CHAVE FROM PRODU WHERE PROTIPO NOT IN ('T','M','C','S','X','K')),
    
    TRAT AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE PROTIPO='T'),
    
    COL AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE PROTIPO='C'),
  
    PED1 AS (SELECT DISTINCT P.ID_PEDIDO,
                      CHAVE LENTE_COD,
                       (SELECT PRODESCRICAO FROM PRODU WHERE PROCODIGO=CHAVE) LENTE
     FROM MONT_PDPRD P
       INNER JOIN LEN L ON L.PROCODIGO=P.PROCODIGO),
       
    PED2 AS (SELECT DISTINCT P.ID_PEDIDO,
                      P.PROCODIGO TRAT_COD,
                        (SELECT PRODESCRICAO FROM PRODU WHERE P.PROCODIGO=PROCODIGO) TRAT
        FROM MONT_PDPRD P
          INNER JOIN TRAT T ON T.PROCODIGO=P.PROCODIGO),
          
    PED3 AS (SELECT DISTINCT P.ID_PEDIDO,
                      P.PROCODIGO COL_COD,
                        (SELECT PRODESCRICAO FROM PRODU WHERE P.PROCODIGO=PROCODIGO) COL
        FROM MONT_PDPRD P
          INNER JOIN COL C ON C.PROCODIGO=P.PROCODIGO)
    
    SELECT P.ID_PEDIDO,
             LENTE_COD,
              LENTE,
               TRAT_COD,
                TRAT,
                 COL_COD,
                  COL,
                   SUM(PEDVRTOTAL) PEDVRTOTAL
                    FROM MONT_PDPRD P
                     LEFT JOIN PED1 ON  PED1.ID_PEDIDO=P.ID_PEDIDO
                      LEFT JOIN PED2 ON  PED2.ID_PEDIDO=P.ID_PEDIDO
                       LEFT JOIN PED3 ON  PED3.ID_PEDIDO=P.ID_PEDIDO
                        GROUP BY 1,2,3,4,5,6,7
                        ")  


View(residual_promo_control)


