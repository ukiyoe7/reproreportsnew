
## Relatório exportar informações N308

## Bibliotecas necessárias

library(DBI)
library(dplyr)
library(reshape2)
library(googlesheets4)

## conexão com banco replica 

con2 <- dbConnect(odbc::odbc(), "reproreplica")


rep_308 <- dbGetQuery(con2,"

SELECT DISTINCT AP.ID_PEDIDO, 
                 AP.LPCODIGO, 
                  LP.LPDESCRICAO, 
                   AP.USUCODIGO,
                    FUN.FUNNOME,
                     PC.CLICODIGO,
                      AP.APDATA, 
                       AP.APOBS,
                        PROCODIGO,
                         PDPDESCRICAO
                        
                FROM ACOPED AP
                 LEFT JOIN LOCALPED LP ON (AP.LPCODIGO = LP.LPCODIGO)
                  LEFT JOIN PEDID PC ON (PC.ID_PEDIDO = AP.ID_PEDIDO)
                   LEFT JOIN FUNCIO FUN ON (FUN.FUNCODIGO = AP.USUCODIGO)
                    LEFT JOIN (SELECT ID_PEDIDO,
                                       PD.PROCODIGO,
                                        PDPDESCRICAO 
                                         FROM PDPRD PD
                                          INNER JOIN (SELECT PROCODIGO 
                                                              FROM PRODU WHERE PROTIPO IN ('F','P','E')) A ON PD.PROCODIGO=A.PROCODIGO)B ON PC.ID_PEDIDO=B.ID_PEDIDO
                    WHERE AP.EMPCODIGO = 1
                     AND AP.LPCODIGO = 91
                      AND AP.APDATA BETWEEN '20.06.2022'AND 'TODAY'
                       ORDER BY 5,6 ")


View(rep_308)


rep_308 %>%  filter(is.na(PROCODIGO)) %>% View()
