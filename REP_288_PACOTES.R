
## Bibliotecas necessárias

library(DBI)
library(dplyr)
library(reshape2)
library(googlesheets4)

## conexão com banco replica 

con2 <- dbConnect(odbc::odbc(), "reproreplica")


rep_288 <- dbGetQuery(con2,"

WITH CLI AS (SELECT DISTINCT C.CLICODIGO,CLINOMEFANT,SETOR
             FROM CLIEN C
             LEFT JOIN (SELECT CLICODIGO,E.ZOCODIGO,ZODESCRICAO SETOR FROM ENDCLI E
                        LEFT JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA WHERE ZOCODIGO IN (20,21,22,23,24,25,28)
                        )Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S')A ON C.CLICODIGO=A.CLICODIGO
             WHERE CLICLIENTE='S')

SELECT P.CLICODIGO,SETOR,PEFDTEMIS EMISSAO,FISCODIGO,ID_PEDIDO,PEFVRTOTAL VRTOTAL FROM PEDFO P
INNER JOIN CLI C ON P.CLICODIGO=C.CLICODIGO
WHERE PEFSIT<>'C'
AND PEFDTEMIS BETWEEN '01.01.2022' and 'TODAY' AND FISCODIGO IN ('1.94C','2.94C')
  AND PEFSIT<>'C'")



range_write("1EqLttJ74cEaDro1FrEeJ1hSyp-qF5GjeAwNVvziwOXs",data=rep_288,sheet = "DEVOLUÇÕES 288 - ANO TODO",
            range = "A1",reformat = FALSE) 