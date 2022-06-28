
library(DBI)
library(tidyverse)
library(googlesheets4)

con2 <- dbConnect(odbc::odbc(), "reproreplica")


pct <- dbGetQuery(con2,"
WITH PCTCLI AS (SELECT CLICODIGO,PCTNUMERO,PCTDTFIM,PCTSITUACAO FROM PCTCLI
                 WHERE PCTSITUACAO<>'C'),

CLI AS (SELECT C.CLICODIGO,
                CLINOMEFANT,
                 GCLCODIGO GRUPO, 
                  SETOR 
                   FROM CLIEN C
                    INNER JOIN (SELECT CLICODIGO, ZODESCRICAO SETOR FROM ENDCLI E
                     INNER JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA) Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S') ED ON C.CLICODIGO=ED.CLICODIGO
                      WHERE CLICLIENTE='S'),

PROD AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE LEFT(PROCODIGO,2)='LA' 
                 AND PRODESCRICAO LIKE '%ORMA%')


SELECT PC.CLICODIGO,
         CLINOMEFANT,
          GRUPO,
          SETOR,
           P.PCTNUMERO,
            PCTDTFIM TERMINO,
              P.PROCODIGO,
               PRODESCRICAO,
                PCPSALDO 
                 FROM PCTPRO P
                  INNER JOIN  PCTCLI PC ON P.PCTNUMERO=PC.PCTNUMERO
                   INNER JOIN  PROD PR ON P.PROCODIGO=PR.PROCODIGO
                    LEFT JOIN CLI C ON PC.CLICODIGO=C.CLICODIGO
                     WHERE PCPSALDO>0

")  


View(pct)

range_write("11-iJtKMh_8Dr2BiESlbQ3g7FBMRqnBmiDTMjY9XzBHw",data=pct %>% arrange(desc(TERMINO)),sheet = "DADOS",
            range = "A1",reformat = FALSE) 
