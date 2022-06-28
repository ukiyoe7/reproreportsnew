## Bibliotecas necessárias

library(DBI)
library(dplyr)
library(googlesheets4)


## conexão com banco replica 

con2 <- dbConnect(odbc::odbc(), "reproreplica")



acordos <- dbGetQuery(con2,"
              WITH TAB AS (SELECT TBPCODIGO,TBPDESCRICAO DESCRICAO,TBPDTVALIDADE VALIDADE FROM TABPRECO 
              WHERE TBPDTVALIDADE>='TODAY' AND TBPDESCRICAO NOT LIKE'%PROMO DO MES%' ),
              
              CLI AS (SELECT C.CLICODIGO,
                CLINOMEFANT,
                 GCLCODIGO GRUPO, 
                  SETOR 
                   FROM CLIEN C
                    INNER JOIN (SELECT CLICODIGO, ZODESCRICAO SETOR FROM ENDCLI E
                     INNER JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA) Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S') ED ON C.CLICODIGO=ED.CLICODIGO
                      WHERE CLICLIENTE='S'),
              
              PROD AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE PROCODIGO='LA0008'),
              
              CLIPRO AS (SELECT C.CLICODIGO,CLINOMEFANT,SETOR,C.TBPCODIGO FROM CLITBP C
                          INNER JOIN TAB T ON C.TBPCODIGO=T.TBPCODIGO
                           INNER JOIN CLI CL ON C.CLICODIGO=CL.CLICODIGO)
              
              SELECT CLICODIGO,
                      CLINOMEFANT,
                       SETOR,
                      TP.TBPCODIGO,
                       DESCRICAO,
                       VALIDADE,
                       TP.PROCODIGO,
                        PRODESCRICAO,
                         TBPPCOVENDA VALOR,
                          TBPPCOVENDA2 VALOR2,
                           TBPPCDESCTO DESCONTO,
                            TBPPCDESCTO2 DESCONTO2
                             FROM TBPPRODU TP
               INNER JOIN TAB T ON TP.TBPCODIGO=T.TBPCODIGO
                INNER JOIN PROD P ON TP.PROCODIGO=P.PROCODIGO
                 INNER JOIN CLIPRO C ON TP.TBPCODIGO=C.TBPCODIGO
                 
    ")  

View(acordos)

range_write("18q_s0MhDn5BL_inV3WOMQipkHehups4usNoH0G1AvyQ",data=acordos,sheet = "ACORDOS",
            range = "A1",reformat = FALSE) 

combinados <- dbGetQuery(con2,"
              WITH TAB AS (SELECT TBPCODIGO,TBPDESCRICAO DESCRICAO,TBPDTVALIDADE VALIDADE  FROM TABPRECO 
              WHERE TBPDTVALIDADE>='TODAY' AND TBPDESCRICAO NOT LIKE'%PROMO DO MES%'),
              
              CLI AS (SELECT C.CLICODIGO,
                CLINOMEFANT,
                 GCLCODIGO GRUPO, 
                  SETOR 
                   FROM CLIEN C
                    INNER JOIN (SELECT CLICODIGO, ZODESCRICAO SETOR FROM ENDCLI E
                     INNER JOIN (SELECT ZOCODIGO,ZODESCRICAO FROM ZONA) Z ON E.ZOCODIGO=Z.ZOCODIGO WHERE ENDFAT='S') ED ON C.CLICODIGO=ED.CLICODIGO
                      WHERE CLICLIENTE='S'),
              
              PROD AS (SELECT PROCODIGO,PRODESCRICAO FROM PRODU WHERE PROCODIGO='LA0008'),
              
              CLIPRO AS (SELECT C.CLICODIGO,CLINOMEFANT,SETOR,C.TBPCODIGO FROM CLITBPCOMB C
                          INNER JOIN TAB T ON C.TBPCODIGO=T.TBPCODIGO
                           INNER JOIN CLI CL ON C.CLICODIGO=CL.CLICODIGO)
              
              SELECT CLICODIGO,
                      CLINOMEFANT,
                       SETOR,
                        TP.TBPCODIGO CODTABELA,
                         VALIDADE,
                          PROCODIGOA,
                           CCINDICEPROA DESCONTOA,
                            CCPCOVENDAPROA VALORA,
                             CCINDICEPROA2 DESCONTOA2,
                              CCPCOVENDAPROA2 VALORA2,
                               PROCODIGOB,
                                CCINDICEPROB DESCONTOB,
                                 CCPCOVENDAPROB VALORB,
                                  CCINDICEPROB2 DESCONTOB2,
                                   CCPCOVENDAPROB2 VALORB2
                        
                         FROM TBPCOMBPROPRO TP
               INNER JOIN TAB T ON TP.TBPCODIGO=T.TBPCODIGO
                INNER JOIN PROD P ON TP.PROCODIGOA=P.PROCODIGO
                 INNER JOIN CLIPRO C ON TP.TBPCODIGO=C.TBPCODIGO
                 
    ")  

View(combinados)

range_write("18q_s0MhDn5BL_inV3WOMQipkHehups4usNoH0G1AvyQ",data=combinados,sheet = "COMBINADOS",
            range = "A1",reformat = FALSE) 
