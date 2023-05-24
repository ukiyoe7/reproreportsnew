
WITH
   FIS AS (SELECT FISCODIGO FROM TBFIS WHERE FISTPNATOP IN ('V','R','SR')),
    
    PED AS (SELECT ID_PEDIDO,
                     PEDDTBAIXA,
                      P.CLICODIGO,
                        FISCODIGO1
                            FROM PEDID P
                             LEFT JOIN FIS ON P.FISCODIGO1=FIS.FISCODIGO
                               WHERE PEDDTBAIXA BETWEEN '01.05.2023' AND 'YESTERDAY' AND PEDSITPED<>'C' ),
                               
      PROD AS (SELECT PROCODIGO FROM PRODU WHERE PROTIPO='T' AND PROCODIGO NOT IN ('TRHC','TRHCF','TRHCT2','TRHCT','COLEXT','INTOPT','INTOPTC')),
      
      CUSTO_MEDIO AS (SELECT PROCODIGO, PREPCOMEDIO CUSTOMEDIO FROM PREMP WHERE EMPCODIGO=1),
      
      CFOP_CORTESIA AS (SELECT FISCODIGO,FISDESCRICAO FROM TBFIS WHERE FISDESCRICAO LIKE '%BONI%')
    
      SELECT PD.ID_PEDIDO,
              PEDDTBAIXA,
               CLICODIGO,
                FISCODIGO1,
                 IIF(CC.FISCODIGO IS NOT NULL,1,0) CORTESIA,
                 PD.PROCODIGO,
                  PDPDESCRICAO,
                   CUSTOMEDIO,
                             SUM(PDPQTDADE)QTD,
                              SUM(PDPUNITLIQUIDO*PDPQTDADE)VRVENDA
                                FROM PDPRD PD
                                 INNER JOIN PED P ON PD.ID_PEDIDO=P.ID_PEDIDO
                                  INNER JOIN PROD PR ON PD.PROCODIGO=PR.PROCODIGO
                                   LEFT JOIN CUSTO_MEDIO CM ON CM.PROCODIGO=PD.PROCODIGO
                                    LEFT JOIN CFOP_CORTESIA CC ON CC.FISCODIGO=P.FISCODIGO1
                                     GROUP BY 1,2,3,4,5,6,7,8 ORDER BY ID_PEDIDO DESC
                               
                               