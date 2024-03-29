WITH RESULTADO AS ( 
SELECT PED.PEDDTEMIS, PED.PEDCODIGO 
 , PED.PEDPZENTRE, PED.PEDHRENTRE
, PEDDTSAIDA, PED.CLICODIGO, PED.ENDCODIGO, 
PEDVRMERC, PEDVRSERVI, PEDVRDESCTO, PEDVRDESCTOSER, PEDVRTOTAL, PEDPCCOMIS, PEDSITPED, 
CLI.CLIRAZSOCIAL AS CLIRAZSOCIAL, EDC.CIDCODIGO, CID.CIDNOME, EDC.ENDDDD1, EDC.ENDFONE1,PEDPZETGSIS, 
JB.JBCODIGO, CB.CORDESCRICAO, LP.LPDESCRICAO, PED.ID_PEDIDO, LP.LPCODIGO
, ((SELECT SUM(P.PDPQTDADE) 
      FROM PDPRD P 
     WHERE P.ID_PEDIDO = PED.ID_PEDIDO 
   )) AS QTDE 
 ,PED.PEDORDEMCOMPRA 
 ,AP.APCODIGO
 ,CLI2.CLICODIGO AS CLIORIGINAL
FROM PEDID PED LEFT JOIN CLIEN CLI ON PED.CLICODIGO  = CLI.CLICODIGO
               LEFT JOIN CLIEN CLI2 ON CLI2.CLICODIGO = ( SELECT FIRST 1 PED2.CLICODIGO 
                                                            FROM PEDXPED PXP 
                                                            INNER JOIN PEDID PED2 ON PED2.ID_PEDIDO = PXP.ID_PEDORI 
                                                                                  AND PXP.PEDORIGEMDIV = 'N' 
                                                            WHERE PXP.ID_PEDDES = PED.ID_PEDIDO) 
               LEFT JOIN ENDCLI EDC ON PED.CLICODIGO = EDC.CLICODIGO AND PED.ENDCODIGO = EDC.ENDCODIGO
               LEFT JOIN CIDADE CID ON EDC.CIDCODIGO = CID.CIDCODIGO
               LEFT JOIN JETBOX JB ON JB.ID_PEDIDO   = PED.ID_PEDIDO
               LEFT JOIN CORBOX CB  ON CB.CORCODIGO  = JB.CORCODIGO 
               LEFT JOIN ACOPED AP  ON AP.ID_PEDIDO  = PED.ID_PEDIDO 
               LEFT JOIN LOCALPED LP ON LP.LPCODIGO  = AP.LPCODIGO 
               LEFT JOIN PDPRD PDP ON PDP.ID_PEDIDO  = PED.ID_PEDIDO 
               LEFT JOIN PRODU PRO ON PRO.PROCODIGO  = PDP.PROCODIGO 
               LEFT JOIN MOVOCORRENCIA MVO ON MVO.ID_PEDIDO = PED.ID_PEDIDO
               LEFT JOIN PEDIDPROCES PP ON PED.ID_PEDIDO = PP.ID_PEDIDO
               LEFT JOIN PEDROTEIRO PR ON PED.ID_PEDIDO = PR.ID_PEDIDO
WHERE PED.PEDDTEMIS BETWEEN @#DATE#EMIS_INICIAL@ AND  @#DATE#EMIS_FINAL@
AND( PEDSITPED = 'A' OR PEDSITPED = 'F' )
 GROUP BY PED.PEDDTEMIS 
        , PED.PEDCODIGO 
        , PED.PEDPZENTRE, PED.PEDHRENTRE
        , PEDDTSAIDA 
        , PED.CLICODIGO 
        , PED.ENDCODIGO 
        , PEDVRMERC 
        , PEDVRSERVI 
        , PEDVRDESCTO 
        , PEDVRDESCTOSER 
        , PEDVRTOTAL 
        , PEDPCCOMIS 
        , PEDSITPED 
        , CLIRAZSOCIAL 
        , EDC.CIDCODIGO 
        , CID.CIDNOME 
        , EDC.ENDDDD1 
        , EDC.ENDFONE1 
        , PEDPZETGSIS
        , JB.JBCODIGO 
        , CB.CORDESCRICAO 
        , LP.LPDESCRICAO 
        , PED.ID_PEDIDO 
        , LP.LPCODIGO 
 , PED.PEDORDEMCOMPRA 
 , AP.APCODIGO 
 , CLI2.CLICODIGO )
 SELECT   R.PEDDTEMIS 
        , R.PEDCODIGO 
        , R.PEDPZENTRE 
        , R.PEDHRENTRE 
        , R.PEDDTSAIDA 
        , R.CLICODIGO 
        , R.ENDCODIGO 
        , R.PEDVRMERC 
        , R.PEDVRSERVI 
        , R.PEDVRDESCTO 
        , R.PEDVRDESCTOSER 
        , R.PEDVRTOTAL 
        , R.PEDPCCOMIS 
        , R.PEDSITPED 
        , R.CLIRAZSOCIAL 
        , R.CIDCODIGO 
        , R.CIDNOME 
        , R.ENDDDD1 
        , R.ENDFONE1 
        , R.PEDPZETGSIS
        , R.JBCODIGO 
        , R.CORDESCRICAO 
        , R.LPDESCRICAO 
        , R.ID_PEDIDO 
        , R.LPCODIGO 
        , COALESCE(R.CLIORIGINAL, R.CLICODIGO) CLIORIGINAL 
        , R.QTDE 
        , R.PEDORDEMCOMPRA 
        , R.APCODIGO 
 FROM RESULTADO R 
WHERE R.ID_PEDIDO > 0 
  AND R.APCODIGO = (SELECT MAX(APCODIGO) FROM ACOPED APO 
      LEFT JOIN LOCALPED LP2 ON APO.LPCODIGO = LP2.LPCODIGO 
  WHERE APO.ID_PEDIDO = R.ID_PEDIDO )
 AND EXISTS (SELECT 1 FROM ACOPED ACP WHERE ACP.ID_PEDIDO= R.ID_PEDIDO AND ACP.LPCODIGO = 1856)
ORDER BY R.LPCODIGO, R.PEDDTEMIS, R.PEDPZENTRE, R.PEDHRENTRE, R.PEDCODIGO