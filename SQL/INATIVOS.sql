SELECT DISTINCT SITCLI.CLICODIGO,SITCODIGO FROM SITCLI
  INNER JOIN (SELECT DISTINCT SITCLI.CLICODIGO,MAX(SITDATA)ULTIMA FROM SITCLI
  GROUP BY 1)A ON SITCLI.CLICODIGO=A.CLICODIGO AND A.ULTIMA=SITCLI.SITDATA 
  INNER JOIN (SELECT DISTINCT SITCLI.CLICODIGO,SITDATA,MAX(SITSEQ)USEQ FROM SITCLI
  GROUP BY 1,2)MSEQ ON A.CLICODIGO=MSEQ.CLICODIGO AND MSEQ.SITDATA=A.ULTIMA 
  AND MSEQ.USEQ=SITCLI.SITSEQ WHERE SITCODIGO=4