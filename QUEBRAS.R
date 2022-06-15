## RELATÓRIO DE QUEBRAS E RETORNOS 
## INDICADOR VARIÁVEL RH

## Bibliotecas necessárias

library(DBI)
library(dplyr)
library(reshape2)
library(googlesheets4)


quebras <- dbGetQuery(con2,"
select pdx.ID_PEDORI as pedcodigo 
      , ped.peddtemis  
      , ped.empcodigo
      , CAST(pro.prodescricao as Varchar(255)) as prodescricao 
      , pdp.PdpQtdade 
      , (pdp.PdpQtdade * pdp.PDPUNITLIQUIDO) Valor  
      , sol.funnome    as SOLICITANTE
      , ven.funnome    as AUTORIZADOR
      , case when ped.pedorigem = 'R' then 'INT'
             when ped.pedorigem = 'X' then 'EXT'
             when ped.pedorigem = 'Z' then 'RID'
             when ped.pedorigem = 'Q' then 'RTQ'
             when ped.pedorigem = 'T' then 'RET'
        else 
             cast( '' as varchar(18)) 
        end as TipoReq 
      , alm.alxdescricao DescCelula
      , tpo.tpocdescricao as MOTIVO 
      , csa.tpudescricao as Causa 
  from pedid ped 
     left join movocorrencia moc on (ped.id_pedido = moc.id_pedido or ( ( Select pxp.id_pedori from pedxped pxp where pxp.id_peddes = ped.id_pedido and pxp.pedorigemdiv = 'P' ) = moc.id_pedido )  ) 
     left join PEDXPED pdx on (pdx.ID_PEDDES = ped.ID_PEDIDO) 
       left join tpcausa csa on (moc.tpucodigo = csa.tpucodigo) 
       left join almox alm on (alm.alxcodigo = moc.alxcodigo 
                           and alm.empcodigo = moc.empcodigo) 
       left join tpocorrencia tpo on (tpo.tpocCodigo = moc.tpocCodigo) 
       left join pdprd pdp   on (pdp.id_pedido = ped.id_pedido) 
       left join premp pre   on (pre.procodigo = pdp.procodigo and pre.empcodigo = pdp.empcodigo)
       left join Produ pro   on (pro.procodigo = pdp.procodigo) 
       left join tplente tpl on (tpl.tplcodigo = pro.tplcodigo) 
       left join funcio sol  on (sol.funcodigo = ped.funcodigo) 
       left join funcio ven  on (ven.funcodigo = ped.funcodigo2) 
       left join clien cli   on (cli.clicodigo = ped.clicodigo)
       INNER JOIN (SELECT FUNCODIGO,FUNNOME FROM FUNCIO WHERE 
                                     (DPTCODIGO=2 AND FUNATIVO='A' OR FUNCODIGO=43))A ON PED.FUNCODIGO=A.FUNCODIGO
 where ped.empcodigo in (1,3,4,5,8)
   and pdp.id_pedido  is not null 
   and ( (tpl.tpltipo <> 'N') or (pdp.procodigo starting 'PDC') )
   and moc.mvocporigem = 'R'
   and Ped.PedSitPed  <> 'C'
   and pro.protipo   not in ('S','M','C','T')
   and ped.peddtemis BETWEEN '01.05.2022' AND '31.05.2022'
   and ped.pedorigem in ('R', 'X')

 order by 1")

View(quebras)

range_write("16VH_47hg-sI9rgd0hnttImDTEM3xvHg3-7cNeUCASHE",data=quebras,sheet = "QUEBRAS",
            range = "A1",reformat = FALSE) 


quebras_atn <-read_sheet("16VH_47hg-sI9rgd0hnttImDTEM3xvHg3-7cNeUCASHE",sheet = "DADOS") %>% 
  filter(ATENDIMENTO=='atendimento') %>% select(SOLICITANTE)


inner_join(quebras,quebras_atn,by="SOLICITANTE") %>% filter(TIPOREQ=='INT') %>% View()
summarize(v=PDPQTDADE)

inner_join(quebras,quebras_atn,by="SOLICITANTE") %>% filter(TIPOREQ=='INT') %>%
summarize(v=sum(PDPQTDADE))

inner_join(quebras,quebras_atn,by="SOLICITANTE") %>% filter(TIPOREQ=='EXT') %>% View()

inner_join(quebras,quebras_atn,by="SOLICITANTE") %>% filter(TIPOREQ=='EXT') %>% 
  summarize(v=sum(PDPQTDADE)*2)
  
  
  

pedidos_quebras <- dbGetQuery(con2,"SELECT ID_PEDIDO,P.FUNCODIGO COD,FUNNOME NOME,PEDORIGEM 
                                      FROM PEDID P
                                     INNER JOIN (SELECT FUNCODIGO,FUNNOME FROM FUNCIO WHERE 
                                     (DPTCODIGO=2 AND FUNATIVO='A' OR FUNCODIGO=43))A ON P.FUNCODIGO=A.FUNCODIGO
                                     WHERE PEDORIGEM='D'
                                     AND PEDDTBAIXA  BETWEEN '01.05.2022' AND '31.05.2022'")

View(pedidos_quebras)


range_write("16VH_47hg-sI9rgd0hnttImDTEM3xvHg3-7cNeUCASHE",data=pedidos_quebras,sheet = "PEDIDOS DIGITADOS ATENDIMENTO",
            range = "A1",reformat = FALSE) 


pedidos <- dbGetQuery(con2,"SELECT ID_PEDIDO,FUNCODIGO2,FUNNOME,PEDORIGEM 
                                      FROM PEDID P
                                     INNER JOIN (SELECT FUNCODIGO,FUNNOME FROM FUNCIO)A ON P.FUNCODIGO=A.FUNCODIGO
                                     WHERE PEDORIGEM='D'
                                     AND PEDDTBAIXA  BETWEEN '01.05.2022' AND '31.05.2022'")

View(pedidos)






