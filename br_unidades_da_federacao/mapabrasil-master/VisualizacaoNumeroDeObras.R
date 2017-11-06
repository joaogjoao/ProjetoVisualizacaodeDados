# Carregando os pacotes----
library(maptools)     
library(spdep)           
library('plotly')
library(leaflet)        
library(dplyr)
library(rgdal)
library(dplyr)
library(RColorBrewer) 
library(readr)
# Importando dataset----
df <- read_csv("~/Documentos/Faculdade/2017-2/VD/Projeto/ProjetoVisualizacaodeDados/br_unidades_da_federacao/mapabrasil-master/PAC_2017_06.csv")

#coloca 1 onde é conlcuida
df$concluida = 0
for(i in 1:nrow(df)){
  if(df$idn_estagio[i] == 90){
    df$concluida[i] = 1
  }
}
novo=df
#


#soma de cada Orgao

novo$cumsum = 1


novo <- novo %>% group_by(dsc_orgao) %>% mutate(cumsum = cumsum(cumsum),concluida = cumsum(concluida))


#deixa so o estado e o investimento total
novo <- novo %>%
  group_by(dsc_orgao) %>%
  summarise(nroObras = max(cumsum),nroCompletas = max(concluida))

novo$nroIncompletas = 0
novo$nroIncompletas = novo$nroObras - novo$nroCompletas
novo$dsc_orgao[1] = "Ministerio da Ciencia"
data <- data.frame(novo$dsc_orgao, novo$nroIncompletas, novo$nroCompletas)

p <- plot_ly(data, x = ~novo$nroIncompletas, y = ~novo$dsc_orgao, type = 'bar', orientation = 'h', name = 'Concluidas',
             marker = list(color = 'rgba(246, 50, 50, 0.5)',
                           line = list(color = 'rgba(255, 50, 50, 1.0)',
                                       width = 1))) %>%
  add_trace(x = ~novo$nroCompletas, name = 'Incompletas',
            marker = list(color = 'rgba(58, 71, 80, 0.5)',
                          line = list(color = 'rgba(58, 71, 80, 1.0)',
                                      width = 1))) %>%
   add_trace(x = ~novo$nroObras, name = 'Total de Obras',
             marker = list(color = 'rgba(255, 255, 255, 0)',
                           line = list(color = 'rgba(255, 255, 2, 0)',
                                       width = 0.1))) %>%
  layout(margin = list(l = 400, r = 0, b = 50, t = 0, pad = 4),
        barmode = 'stack',
         yaxis = list(title = ""),
         xaxis = list(title ="Quantidade de Obras")
         )
p





