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


df$concluida = 0

#coloca 1 onde é conlcuida
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



novo$dsc_orgao[1] = "Ministerio da Ciencia"
# data <- data.frame(novo$dsc_orgao, novo$nroObras, novo$nroCompletas)
# Encoding(novo$dsc_orgao)
# p <- plot_ly(data, orientation = 'h', x = ~novo$dsc_orgao, y = ~novo$nroObras, type = 'bar', name = 'Obras em andamento') %>%
#   add_trace(x = ~novo$nroCompletas, name = 'Obras Concluidas') %>%
#   layout(yaxis = list(title = 'Total De Obras'), xaxis = list(title = "Ministerio"),barmode = 'stack')
# 

data <- data.frame(novo$dsc_orgao, novo$nroObras, novo$nroCompletas)

p <- plot_ly(data, x = ~novo$nroObras, y = ~novo$dsc_orgao, type = 'bar', orientation = 'h', name = 'Concluidas',
             marker = list(color = 'rgba(246, 78, 139, 0.6)',
                           line = list(color = 'rgba(246, 78, 139, 1.0)',
                                       width = 3))) %>%
  add_trace(x = ~novo$nroCompletas, name = 'Incompletas',
            marker = list(color = 'rgba(58, 71, 80, 0.6)',
                          line = list(color = 'rgba(58, 71, 80, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack',
         yaxis = list(title = ""),
         xaxis = list(title ="Quantidade de Obras"))
p





