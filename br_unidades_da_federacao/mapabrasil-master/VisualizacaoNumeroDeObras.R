# Carregando os pacotes----
library(maptools)     
library(spdep)           
library(leaflet)        
library(dplyr)
library(rgdal)
library(dplyr)
library(RColorBrewer) 
library(readr)
# Importando shapefile (mapa do Brasil)----

shp <- readOGR("/home/jaogjao/Documentos/Faculdade/2017-2/VD/Projeto/ProjetoVisualizacaodeDados/br_unidades_da_federacao/mapabrasil-master/Mapa", "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")

class(shp)

# Importando dataset----
df <- read_csv("~/Documentos/Faculdade/2017-2/VD/Projeto/ProjetoVisualizacaodeDados/br_unidades_da_federacao/mapabrasil-master/PAC_2017_06.csv")


# Importando códigos do IBGE e adicionando ao dataset----

ibge <- read.csv("/home/jaogjao/Documentos/Faculdade/2017-2/VD/Projeto/ProjetoVisualizacaodeDados/br_unidades_da_federacao/mapabrasil-master/Dados/estadosibge.csv", header=T,sep=",")

df$concluida = 0
#coloca 0 onde é null
for(i in 1:nrow(df)){
  if(df$idn_estagio[i] == 90){
    df$concluida[i] = 1
  }
}

#novo = df[!nchar(df$sig_uf) > 2,] # remove 96 linhas
novo=df

#soma de cada UF

novo$cumsum = 1
novo <- novo %>% group_by(dsc_orgao) %>% mutate(cumsum = cumsum(cumsum),concluida = cumsum(concluida))


#deixa so o estado e o investimento total
novo <- novo %>%
  group_by(dsc_orgao) %>%
  summarise(nroObras = max(cumsum),nroCompletas = max(concluida))



library('plotly')
novo$dsc_orgao[1] = "Ministerio da ciencia"
data <- data.frame(novo$dsc_orgao, novo$nroObras, novo$nroCompletas)
Encoding(novo$dsc_orgao)
p <- plot_ly(data, orientation = 'h', x = ~novo$dsc_orgao, y = ~novo$nroObras, type = 'bar', name = 'Obras em andamento') %>%
  add_trace(x = ~novo$nroCompletas, name = 'Obras Concluidas') %>%
  layout(yaxis = list(title = 'Total De Obras'), xaxis = list(title = "Ministerio"),barmode = 'stack')
p


library(plotly)

y <- c('giraffes', 'orangutans', 'monkeys')
SF_Zoo <- c(20, 14, 23)
LA_Zoo <- c(12, 18, 29)
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
         xaxis = list(title = ""),
         yaxis = list(title =""))
p





