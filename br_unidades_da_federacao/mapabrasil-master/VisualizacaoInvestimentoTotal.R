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

df$temvalor = 0
#coloca 0 onde é null
for(i in 1:nrow(df)){
  if(is.na(df$investimento_total[i])){
    df$investimento_total[i] = 0
    df$temvalor[i] = 1
    
  }
}

novo = df[!nchar(df$sig_uf) > 2,] # remove 96 linhas referentes as obras multiestaduais


#soma de cada UF

novo <- novo %>% group_by(sig_uf) %>% mutate(numero = cumsum(1))

novo <- novo %>% group_by(sig_uf) %>% mutate(numero = cumsum(numero))

novo <- novo %>% group_by(sig_uf) %>% mutate(cumsum = cumsum(investimento_total))
novo <- novo %>% group_by(sig_uf) %>% mutate(temvalor = cumsum(temvalor))


#deixa so o estado e o investimento total
novo <- novo %>%
  group_by(sig_uf) %>%
  summarise(Invest_estado = max(cumsum), Obras = max(numero), temvalor = max(temvalor))

#merge entre valor de cada estado e siglas e numeros do ibge
novo <- merge(novo,ibge, by.x = "sig_uf", by.y = "UF")


# Fazendo a junção entre o dataset e o shapefile----
  
brasileiropg <- merge(shp,novo, by.x = "CD_GEOCUF", by.y = "Codigo.UF")

#Tratamento e transformação dos dados----

proj4string(brasileiropg) <- CRS("+proj=longlat +datum=WGS84 +no_defs") #adicionando coordenadas geográficas

Encoding(brasileiropg$NM_ESTADO) <- "UTF-8"

# Gerando o mapa----

display.brewer.all()

oi = brasileiropg$Invest_estado
oi[17] = oi[22]


pal <- colorBin("YlGn",domain = oi ,n = 9)#max(novo$Invest_estado)) #cores do mapa
print(pal(brasileiropg$Invest_estado))
state_popup <- paste0("<strong>Estado: </strong>", 
                      brasileiropg$NM_ESTADO,
                      "<br><strong>Investimento Total: </strong>", 
                      brasileiropg$Invest_estado,
                      "<br><strong>Obras Realizadas: </strong>", 
                      brasileiropg$Obras,
                      "<br><strong>Obras Sem Valor Divulgado: </strong>", 
                      brasileiropg$temvalor)
leaflet(data = brasileiropg) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = TRUE,
              fillColor = ~pal(oi), 
              #smoothFactor = 0.2,
              fillOpacity = 1, 
              color = "#000000",
              
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomright", pal = pal, values = ~oi,
            title = "Investimento das Obras do PAC",
            labFormat = labelFormat(prefix = "R$ ",),
            opacity = 1)






