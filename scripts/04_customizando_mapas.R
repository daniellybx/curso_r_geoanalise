##INSTALANDO E CARREGANDO PACOTES COM O PACMAN
library(pacman)
p_load(
  geobr,           #pacote com datasets geoespaciais do Brasil
  ggsn,            #símbolos de direção e barras de escala para pacotes criados com "ggplot2" ou "ggmap"    
  ggspatial,       #dados espaciais de suporte para o pacote "ggplot2"
  gpclib,          #tratamento de feições de polígonos/áreas para o R 
  jsonlite,        #leitor e gerador de arquivos no formato JSON
  leaflet,         #versão do R para a biblioteca Leaflet do Javascript para mapas interativos
  lubridate,       #pacote para tratamento de datas no R 
  maptools,        #manipulação de dados geográficos
  RColorBrewer,    #paletas de cor para visualizações do R
  RCurl,           #interface de client para o R (usado para simular visualizações web)
  rgdal,           #funções de integração para a biblioteca gdal
  rgeos,           #operações com topologia e geometrias
  rio,             #importação e exportação de arquivos variados
  rjson,           #conversão de arquivos JSON 
  sf,              #pacote para a leitura de arquivos .shp
  tidyverse        #conjunto de pacotes de manipulação e tratamento de dados e criação de gráficos
)

#CRIANDO O MAPA QUE SERÁ CUSTOMIZADO

#PREPARANDO DADOS DE INCIDÊNCIA POR SRAG COVID-19
casos_uf = import("dados/sivep2020_casos.RDS")

##CALCULANDO TAXA DE INCIDÊNCIA
populacao_uf = import("dados/populacao_uf_2020.csv")

casos_uf = casos_uf |>
  left_join(populacao_uf, by = c("SG_UF_NOT" = "sigla"))

casos_uf$TX_INCIDENCIA = round(casos_uf$CASOS/casos_uf$pop*100000, 2)

##IMPORTANDO SHAPEFILE DO BRASIL 
br_shp = read_sf('dados/BR_UF_2021/BR_UF_2021.shp')
plot(br_shp) #plotando atributos básicos do shapefile
glimpse(br_shp) #visualizando a tabela de atributos

ggplot() + #plotando o mapa com os dados do Brasil
  geom_sf(data= br_shp)

##UNINDO DADOS DO MAPA DO BRASIL COM OS DADOS DE INCIDÊNCIA
df_inc = data.frame(
  CD_UF = as.character(casos_uf$ibge),
  TX_INCIDENCIA = casos_uf$TX_INCIDENCIA
)

br_shp_inc = br_shp |>
  left_join(df_inc, by = "CD_UF")

##ELABORANDO MAPAS TEMÁTICO DE TAXA DE INCIDÊNCIA
ggplot() +
  geom_sf(data = br_shp_inc, aes(fill = TX_INCIDENCIA))

#CUSTOMIZANDO O MAPA
