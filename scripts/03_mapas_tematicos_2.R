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

#PREPARANDO DADOS DE LETALIDADE POR SRAG COVID-19
casos_uf = import("dados/sivep2020_casos.RDS")

sivep2020 = import("dados/sivep_gripe_2020.csv")
sivep2020_obitos = sivep2020 |>  #criando um data.frame que recebe dados do original
  select(EVOLUCAO, SG_UF_NOT) |> #selecionando as variáveis de interesse
  filter(EVOLUCAO == 2) |>       #selecionando apenas dados de Covid-19
  select(SG_UF_NOT) |>           #selecionando apenas dados de UF
  group_by(SG_UF_NOT) |>         #agrupando dados por UF
  summarise(OBITOS = n())        #contando número de óbitos por UF

##CALCULANDO TAXA DE LETALIDADE
letal_uf = casos_uf |>
  left_join(sivep2020_obitos, by = c("SG_UF_NOT" = "SG_UF_NOT"))
letal_uf$TX_LETALIDADE = round(letal_uf$OBITOS/letal_uf$CASOS*100, 2)

##CALCULANDO TAXA DE INCIDÊNCIA
populacao_uf = import("dados/populacao_uf_2020.csv")
letal_uf = letal_uf |>
  left_join(populacao_uf, by = c("SG_UF_NOT" = "sigla"))
letal_uf$TX_INCIDENCIA = round(letal_uf$CASOS/letal_uf$pop*100000, 2)

#IMPORTANDO SHAPEFILE DO BRASIL 
br_shp = read_sf('dados/BR_UF_2021/BR_UF_2021.shp')
plot(br_shp) #plotando atributos básicos do shapefile
glimpse(br_shp) #visualizando a tabela de atributos

ggplot() + #plotando o mapa com os dados do Brasil
  geom_sf(data= br_shp)

ggplot() + #personalizando o mapa com os dados do Brasil
  geom_sf(data= br_shp, 
          fill = "#009c3b", #verde da bandeira do Brasil em cor hexadecimal
          color = "white", size = 1)+
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = "black", size = 0.2))

ggplot() + #realizando um filtr de unidades federadas
  geom_sf(data = br_shp %>% 
          filter(NM_UF == "Santa Catarina" | SIGLA == "RS"), 
          fill = "#002776", #azul da bandeira do Brasil em cor hexadecimal
          color = "white",
          alpha = 0.9,
          size = 1.2)+
  theme_dark()

#ELABORANDO MAPAS DE CENTROIDE POR UF 

##UNINDO DATA.FRAME DE LETALIDADE E INCIDÊNCIA COM O SHAPEFILE DO BRASIL
df_let = data.frame(
  CD_UF = as.character(letal_uf$ibge),
  TX_LETALIDADE = letal_uf$TX_LETALIDADE,
  TX_INCIDENCIA = letal_uf$TX_INCIDENCIA
)

br_shp_let = br_shp |>
  left_join(df_let, by = "CD_UF")

coord_pontos <- br_shp_let %>% #gerando os centroides para o nosso mapa
  st_centroid()

ggplot() + #criando mapade centroides
  geom_sf(data = br_shp_let, fill = "#009c3b",
          color = "white", size = 1, alpha = 0.5) +
  geom_sf(data = coord_pontos, aes(size = TX_LETALIDADE), col = "black", alpha = 0.9,
          show.legend = "point")+
  theme(panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_line(color = "white", size = 0.2))  

#ELABORANDO MAPAS COMPOSTOS: INFORMAÇÕES EM POLÍGONOS E CENTROIDES
ggplot() +
  geom_sf(data = br_shp_let, aes(fill = TX_INCIDENCIA), color = "white", size = 0.8, alpha = 0.8) + 
  geom_sf(data = coord_pontos, aes(size = TX_LETALIDADE), color = "black", alpha = 0.9,
          show.legend = "point")+
  theme_bw()

