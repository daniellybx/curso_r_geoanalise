#AULA 4 - PERSONALIZANDO MAPAS

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

##GERANDO QUEBRAS DE ACORDO COM OS QUARTIS 
quartis = quantile(br_shp_inc$TX_INCIDENCIA, prob=c(0.25, 0.5, 0.75))
br_shp_inc$quartis = factor(findInterval(br_shp_inc$TX_INCIDENCIA, quartis))
levels(br_shp_inc$quartis) <- c("0 - 246", "247-304", "305-322", ">322") #usando os valores dos quartis para gerar a variável

##PLOTANDO O MAPA 
ggplot()+
  geom_sf(data = br_shp_inc, 
               aes(fill = quartis))
  
##MODIFICANDO PALETA DE CORES MANUALMENTE
pallete = c("#dff6ff", "#5d8bf4", "#2d31fa", "#051367") #tons graduados de azul

ggplot()+
  geom_sf(data = br_shp_inc, 
          aes(fill = quartis))+
  scale_fill_manual(values = pallete)

##MODIFICANDO PALETA COM O PACOTE "RColorBrewer"
display.brewer.all() #plotando paletas de cores

ggplot()+
  geom_sf(data = br_shp_inc, 
          aes(fill = quartis))+
  scale_fill_brewer(palette="BuPu")

##MOFICICANDO ESPESSURA DAS BORDAS
ggplot()+
  geom_sf(data = br_shp_inc, 
          aes(fill = quartis), color = "white", size = 1)+ #incluindo cor branca na borda e tamanho igual a 1
  scale_fill_brewer(palette="BuPu")

ggplot()+
  geom_sf(data = br_shp_inc, 
          aes(fill = quartis), color = "gray67", size = 1)+ #incluindo cor cinza na borda e tamanho igual a 1
  scale_fill_brewer(palette="BuPu")

##DESTACANDO UMA ÁREA GEOGRÁFICA

ggplot() + #realizando um filter de unidades federadas
  geom_sf(data = br_shp_inc %>% 
          filter(NM_REGIAO == "Nordeste"), 
          aes(fill = quartis), #graduando por quartis nacionais
          color = "gray60",    #incluindo borda cinza
          alpha = 0.9,         #gerando transparencia
          size = 1.2)+         #escolhendo tamanho da borda  
  scale_fill_brewer(palette="BuGn")+
  guides(fill=guide_legend(title="Taxa de incidência"))+ #mudando título da legenda
  theme_bw()

##ESCALA

ggplot()+
  geom_sf(data = br_shp_inc, 
          aes(fill = quartis), 
          color = "gray67", 
          size = 1)+ 
  scale_fill_brewer(palette="PuRd")+
  guides(fill=guide_legend(title="Taxa de incidência"))+ 
  theme_bw()+
  annotation_scale() #adicionando escala

ggplot()+
  geom_sf(data = br_shp_inc, 
          aes(fill = quartis), 
          color = "gray67", 
          size = 1)+ 
  scale_fill_brewer(palette="PuRd")+
  guides(fill=guide_legend(title="Taxa de incidência"))+ 
  theme_bw()+
  annotation_scale(pad_x = unit(7, "cm"), #mudando escala de lugar
                   pad_y = unit(0.2, "cm"))

##ROSA DOS VENTOS

ggplot()+
  geom_sf(data = br_shp_inc, 
          aes(fill = quartis), 
          color = "gray67", 
          size = 1)+ 
  scale_fill_brewer(palette="Greens")+
  guides(fill=guide_legend(title="Taxa de incidência"))+ 
  theme_bw()+
  annotation_scale(pad_x = unit(7.4, "cm"), 
                   pad_y = unit(0.3, "cm"))+
  annotation_north_arrow(style=north_arrow_nautical()) #adicionando rosa dos ventos

ggplot()+
  geom_sf(data = br_shp_inc, 
          aes(fill = quartis), 
          color = "gray67", 
          size = 1)+ 
  scale_fill_brewer(palette="Greens")+
  guides(fill=guide_legend(title="Taxa de incidência"))+ 
  theme_bw()+
  annotation_scale()+
  annotation_north_arrow(style=north_arrow_orienteering(), #modificando rosa dos ventos para seta de norte
                         pad_x = unit(9, "cm"),            #mudando posição da seta
                         pad_y = unit(8, "cm")) 

##REMOVENDO LATITUDE E LONGITUDE DAS MARGENS

ggplot()+
  geom_sf(data = br_shp_inc, 
          aes(fill = quartis), 
          color = "gray67", 
          size = 1)+ 
  scale_fill_brewer(palette="Blues")+
  guides(fill=guide_legend(title="Taxa de incidência"))+ 
  theme(panel.background = element_blank(), #mudando o tema para excluir a grade de fundo
        axis.ticks = element_blank(),       #excluindo dados dos eixos
        axis.text = element_blank())+       #excluindo títulos dos eixos
  annotation_scale()+
  annotation_north_arrow(style=north_arrow_orienteering(), 
                         pad_x = unit(9, "cm"),           
                         pad_y = unit(8, "cm")) 

##TÍTULO

ggplot()+
  geom_sf(data = br_shp_inc, 
          aes(fill = quartis), 
          color = "gray67", 
          size = 1)+ 
  scale_fill_brewer(palette="Purples")+
  guides(fill=guide_legend(title="Taxa de incidência"))+ 
  theme(panel.background = element_blank(), 
        axis.ticks = element_blank(),       
        axis.text = element_blank())+       
  annotation_scale()+
  annotation_north_arrow(style=north_arrow_orienteering(), 
                         pad_x = unit(8, "cm"),           
                         pad_y = unit(0.5, "cm"))+
  ggtitle("Taxa de incidência por Covid-19 no Brasil, 2020") #Incluindo título

##EXPORTANDO O MAPA

mapa_final = ggplot()+ #salvando o mapa em um objeto
  geom_sf(data = br_shp_inc, 
          aes(fill = quartis), 
          color = "gray67", 
          size = 1)+ 
  scale_fill_brewer(palette="Purples")+
  guides(fill=guide_legend(title="Taxa de incidência"))+ 
  theme(panel.background = element_blank(), 
        axis.ticks = element_blank(),       
        axis.text = element_blank(),
        plot.title = element_text(size = 20, face = "bold"))+ #aumentando o título e colocando em negrito   
  annotation_scale()+
  annotation_north_arrow(style=north_arrow_orienteering(), 
                         pad_x = unit(21, "cm"),              #alterando local para se ajustar ao tamanho do título 
                         pad_y = unit(21, "cm"))+
  ggtitle("Taxa de incidência por Covid-19 no Brasil, 2020")

mapa_final

ggsave(filename = "mapa_tx_inc_covid.png", #exportando o último mapa plotado
       height = 10, width = 15, dpi = 320)
