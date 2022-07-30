#AULA 2 - MAPAS TEMÁTICOS: PARTE I

#INSTALANDO OS PACOTES UTILIZADOS NA AULA

##INSTALANDO PACOTES DIRETAMENTE DO CRAN 
install.packages("devtools", dependencies = T) #ferramentas de desenvolvedor do R
install.packages("geobr", dependencies = T) #pacote de geoanálise de arquivos do Brasil
install.packages("pacman", dependencies = T) #pacote para instalação e carregamento de outros pacotes)

##INSTALANDO PACOTE GEOBR DIRETAMENTE DO GITHUB 
devtools::install_github("ipeaGIT/geobr", subdir = "r-package", force = T) #instalando geobr do GitHub, caso a instalação do CRAN não tenha funcionado

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
  tidyverse        #conjunto de pacotes de manipulação e tratamento de dados e criação de gráficos
  )

#IMPORTANDO OBJETOS ESPACIAIS

##VERIFICANDO FUNÇÕES DISPONÍVEIS NO PACOTE GEOBR
funcoes_geobr = list_geobr()
View(funcoes_geobr)

##IMPORTANDO DADOS DOS ESTADOS BRASILEIROS
brasil = read_state(code_state = "all", year = 2018)

##PLOTANDO OS DADOS COM O PACOTE GGPLOT2
ggplot() +
  geom_sf(data= brasil)

##CARREGANDO E PLOTANDO DADOS DAS MICRORREGIÕES NO ANO DE 2015
micro_reg = read_micro_region(code_micro = "all", year = 2015)

ggplot() +
  geom_sf(data= micro_reg, 
          fill = "blue", 
          color = "grey", 
          show.legend = T)

##CARREGANDO E PLOTANDO MAPA DO ESTADO DE SERGIPE
sergipe = read_state(code_state="SE", year=2018)

ggplot() +
  geom_sf(data= sergipe, 
          fill = "red", 
          color = "black", 
          alpha = 0.5)

##CARREGANDO E PLOTANDO MAPA DOS MUNICÍPIOS DO ESTADO DE ALAGOAS
munic_al = read_municipality(code_muni= "AL", year=2007)

ggplot() +
  geom_sf(data= munic_al, 
          fill = "darkgreen", 
          color = "white",
          alpha = 0.4)

#ELABORANDO MAPA TEMÁTICO

##IMPORTANDO DADOS DE SARS POR COVID-19 EM 2020
sivep2020 = import("dados/sivep_gripe_2020.csv")

sivep2020_casos = sivep2020 |>     #criando um data.frame que recebe dados do original
  select(CLASSI_FIN, SG_UF_NOT) |> #selecionando as variáveis de interesse
  filter(CLASSI_FIN == 5) |>       #selecionando apenas dados de Covid-19
  select(SG_UF_NOT) |>             #selecionando apenas dados de UF
  group_by(SG_UF_NOT) |>           #agrupando dados por UF
  summarise(CASOS = n())           #contando número de casos por UF

##SALVANDO O ARQUIVO EM FORMATO .RDS
export(sivep2020_casos, "dados/sivep2020_casos.RDS")

##IMPORTANDO DADOS DE POPULACAO E DE CASOS
casos_uf = import("dados/sivep2020_casos.RDS")
populacao_uf = import("dados/populacao_uf_2020.csv")

##UNINDO DATA.FRAMES DE CASOS E DE POPULAÇÕES
incid_uf = casos_uf |>
  left_join(populacao_uf, by = c("SG_UF_NOT" = "sigla"))

##CALCULANDO TAXA DE INCIDÊNCIA
incid_uf$taxa_incidencia = round(incid_uf$CASOS/incid_uf$pop*100000,2)

##SALVANDO O ARQUIVO EM FORMATO .RDS
export(sivep2020_casos, "dados/sivep2020_casos.RDS")

##SELECIONANDO APENAS VARIÁVEIS DE INTERESSE
incid_uf = incid_uf |>
  select(SG_UF_NOT, taxa_incidencia)

##UNINDO À TABELA DE ATRIBUTOS DO OBJETO "brasil"
brasil_covid = brasil %>%
  left_join(incid_uf, by = c("abbrev_state" = "SG_UF_NOT"))

##CRIANDO UM MAPA TEMÁTICO
ggplot(data = brasil_covid, aes(fill = taxa_incidencia)) +
  geom_sf(color = "white", #cor da borda
          size = .15,      #tamanho da borda
          show.legend = F, #não mostrar legendas
          alpha = 0.7) +   #transparência 
  geom_sf_label(aes(label=taxa_incidencia),    #escolhendo variável para os rótulos
                label.padding = unit(2, "mm"), size = 2) #tamanho dos rótulos
