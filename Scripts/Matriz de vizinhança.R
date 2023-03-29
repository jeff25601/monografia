
require(rgdal)
require(rgeos)
require(sp)
require(spdep)
library(FNN)

# Leitura do Shapefile contendo divisão territorial de Governador Valadares por Bairro
mapa <- st_read("dados\\Shapefile\\bairros.shp")

# Removendo linhas cujo bairro não está identificado
mapa <- mapa[which(!is.na(mapa$BAIRROS)),]

# Unindo geometrias de bairros que foram duplicados
mapa <- mapa %>% group_by(BAIRROS) %>%
  summarise(geometry = st_union(geometry))

# Removendo caracteres especiais dos nomes dos bairros
mapa$BAIRROS <- stringi::stri_trans_general(str = mapa$BAIRROS, id = "Latin-ASCII")

# Recuperando base de dados em que linhas não contém dados faltantes
df <- dado_select %>% na.omit()

# Filtrando nomes dos bairros em que indivíduos foram entrevistados
nomes <- mapa$BAIRROS[which((toupper(mapa$BAIRROS) %in% unique(toupper(df$q4))))]

mapa <- mapa[mapa$BAIRROS %in% nomes, ]


row.names(mapa) <- mapa$BAIRROS

# Criando matriz de vizinhança utilizando compartilhamento de fronteira como parâmetro
mapa <- poly2nb(mapa, queen = T)
mat <- nb2mat(mapa, style="B", zero.policy = T)

colnames(mat) <- rownames(mat)

linhas <- which(rowSums(mat) == 0)

# Alguns bairros não tiveram vizinhos encontrados por compartilhamento de froteira
# Esse erro pode ter ocorrido devido a simplificação dos poligonos das fronteiras
# Corrigimos esses casos manualmente

mat["ILHA DOS ARAUJOS", "CENTRO C"] <- 1
mat["CENTRO C", "ILHA DOS ARAUJOS"] <- 1

mat["CIDADE NOVA", "MORADA DO VALE"] <- 1
mat["MORADA DO VALE", "CIDADE NOVA"] <- 1

mat["TIRADENTES", "PALMEIRAS"] <- 1
mat["PALMEIRAS", "TIRADENTES"] <- 1

rm(mapa, linhas)
