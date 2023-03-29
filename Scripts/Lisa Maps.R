rm(list = ls())
library(tidyverse)
library(shiny)
require(rgdal)
library(magrittr)
library(stringi)
library(sf)
library(tigris)
library(spdep)
library(sfweight)
library(rgeoda)


mapa <- st_read("C:\\Users\\jeees\\Documents\\Shapefile\\bairros.shp")  
dados <- read.csv("C:\\Users\\jeees\\Documents\\GV - Medidas Protetivas\\dados.csv", sep = ';')


mapa$BAIRROS <- stri_trans_general(str = mapa$BAIRROS, id = "Latin-ASCII")
dados$q4 <- stri_trans_general(str = dados$q4, id = "Latin-ASCII")

mapa <- mapa %>% 
  group_by(BAIRROS) %>%
  summarise(geometry = st_union(geometry))

dados <- dados %>%
  mutate(q4 = case_when(
    q4 == "JK1" ~ "JK 1",
    q4 == "JK2" ~ "JK 2",
    q4 == "JK3" ~ "JK 3",
    q4 == "Vila P. Ibituruna" ~ "Vila P Ibituruna",
    q4 == "Vila P. Sao joao" ~ "Vila  P Sao Joao",
    q4 == "Nossa Senhora das Gracas" ~ "N S DAS GRACAS",
    q4 == "Nossa Senhora de Fatima" ~ "Nossa S de Fatima",
    TRUE ~ q4
  ),
  percep = (dados %>% select(starts_with("q119")) %>% rowMeans(na.rm = T))*chance_nova_ocorrencia,
  renda_casa = case_when(
    renda_casa == 8 ~ 1,
    renda_casa %in% 1:7 ~ 0,
    TRUE ~ NA_real_
  ),
  across(.cols = starts_with("q120_"),
         .fns = ~ case_when(
           .x %in% 4:5 ~ 1,
           .x %in% 1:3 ~ 0,
           TRUE ~ NA_real_
         )),
  sexo = case_when(
    sexo == 1 ~ 1,
    sexo == 2 ~ 0,
    TRUE ~ NA_real_
  )) 

teste <- dados %>%
  mutate(q4 = str_to_upper(q4)) %>% 
  group_by(q4) %>% 
  summarise(across(.cols = where(is.numeric),
                   .fns = ~mean(.x, na.rm = T)))

teste <- geo_join(mapa, teste, by_sp = "BAIRROS", by_df = "q4", how = "inner")

gen_map <- function(dados, var){
  
  if(!var %in% c("renda", "renda_casa", "sexo", "area_risco")){
    
    var <- switch (var,
                   "Kit" = "1",
                   "Info" = "2",
                   "List" = "3",
                   "Plan" = "4",
                   "Sand" = "5",
                   "Insur" = "6"
    )
    
    var <- paste0("q120_", var, "9")
    
  }
  
  
  
  # Elimina do shapefile os casos faltantes para a variável de interesse
  gv_lisa <- dados %>% select({{var}}, geometry) %>% na.omit()
  
  # Converte o shape em formato sf (polígono)
  # pois há perda do formato shapefile após o uso da filtragem
  gv_lisa <- st_as_sf(gv_lisa)
  
  #queen_w = queen_weights(gv_lisa)
  
  #queen_w$SetNeighbors(idx = 24, nbrs =  12)
  #queen_w$SetNeighbors(idx = 15, nbrs =  43)
  #queen_w$SetNeighbors(idx = 22, nbrs =  30)

  #queen_w$Update(updateStats = T)
  
  #lisa <- local_moran(queen_w, dados[var])
  
  
  #dados$cluster <- as.factor(lisa$GetClusterIndicators())
  #levels(dados$cluster) <- lisa$GetLabels()
  #dados$cluster <- forcats::fct_relevel(dados$cluster, c("High-High", "High-Low", "Low-High", "Low-Low", "Not significant"))
  
  # Define as variáveis de vizinhança, peso espacial, lag espacial e estatística
  # lisa para autocorrelação (Alto-Alto, Baixo-Baixo, Alto-Baixo e Baixo-Alto)
  gv_lisa %<>%
    mutate(nb = st_neighbors(geometry))
  
  gv_lisa$nb[24] <- list(as.integer(12))
  gv_lisa$nb[15] <- list(as.integer(43))
  gv_lisa$nb[22] <- list(as.integer(30))
  
  gv_lisa %<>% 
    mutate(wts = st_weights(nb),
           lag = st_lag(.data[[var]], nb, wts, allow_zero = T),
           lisa = factor(categorize_lisa(.data[[var]], lag), levels = c("HH", "HL", "LH", "LL"), 
                         ordered = T)) %>%
    mutate(lisa = case_when(
      lisa == "HH" ~ "High-High",
      lisa == "HL" ~ "High-Low",
      lisa == "LH" ~ "Low-High",
      lisa == "LL" ~ "Low-Low"
    ))
  
  # Visualiza os resultados graficamente
  return(ggplot(data = dados) +
    geom_sf(aes(fill = cluster)) +
    #geom_sf(data=dados[is.na(dados[[var]]),],fill="gray") +
    #scale_fill_gradient2(name = "LISA") +
    scale_fill_manual(name="LISA", values = c("gray", "red", "blue", "lightblue", "lightpink1")) +
    theme_classic() + 
    scale_x_continuous(breaks = seq(-42, -41.9, length.out = 4), 
                       labels = c("42.00ºW", "41.96ºW", "41.93ºW", "42.90ºW")))
  
}

gen_map(teste, "Insur")
gen_map(teste, "Kit")
gen_map(teste, "Plan")
gen_map(teste, "Info")
gen_map(teste, "List")
gen_map(teste, "Sand")
gen_map(teste, "renda")
gen_map(teste, "renda_casa")
gen_map(teste, "sexo")
gen_map(teste, "area_risco")
## Como interpretar: 

## Bairros de categoria HH possuem valores altos para a variável de interesse assim como seus
## vizinhos, o mesmo se aplica para as demais categorias

## HH = High-High
## HL = High-Low
## LH = Low-High
## LL = Low-Low

ggarrange(gen_map(teste, "Kit"), gen_map(teste, "Info"), 
          gen_map(teste, "List"), gen_map(teste, "Plan"), 
          gen_map(teste, "Insur"), gen_map(teste, "Sand"), 
          common.legend = T, legend = "bottom", 
          labels = c("Kit de emergência", "Buscar informação",
                     "Lista do que fazer", "Planejar com vizinhos",
                     "Seguro", "Sacos de areia"), 
          font.label = list(size = 12, face = "plain", color = "black"),
          ncol = 3, nrow = 2)
          
ggsave("Lisa Maps.pdf", dpi = 500)




df = bind_rows( kit_m %>% rename(var = q120_19), 
               info_m %>% rename(var = q120_29), 
               list_m %>% rename(var = q120_39), 
               plan_m %>% rename(var = q120_49), 
              insur_m %>% rename(var = q120_69), 
               sand_m %>% rename(var = q120_59))

df$Medida = rep(c("Kit de emergência", "Buscar informação",
                  "Lista do que fazer", "Planejar com vizinhos",
                  "Seguro", "Sacos de areia"), each = 87)

ggplot(data = df) +
  geom_sf(aes(fill = lisa)) +
  scale_fill_manual(name="LISA", values = c("red", "lightpink1", "lightblue", "blue")) +
  theme_minimal() + 
  scale_x_continuous(breaks = c(-42, -41.95, -41.9)) + 
  facet_wrap(~Medida, ) + 
  theme(legend.position = "bottom", 
        panel.spacing = unit(2, "lines"))

ggsave("Lisa Maps.pdf", dpi = 500)



df_s <- bind_rows(kit, info, list, plan, insur, sand)

df_s$Medida = rep(c("Kit de emergência", "Buscar informação",
                  "Lista do que fazer", "Planejar com vizinhos",
                  "Seguro", "Sacos de areia"), each = 87)

ggplot(data = df_s) +
  geom_sf(aes(fill = cluster)) +
  scale_fill_manual(name="LISA", values = c("red", "lightpink1", "lightblue", "blue", "grey")) +
  theme_minimal() + 
  scale_x_continuous(breaks = c(-42, -41.95, -41.9)) + 
  facet_wrap(~Medida) + 
  theme(legend.position = "bottom", 
        panel.spacing = unit(2, "lines"))

ggsave("Lisa Significance Maps.png", dpi = 500)
