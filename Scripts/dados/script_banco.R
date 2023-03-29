library(tidyverse)

#lendo banco de dados
dado_completo = data.table::fread("dados/att_banco.csv")

dado_completo$q4[which(dado_completo$q4 == "Centro")] <- c("Centro B", "Centro A", "Centro A", "Centro A", "Centro A", "Centro B", "Centro B", "Centro C", "Centro C")

table(dado_completo$q4)
#colunas úteis para a análise
dado_select = dado_completo %>% 
  select(
    cab3,
    q4,
    q120_11:q120_69, 
    chance_nova_ocorrencia = q118, q119_1:q119_4,
    q120_11:q120_69,
    sexo = q13, renda = q21, renda_casa = q22, area_risco = dalag160,
    maior_intrucao = q42,
    n_enchentes = q100
  ) %>% 
  mutate(renda = ifelse(renda == 9, NA, renda),
         renda_casa = ifelse(renda == 9, NA, renda),
         maior_instrucao =  cut(maior_intrucao, c(-Inf, 2, 4, Inf), labels = c("baixa", "media", "alta")),
         q4 = stringi::stri_trans_general(str = q4, id = "Latin-ASCII")
         ) %>%
  na.omit() %>%
  arrange(q4) %>%
  mutate(q4 = case_when(
    q4 == "JK1" ~ "JK 1",
    q4 == "JK2" ~ "JK 2",
    q4 == "JK3" ~ "JK 3",
    q4 == "Nossa Senhora das Gracas" ~ "N S DAS GRACAS",
    q4 == "Nossa Senhora de Fatima" ~ "NOSSA S DE FATIMA",
    q4 == "Vera Cruz" ~ "Jardim Vera Cruz",
    q4 == "Vila P. Ibituruna" ~ "VILA P IBITURUNA",
    q4 == "Vila P. Sao joao" ~ "VILA  P SAO JOAO",
    TRUE ~ q4
  ))

dado_select <- dado_select %>%
  mutate(
    viz = group_indices(., q4))





write.csv2(dado_select, "dados.csv")

length(unique(dado_completo$cab3)) 
length(dado_completo$cab3)




