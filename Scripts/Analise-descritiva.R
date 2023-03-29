setwd("c:/users/pedro/desktop/materias/2019-02/monografia/scripts")


source("dados/script_banco.R")
#retorna dado_select: colunas úteis para análise do banco original
#        dado_completo: todas as colunas do banco


library(naniar)
library(GGally)
library(tidyverse)
library(magrittr)
library(irtoys)
library(ltm)

set_construct = function(x) {
  if(x %in% c("EFpe", "EFpa", "UoP")) {
    return("Efetitividade")
  }else if(x %in% c("Cost", "Temp", "Esf", "CeH", "CoP")) {
    return("Custo de opt.")
  }else{
    return("AdI")
  }
}

set_construct = function(x) {
  if(x %in% 1:3) {
    return("efetitividade")
  }else if(x %in% 4:8) {
    return("custo de opt")
  }else{
    return("intencão")
  }
}


label_medida = (function(x){
  switch(x, '1' = "Kit", '2' = "Info", "3" = "List", "4" = "CoopF", "5" = "Sac", "6" = "Seg")
}) %>% Vectorize()

label_atributo = (function(x){
  switch(x, 
         '1' = "EFpe", '2' = "EFpa", "3" = "UoP", 
         "4" = "Cost", "5" = "Temp", "6" = "Esf", "7" = "CeH", "8" = "CoP", "9" = "AdI")
  
}) %>% Vectorize()


set_construct = Vectorize(set_construct)

#dimenção
dim(dado_select)


select = dplyr::select







#atributos
atributos = dado_select %>% 
  as_tibble() %>%
  select(q120_11:q120_69) %>% 
  gather(atributo_cond, nota) %>%
  mutate(atributo_cond = str_extract(atributo_cond, "..$")) %>% 
  mutate(medida = str_extract(atributo_cond,"^."), 
         atributo = str_extract(atributo_cond,".$")) %>% 
  select(-atributo_cond) %>% 
  mutate(contructo = set_construct(atributo)) %>% 
  mutate(medida = label_medida(medida) %>% factor(levels = label_medida(1:6)),
         atributo = label_atributo(atributo) %>% factor(levels = label_atributo(1:9)))



atributos
glimpse(atributos)
#observações
nrow(atributos)



#### valores faltantes ####
atributos %>% 
  group_by(medida, atributo) %>% 
  summarise(
    prop_na = mean(is.na(nota))
  ) %>%
  ggplot(aes(y=atributo, x = medida, fill = prop_na, 
             label = paste0(round(prop_na*100,2), "%"))) + 
  geom_tile(col = 'black') + 
  labs(fill = "Proporção NA's") + 
  geom_text(col = 'black') + 
  theme_minimal() + 
  scale_fill_gradient(low = "white", high = "grey50")


atributos %>% is.na() %>% sum()

#### descritivas atributos ####

# distribuição das respostas completas

prop = atributos %>%
  na.omit() %>%
  group_by(atributo, medida, nota) %>% 
  summarise(p = n()/nrow(dado_select %>% select(q120_11:q120_69) %>% na.omit()), n = n())


prop %>% 
  ggplot(aes(x=nota, y = p)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(y = p, label = round(p,2)), vjust=-0.5, size = 3) +
  facet_grid(medida ~ atributo) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, .25, .50, .75)) + 
  labs(x = "Grau", y = "Proporção") + 
  theme_bw() + 
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 15)) 


prop %>% 
  filter(medida == "Kit") %>%
  ggplot(aes(x=nota, y = p)) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(y = p, label = round(p,2)), vjust=-0.5, size = 3) + 
  facet_grid(.~atributo) + 
  theme_bw()




atributos %>% 
  na.omit() %>%
  ggplot(aes(x = nota)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size = 4) +
  facet_grid(rows = vars(medida), cols = vars(atributo)) + 
  scale_y_continuous(limits = c(0, 1000)) + 
  theme_minimal() 
  


#tabela com estatísticas
atributos %>% 
  group_by(medida, atributo) %>% 
  summarise(
    media = mean(nota, na.rm = T),
    mediana = median(nota, na.rm = T),
    moda = which.max(table(nota)),
    sd = sd(nota, na.rm = T)
  )



#visualizão estatísticas
atributos %>% 
  group_by(medida, atributo) %>% 
  summarise(
    media = mean(nota, na.rm = T),
    mediana = median(nota, na.rm = T),
    moda = which.max(table(nota)),
    sd = sd(nota, na.rm = T)
  ) %>% 
  gather(descricao, valor, -medida, -atributo) %>% 
  mutate(constructo = set_construct(atributo),
         descricao = factor(descricao, 
                            levels = c("media", "mediana", "moda", "sd"))) %>%
  #plot
  ggplot(aes(x=atributo, y = valor, label = round(valor,2))) + 
  geom_point() + 
  facet_grid(rows = vars(medida), cols = vars(descricao)) + 
  geom_text(vjust = 1.5, size = 3) + 
  labs(shape = "Constructo:") +
  scale_y_continuous(limits = c(0, 5)) + 
  scale_shape_manual(values = c(1, 16, 17)) + 
  theme_bw() +
  theme(legend.position = "top") 

#### correlação entre os atributos ####

#df com correlações
cor_mat = dado_select %>% 
  as_tibble() %>%
  select(q120_11:q120_69) %>% 
  cor(method = "spearman", use = "pairwise.complete.obs")


cor_mat_df = cor_mat %>% as.data.frame()
cor_mat_df$x = rownames(cor_mat_df)



cor_mat_df %<>% 
  gather(y, cor, -x) %>% 
  as_tibble() %>%
  mutate(x = str_extract(x, "..$")) %>% 
  mutate(medida_x = str_extract(x,"^."), 
         atributo_x = str_extract(x,".$")) %>% 
  select(-x) %>%
  mutate(y = str_extract(y, "..$")) %>% 
  mutate(medida_y = str_extract(y,"^."), 
         atributo_y = str_extract(y,".$")) %>% 
  select(-y) %>%
  select(medida_x, medida_y, atributo_x, atributo_y, cor)

#plot correlações dentro da mesma medida
cor_mat_df %>% 
  filter(medida_x == medida_y) %>%
  mutate(medida_x = label_medida(as.numeric(medida_x)) %>% 
           factor(levels = label_medida(1:6))) %>%
  mutate(atributo_x = label_atributo(atributo_x) %>% factor(levels = label_atributo(1:9)),
         atributo_y = label_atributo(atributo_y) %>% factor(levels = label_atributo(1:9))) %>%
  ggplot(aes(x = atributo_x, y = atributo_y, fill = cor, 
             label = round(cor,2))) + 
  geom_tile(col = 'white') + 
  facet_grid(cols = vars(medida_x)) + 
  coord_equal() + 
  geom_text(col = 'black', size = 2) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) + 
  scale_fill_gradient(low = "white", high = "grey50")

  


#independência entre as repostas em diferentes medidas protetivas


#variáveis binarizadas
dado_select_bin = dado_select %>% 
  select(q120_11:q120_69) %>%
  apply(FUN=function(x){ifelse(x>=4,1,0)}, 
        MARGIN = 2) %>% as.data.frame()


#condicionando distribuição
given = dado_select_bin %>%
  filter(q120_21 == 1, q120_22 == 1, q120_23 == 1) 

#distribuição das observações
x = table(dado_select_bin$q120_11, dado_select_bin$q120_12, dado_select_bin$q120_13) %>%
  as.data.frame() %>% 
  mutate(prob = Freq/nrow(dado_select_bin))

#distribuição condicional
xgiven = table(given$q120_11, given$q120_12, given$q120_13) %>%
  as.data.frame() %>%
  mutate(prob = Freq/nrow(given))

#tabelas
x
xgiven


#frequencia observada na distribuição condicional
observado = xgiven$Freq
#em caso de independência, espera-se que seja igual a distribuição não condicional
esperado = sum(xgiven$Freq)*x$prob

#valores
observado
esperado

#chisq est
sum(((observado-esperado)^2)/esperado)
#quantil
qchisq(0.95, df = 7)



#para o dado não binário
given = dado_select %>%
  filter(q120_21 == 5, q120_22 == 5, q120_23 == 5) 


x = table(dado_select$q120_11, dado_select$q120_12, dado_select$q120_13) %>%
  as.data.frame() %>% 
  mutate(prob = Freq/nrow(dado_select))


xgiven = table(given$q120_11, given$q120_12, given$q120_13) %>%
  as.data.frame() %>%
  mutate(prob = Freq/nrow(given))

x
xgiven

observado = xgiven$Freq
esperado = sum(xgiven$Freq)*x$prob + 0.0001

observado
esperado


chisq = sum(((observado-esperado)^2)/esperado)
chisq
qchisq(0.95, df = length(observado)-1)





pca_scores = function(x, p) {
  dec = x %>% 
    as.matrix() %>%
    cor(method = "spearman", use = "pairwise.complete.obs") %>% 
    eigen()
  
  m = as.matrix(x)%*%dec$vectors
  return(m[,1:p])
  
}






#### variáveis individuais ####
features = dado_select %>% 
  select(sexo:area_risco, 
         q120_19, q120_29, q120_39, q120_49, q120_59, q120_69) %>% 
  as_tibble()


resp_string = paste0(str_c("q120_", 1:6), 9)


gfeatures = features %>% 
  select(-(q120_19:q120_69)) %>%
  gather(variavel, valor) %>% 
  as_tibble()


gfeatures %>% 
  group_by(variavel, valor) %>% 
  summarise(n = n()) %>% 
  as.data.frame()

#sexo 
features %>% 
  na.omit() %>%
  select(c("sexo", resp_string)) %>%
  gather(resposta, valor, -sexo) %>%
  mutate(sexo = ifelse(sexo == 1, "Masc", "Fem"),
         valor = factor(valor),
         resposta = str_sub(resposta, 6, 6) %>% label_medida() %>% 
           factor(levels = label_medida(1:6))) %>% 
  #filter(resposta == "q120_19") %>% 
  ggplot(aes(x=sexo, fill = valor)) + 
  geom_bar(position = 'fill', col = 'black') + 
  facet_grid(cols = vars(resposta)) + 
  scale_fill_brewer(palette = 6) + 
  labs(x="Sexo", y = "Proporção", fill = "Intenção") + 
  theme_minimal()


#renda
features %>% 
  na.omit() %>%
  select(c("renda", resp_string)) %>%
  gather(resposta, valor, -renda) %>%
  mutate(renda = factor(renda),
         valor = factor(valor),
         resposta = str_sub(resposta, 6, 6) %>% label_medida() %>% 
           factor(levels = label_medida(1:6))) %>% 
  #filter(resposta == "q120_19") %>% 
  ggplot(aes(x=renda, fill = valor)) + 
  geom_bar(position = 'fill', col = 'black') + 
  facet_grid(cols = vars(resposta)) + 
  scale_fill_brewer(palette = 6) + 
  labs(x="Nível de renda", y = "Proporção", fill = "Intenção") + 
  theme_minimal()

#renda casa
features %>% 
  na.omit() %>%
  select(c("renda_casa", resp_string)) %>%
  gather(resposta, valor, -renda_casa) %>%
  mutate(renda_casa = factor(renda_casa),
         valor = factor(valor),
         resposta = str_sub(resposta, 6, 6) %>% label_medida() %>% 
           factor(levels = label_medida(1:6))) %>% 
  #filter(resposta == "q120_19") %>% 
  ggplot(aes(x=renda_casa, fill = valor)) + 
  geom_bar(position = 'fill', col = 'black') + 
  facet_grid(cols = vars(resposta)) +
  labs(x="Nível de renda", y = "Proporção", fill = "Intenção") + 
  scale_fill_brewer(palette = 6) + 
  theme_minimal()


#renda casa agregada
features %>% 
  na.omit() %>%
  select(c("renda_casa", resp_string)) %>%
  gather(resposta, valor, -renda_casa) %>%
  mutate(renda_casa = renda_casa %>% cut(c(-Inf, 5, 7, Inf),
                                         label = c("Baixa", "Média", "Alta")),
         valor = factor(valor),
         resposta = str_sub(resposta, 6, 6) %>% label_medida() %>% 
           factor(levels = label_medida(1:6))) %>% 
  #filter(resposta == "q120_19") %>% 
  ggplot(aes(x=renda_casa, fill = valor)) + 
  geom_bar(position = 'fill', col = 'black') + 
  facet_grid(cols = vars(resposta)) + 
  labs(x = "Nível renda", y = "Proporção", fill = "AdI") + 
  scale_fill_brewer(palette = 6) + 
  theme_minimal()




#área risco
features %>% 
  na.omit() %>%
  select(c("area_risco", resp_string)) %>%
  gather(resposta, valor, -area_risco) %>%
  mutate(area_risco = factor(ifelse(area_risco==1, "Sim", "Não")),
         valor = factor(valor),
         resposta = str_sub(resposta, 6, 6) %>% label_medida() %>% 
           factor(levels = label_medida(1:6))) %>% 
  #filter(resposta == "q120_19") %>% 
  ggplot(aes(x=area_risco, fill = valor)) + 
  geom_bar(position = 'fill', col = 'black') + 
  facet_grid(cols = vars(resposta)) + 
  scale_fill_brewer(palette = 6) + 
  labs(x="Residência em área de risco", y = "Proporção", fill = "Intenção") +
  theme_minimal()

#área risco segmentado por renda_casa
features %>% 
  na.omit() %>%
  select(c("area_risco", "renda_casa", resp_string)) %>%
  gather(resposta, valor, -area_risco, -renda_casa) %>%
  mutate(area_risco = factor(ifelse(area_risco==1, "Sim", "Não")),
         renda_casa = renda_casa %>% 
           cut(c(-Inf, 5, 7, Inf),
                label = paste("Renda", c("Baixa", "Média", "Alta"))),
         valor = factor(valor),
         resposta = str_sub(resposta, 6, 6) %>% label_medida() %>% 
           factor(levels = label_medida(1:6))) %>% 
  #filter(resposta == "q120_19") %>% 
  ggplot(aes(x=area_risco, fill = valor)) + 
  geom_bar(position = 'fill', col = 'black') + 
  facet_grid(cols = vars(resposta), rows = vars(renda_casa)) + 
  scale_fill_brewer(palette = 6) + 
  labs(x="Residência em área de risco", y = "Proporção", fill = "Intenção") +
  theme_minimal()



#percepção de risco

percepcao = dado_select %>% select(q119_1:q119_4) %>% rowMeans() %>% 
  `*`(., dado_select$chance_nova_ocorrencia) 

dfpercp = data.frame(percepcao) %>% bind_cols(dado_select %>% select(resp_string))

dfpercp %>% 
  na.omit() %>%
  gather(medida, intencao, -percepcao) %>% 
  as_tibble() %>% 
  mutate(medida = str_sub(medida, 6,6) %>% label_medida() %>% 
           factor(levels = label_medida(1:6))) %>%
  ggplot(aes(x=factor(intencao), y = percepcao)) + 
  geom_boxplot() + 
  facet_grid(.~medida) + 
  labs(x="Intenção", y = "Percepção de risco") + 
  theme_minimal()


























  
















