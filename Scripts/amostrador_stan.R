setwd("Scripts")
rm(list = ls())
library(tidyverse)
library(truncnorm)
library(rstan)


source("dados/script_banco.R")
#retorna dado_select: colunas úteis para análise do banco original
#        dado_completo: todas as colunas do banco

options(mc.cores = parallel::detectCores())

rstan_options(auto_write = TRUE)

source("Matriz de vizinhança.R")

# Criando matriz que identifica a qual bairro "k" o indivíduo "i" pertence
xik <- df %>% mutate(um = 1) %>% 
  pivot_wider(names_from = q4, 
              values_from = um, 
              values_fill = list(um = 0)) %>%
  select(Altinopolis:`Vila Rica`) %>% 
  as.matrix()

#resposta binária

y = (df %>% select(str_c("q120_", 1:6) %>% str_c(9)) %>% as.matrix())

y <- ifelse(y >= 4, 1, 0)



#variáveis individuais
percep = df %>% select_at(vars(matches("q119"))) %>% rowMeans()
percep = percep*df$chance_nova_ocorrencia
sexo = ifelse(df$sexo == 1, 1, 0) 
renda = df$renda_casa %>% cut(c(-Inf, 4, 6, Inf), 
                              labels = c("baixa", "media", "alta"))
renda = model.matrix(lm(df$q120_11 ~ renda + df$q120_13)) %>% as.matrix()
renda = renda[,c(-1, -4)]


z = cbind(percep, sexo, renda)


#efetividade
xefe_bin = list()
for(i in 1:6) {
  xefe_bin[[i]] = df %>% select(str_c("q120_", i) %>% str_c(1:3)) %>% 
    apply(MARGIN = 2, FUN = function(x) ifelse(x>=4, 1, 0))
}

#custo
xcost_bin = list()
for(i in 1:6) {
  xcost_bin[[i]] = df %>% select(str_c("q120_", i) %>% str_c(4:8)) %>% 
    apply(MARGIN = 2, FUN = function(x) ifelse(x>=4, 1, 0))
}


xbin = xefe_bin %>% unlist() %>% array(dim = c(1146, 3, 6))

#Apenas Seguros
xbin = xbin[, , 6]

wbin = xcost_bin %>% unlist() %>% array(dim = c(1146, 5, 6))

#Apenas Seguros
wbin = wbin[, , 6]

# Dados de Entrada para o Stan
datamodel_bin = list(
  K = 1,
  N = nrow(xbin),
  M = 6,
  Jc = ncol(wbin),
  Je = ncol(xbin),
  w = wbin,
  x = xbin,
  L = dim(xik)[2],
  I = diag(rep(1, 6)),
  Um0 = matrix(1, 6, 6) - diag(rep(1, 6)),
  mu = rep(0, 6),
  muviz = rep(0, 85),
  viz = diag(rowSums(mat)),
  vizmat = mat,
  nk = colSums(xik),
  y = y,
  z = z,
  zl = ncol(z)
)

# Leitura do arquivo Stan contendo o modelo a ser utilizado
model = stan_model(file = "scripts_stan/modelo_para_uma_medida_isolada.stan",
                   model_name = "main_model", save_dso = T, auto_write = T)

final_model = stan_model(file = "scripts_stan/modelo_final.stan",
                   model_name = "main_final_model", save_dso = T, auto_write = T)

# Valores iniciais de alguns parâmetros para duas cadeias distintas
inits = list(
  list(
    "a_c" = matrix(runif(6*5, 0.5, 1), 6, 5),
    "theta" = matrix(0.5, 1146, 6),
    "lambda" = matrix(0.5, 1146, 6),
    "b_l" = rep(3,6),
    "b_t" = rep(4,6)
  ),
  list(
    "a_c" = matrix(runif(6*5, 0.5, 1), 6, 5),
    "theta" = matrix(-0.5, 1146, 6),
    "lambda" = matrix(-0.5, 1146, 6),
    "b_l" = rep(-2,6),
    "b_t" = rep(-4,6)
  )
)

# Execução do modelo
amostra <- sampling(model, data = datamodel_bin, chains = 2, iter = 200000, init = inits, warmup = 100000,
                    pars = c("b_l", "b_t", "bz", "gamma", "ypred", "logy"))

amostra <- sampling(final_model, data = datamodel_bin, chains = 1, iter = 10, init = inits, warmup = 5,
                    pars = c("b_l", "b_t", "bz", "gamma", "ypred"))
  

saveRDS(amostra, "C:\\Users\\jeees\\Desktop\\Amostras Finais - Monografia\\amostra_seguros.RDS")


rm(list = ls())
