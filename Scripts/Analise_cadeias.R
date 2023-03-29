rm(list = ls())

# Leitura de Pacotes e amostras das distribuições a posteriori
library(tidyverse)
library(rstan)
library(coda)


amostra <- readRDS("amostras/amostra_seguros.RDS")

amostra <- As.mcmc.list(amostra, pars = c("b_l", "b_t"), include = T)

# Diagnóstico de convergência das amostras das distribuições a posteriori
gelman_teste <- gelman.diag(amostra) # Pelo teste de gelman

traceplot(as.mcmc.list(amostra)) # Por uma análise gráfica de convergência de múltiplas cadeias

# Diagnóstico de autocorrelação para as amostras das distribuições a posteriori de cada uma das cadeias
autocorr.plot()

