rm(list = ls())

# Leitura de Pacotes e amostras das distribuições a posteriori ----
library(tidyverse)
library(rstan)
library(coda)
require(rgdal)
library(stringi)
library(sf)
library(tigris)


amostra <- readRDS("C:\\Users\\jeees\\Desktop\\Amostras Finais - Monografia\\amostra_seguros.RDS")

# Preparação dos resultados ----

posterior_summary <- summary(amostra, pars = "gamma", probs = c(0.025, 0.5, 0.975))$summary

source("dados/script_banco.R")
posterior_summary <- data.frame(Bairro = toupper(unique(dado_select$q4)), 
                                Media = posterior_summary[, 1],
                                LI = posterior_summary[, 4],
                                LS = posterior_summary[, 6]) %>%
  mutate(Significative_values = case_when(
    LI > 0 | LS < 0 ~ Media,
    TRUE ~ NA_real_))
  

mapa <- st_read("dados\\Shapefile\\bairros.shp")

mapa$BAIRROS <- stri_trans_general(str = mapa$BAIRROS, id = "Latin-ASCII")

mapa <- mapa %>% 
  group_by(BAIRROS) %>%
  summarise(geometry = st_union(geometry))


posterior_summary$Bairro <- stri_trans_general(str = posterior_summary$Bairro, id = "Latin-ASCII")

posterior_summary <- posterior_summary %>% 
  mutate(Efeito = case_when(
    is.na(Significative_values) ~ "Não Significativo",
    Significative_values > 0 ~ "Positivo",
    Significative_values < 0 ~ "Negativo"
  ))

posterior <- geo_join(mapa, posterior_summary, by_sp = "BAIRROS", by_df = "Bairro", how = "inner")

# Gráficos ----

rstan::traceplot(amostra, pars = c("b_l", "b_t"), inc_warmup = T)

rstan::traceplot(amostra, pars = c("bz[1]", "bz[2]"), inc_warmup = T)

rstan::traceplot(amostra, pars = c("bz[3]", "bz[4]"), inc_warmup = T)

ICM <- ggplot(posterior, aes(color = !is.na(Significative_values))) + 
  geom_point(aes(x = Media, y = BAIRROS)) +
  geom_errorbarh(aes(xmin = LI, xmax = LS, y = BAIRROS)) + 
  scale_color_manual(values = c("Gray70", "Black")) + 
  geom_vline(xintercept = 0, color = "red") + 
  labs(x = "Média", y = "Bairros") + 
  theme_minimal() + 
  theme(axis.text.y = element_text(size = 4),
        legend.position = "none")

mapa_efeitos_completo <- ggplot(posterior) +
  geom_sf(aes(fill = Media)) +
  theme_classic() + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, name = "Media") + 
  theme(legend.position = "bottom")

mapa_efeitos_significativos <- ggplot(posterior) +
  geom_sf(aes(fill = Significative_values)) +
  theme_classic() + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, name = "Media") + 
  theme(legend.position = "bottom")


ggpubr::ggarrange(mapa_efeitos_completo, ICM, mapa_efeitos_significativos, nrow = 1)

mapa_efeitos_bin <- ggplot(posterior %>% mutate(Efeito_bin = ifelse(Media > 0, "Positivo", "Negativo"))) +
  geom_sf(aes(fill = Efeito_bin)) + 
  theme_classic() +
  scale_fill_manual(values = c("Blue", "Red"), name = "Media") + 
  theme(legend.position = "bottom")

mapa_efeitos_bin_significativos <- ggplot(posterior) + 
  geom_sf(aes(fill = Efeito)) + 
  theme_classic() +
  scale_fill_manual(values = c("Gray50", "Blue", "Red"), name = "Media") + 
  theme(legend.position = "bottom")

ggpubr::ggarrange(mapa_efeitos_bin, ICM, mapa_efeitos_bin_significativos, nrow = 1)

Amostra <- As.mcmc.list(amostra, pars = c("b_l", "b_t", "bz", "gamma"), include = T)


# Diagnóstico de convergência das amostras das distribuições a posteriori
gelman_teste <- gelman.diag(Amostra) # Pelo teste de gelman

traceplot(amostra) # Por uma análise gráfica de convergência de múltiplas cadeias

# Diagnóstico de autocorrelação para as amostras das distribuições a posteriori de cada uma das cadeias
autocorr.plot(Amostra, auto.layout = F, ask = F)
par(mfrow = c(1, 1))


posterior_summary <- summary(amostra, pars = c("b_l", "b_t", "bz"), probs = c(0.025, 0.5, 0.975))

posterior_summary <- data.frame(Media = posterior_summary$summary[, 1],
                                LI = posterior_summary$summary[, 4],
                                LS = posterior_summary$summary[, 6])
posterior_summary

summary(amostra, pars = c("y_pred"))
