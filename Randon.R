
# INFORMACOES ----
# CODIGO DESENVOLVIDO EM R

# TESTE PARA VAGA DE CIENTISTA DE DADOS NA RANDON
# NOME: BRENNER BIASI SOUZA SILVA

# SOFTWARE
# R version 3.6.2 (2019-12-12)
# PLATAFORMA - x86_64-apple-darwin15.6.0
# macOS MOJAVE
# RSTUDIO Version 1.2.5033


# ADEQUANDO O AMBIENTE DE TRABALHO ----
# ATENCAO: TODO O ENVIRONMENT SERA PREVIAMENTE EXCLUIDO.
rm(list = ls()); gc()

# DATASET ----
# IMPORT DATASET
df <- read.csv("precos_imoveis.csv", header = TRUE)

# PACOTES REQUERIDOS ----
{
  if(!require("pacman")) install.packages("pacman")
  
  pacman::p_load(dplyr, tidyr, naniar, ggplot2, lubridate, stringr, 
                 dummies, missForest, tibble, quantmod, caret, Metrics, 
                 reshape2, spacetime, ggmap, cowplot, WVPlots, purrr, 
                 caretEnsemble, clustMixType, plotly, xgboost, glmnet, randomForest)
  }

# ANALISE E AJUSTE DO DATASET ----
#ESTRUTURA
dplyr::glimpse(df)

# NA'S
naniar::gg_miss_var(df, show_pct = TRUE) +
  labs(x = "Variáveis", y = "% NA")

# SELECAO INCIAL, TRANSFORMACAO E AJUSTE DE VARIAVEIS
dfx <- df %>% 
  dplyr::mutate(id = 1:nrow(df), 
                Data = lubridate::parse_date_time(Data, '%d-%m-%Y'),
                Data = as.Date(Data),
                Endereco = stringr::str_to_lower(Endereco),
                Endereco = stringr::str_trim(Endereco),
                Endereco = stringr::str_replace_all(Endereco, "[:digit:]", ""),
                Endereco = gsub(" *\\b[[:alpha:]]{1}\\b *", "", Endereco),
                Endereco = gsub("[[:punct:]]", "", Endereco),
                Endereco = stringr::str_trim(Endereco),
                Endereco = as.factor(Endereco),
                Tipo = as.factor(Tipo),
                Metodo = as.factor(Metodo),
                Latitude = Latitudo,
                Regiao = as.factor(Regiao),
                Distrito = as.factor(Distrito),
                NumImoveis = as.factor(NumImoveis),
                Distancia = as.factor(Distancia)) %>%
  dplyr::filter(Regiao   != "#N/A",
                Distrito != "#N/A",
                NumImoveis != "#N/A", 
                Distancia  != "#N/A") %>% # REMOVENDO ABERRACAO
  dplyr::mutate(NumImoveis  = as.numeric(as.character(NumImoveis)),
                Distancia   = as.numeric(as.character(Distancia)),
                Latitude    = as.numeric(as.character(Latitude)),
                Longitude   = as.numeric(as.character(Longitude))) %>%  
  dplyr::select(-Latitudo) %>% 
  tidyr::drop_na(Preco) 

# EDA ----
# DENSIDADE
p <- ggplot(dfx, aes(x = Preco)) + 
  geom_density(fill = "blue", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(Preco)),
              color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("Preço") +
  theme_bw()

p_log <- ggplot(dfx, aes(x = log10(Preco))) + 
  geom_density(fill = "blue", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(log10(Preco))),
             color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("log10(Preço)") +
  theme_bw()
cowplot::plot_grid(p, p_log, align = 'hv', nrow = 1)

# PRECO VERSUS REGIAO
dfx %>% 
  ggplot() +
  geom_violin(aes(x = Regiao, y = log10(Preco)), 
              fill = "blue", alpha = 0.2, draw_quantiles = 0.5) +
  coord_flip() +
  theme_bw()

# PRECO VERSUS DISTRITO
dfx %>% 
  ggplot() +
  geom_violin(aes(x = Distrito, y = log10(Preco)), 
              fill = "blue", alpha = 0.2, draw_quantiles = 0.5) +
  coord_flip() +
  theme_bw()

# PRECO VERSUS DISTANCIA, METODO
p2 <- dfx %>% 
  ggplot() +
  geom_point(aes( x = log10(Preco), y = (Distancia)),
             col = "grey", alpha = 0.4) +
  geom_smooth(aes(x = log10(Preco), y = (Distancia))) +
  facet_wrap(~Metodo, nrow = 1) +
  theme_bw()

p3 <- dfx %>% 
  ggplot() +
  geom_point(aes( x = log10(Preco), y = (Distancia)),
             col = "grey", alpha = 0.4) +
  geom_smooth(aes(x = log10(Preco), y = (Distancia))) +
  facet_wrap(~Metodo, scale = "free", nrow = 1) +
  theme_bw()
cowplot::plot_grid(p2, p3, align = 'hv', nrow = 2)

# PRECO VERSUS DISTANCIA, REGIAO
p4 <- dfx %>% 
  ggplot() +
  geom_point(aes( x = log10(Preco), y = log10(Distancia)),
             fill = "blue", alpha = 0.2) +
  geom_smooth(aes(x = log10(Preco), y = log10(Distancia))) +
  facet_wrap(~Regiao) +
  theme_bw()
p5 <- dfx %>% 
  ggplot() +
  geom_point(aes( x = log10(Preco), y = (Distancia)),
             fill = "blue", alpha = 0.2) +
  geom_smooth(aes(x = log10(Preco), y = (Distancia))) +
  facet_wrap(~Regiao) +
  theme_bw()
cowplot::plot_grid(p4, p5, align = 'hv', nrow = 2)

# PRECO VERSUS METODO
dfx %>% ggplot() +
  geom_violin(aes(x = Metodo, y = log10(Preco)), 
              fill = "blue", alpha = 0.2, draw_quantiles = 0.5) +
  coord_flip() +
  theme_bw()

#
dfx %>% 
  dplyr::select(Preco, Quartos, Distancia, NumImoveis, Regiao) %>% 
  PairPlot(c("Preco", "Quartos", "Distancia", "NumImoveis"), 
           title = "Matriz de dispersão",
           group_var = "Regiao")

dfx %>% 
  dplyr::select(Preco, Quartos, Distancia, NumImoveis, Regiao)


dfx %>% 
  dplyr::select(log10(Preco), Quartos, Distancia, NumImoveis, Regiao) %>% 
  WVPlots::PairPlot(c("Preco", "Quartos", "Distancia", "NumImoveis"), 
                    title = "Matriz de dispersão",
                    group_var = "Regiao")

# PLOT DENSIDADE
# QUARTOS
p_quart <- ggplot(dfx, aes(x = Quartos)) + 
  geom_density(fill = "blue", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(Quartos)),
             color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("Quarto") +
  theme_bw()

p_log_quart <- ggplot(dfx, aes(x = log10(Quartos))) + 
  geom_density(fill = "blue", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(log10(Quartos))),
             color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("log10(Quarto)") +
  theme_bw()
p_quart <- cowplot::plot_grid(p_quart, p_log_quart, align = 'hv', nrow = 1)

# DISTANCIA
p_dist <- ggplot(dfx, aes(x = Distancia)) + 
  geom_density(fill = "black", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(Distancia)),
             color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("Distância") +
  theme_bw()

p_log_dist <- dfx %>% 
  dplyr::filter(Distancia > 0) %>%
  ggplot(aes(x = log10(Distancia))) + 
  geom_density(fill = "black", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(log10(Distancia))),
             color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("log10(Distância > 0)") +
  theme_bw()
p_dist <- cowplot::plot_grid(p_dist, p_log_dist, align = 'hv', nrow = 1)

# NUMERO IMOVEIS NO BAIRRO
p_nimov <- ggplot(dfx, aes(x = NumImoveis)) + 
  geom_density(fill = "green", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(NumImoveis)),
             color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("NumImoveis") +
  theme_bw()

p_log_nimov <- dfx %>% 
  ggplot(aes(x = log10(NumImoveis))) + 
  geom_density(fill = "green", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(log10(NumImoveis))),
             color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("log10(DNumImoveis)") +
  theme_bw()
p_nimov <- cowplot::plot_grid(p_nimov, p_log_nimov, align = 'hv', nrow = 1)

cowplot::plot_grid(p_quart, NA, p_dist, 
                   NA, p_nimov, NA, 
                   align = 'hv', nrow = 2)

# PRECO VERSUS TEMPO
dfx %>% 
  ggplot(aes(x = Data, y = Preco)) +
  geom_point(aes(col = Distancia), alpha = .5) +
  facet_wrap(~Regiao) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  theme_bw()

dfx %>% 
  ggplot(aes(x = Data, y = log10(Preco))) +
  geom_point(aes(col = Distancia), alpha = .5) +
  facet_wrap(~Regiao) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  theme_bw()

#
dfx %>% 
  ggplot(aes(x = Data, y = Distancia)) +
  geom_point(aes(col = Preco), alpha = .5) +
  facet_wrap(~Regiao) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  theme_bw()

dfx %>% 
  ggplot(aes(x = Data, y = Distancia)) +
  geom_point(aes(col = log10(Preco)), alpha = .5) +
  facet_wrap(~Regiao) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  theme_bw()

#
dfx %>% 
  ggplot(aes(x = Data, y = Distancia)) +
  geom_point(aes(col = Preco), alpha = .5) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  theme_bw()

dfx %>% 
  ggplot(aes(x = Data, y = Distancia)) +
  geom_point(aes(col = log10(Preco)), alpha = .5) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  theme_bw()

#
dfx %>% 
  ggplot(aes(x = Distancia, y = Preco)) +
  geom_point(aes(col = Regiao), alpha = .5) +
  theme_bw()

dfx %>% 
  ggplot(aes(x = Distancia, y = log10(Preco))) +
  geom_point(aes(col = Regiao), alpha = .5) +
  theme_bw()

# MAPA DE LOCALIZACAO IMOVEIS
ggmap::qmplot(Longitude, Latitude, data = dfx, 
              geom = "density2d", size = I(2), alpha = .1) +
  theme(legend.position = "none")

# NA
naniar::gg_miss_var(dfx, show_pct = TRUE) +
  labs(x = "Variáveis", y = "% NA")

# OBTENDO COTACAO HISTORICA DO DOLAR AUSTRALIANO ----
quantmod::getSymbols("AUDBRL=x")

# AJUSTANDO ARQUIVO
colnames(`AUDBRL=X`) <- c("x", "xx", "xxx", "fechamento", "xxxx", "xxxxx")
AUD <- data.frame(Data = index(`AUDBRL=X`),
                  `AUDBRL=X`, row.names = NULL)
AUD <- AUD %>%
  dplyr::select(Data, fechamento) %>% 
  na.omit()

# MERGE DE DF'S
df_ts <- AUD %>% 
  dplyr::left_join(dfx, by = "Data") %>% 
  tidyr::drop_na(Preco)

# plot
p_aud <- df_ts %>% 
  ggplot(aes(x = Data)) +
  geom_line(aes(y = fechamento), alpha = .5) +
  theme_bw()
  
p_ts <- df_ts %>% 
  ggplot(aes(x = Data)) +
  geom_point(aes(y = Preco, col = Distancia), alpha = .5, show.legend = F) +
  facet_wrap(~Regiao, ncol = 1) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  theme_bw()

cowplot::plot_grid(p_aud, p_ts, align = 'hv', 
                   rel_heights = c(.3, 1), nrow = 2)

#
p_ts <- df_ts %>% 
  ggplot(aes(x = Data)) +
  geom_point(aes(y = log10(Preco), col = Distancia), alpha = .5, show.legend = F) +
  facet_wrap(~Regiao, ncol = 1) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  theme_bw()

cowplot::plot_grid(p_aud, p_ts, align = 'hv', 
                   rel_heights = c(.3, 1), nrow = 2)
#
rm(df_ts, `AUDBRL=X`, AUD)

# FILL NA ----
fill_df <- dfx %>% 
  dplyr::select(-c(Latitude, Longitude, Quartos_aux, Banheiros, 
                   Garagem, Terreno, AnoConstrucao, AreaConstruida, 
                   id))

vars <- c("Quartos", 'Tipo', 'Preco', 'Metodo', 'NumImoveis', 
          'Distrito', "Distancia", 'Regiao') # VARIAVEIS PARA PREDICAO DE OUTRAS - FILL NA

fill <- c("Latitude", "Longitude", "Quartos_aux", 'Banheiros', 
          'Garagem', 'Terreno', 'AnoConstrucao', 'AreaConstruida')

handling <- function(x) { 
  
  fill_aux <- fill_df %>% 
    dplyr::select(vars) %>% 
    dplyr::mutate(ext = as.numeric(dfx[,fill[x]])) %>% 
    dplyr::mutate_if(is.factor, as.numeric) 
  
  
  fill_imp <- missForest::missForest(fill_aux)
  fill <- fill_imp$ximp
  
}

# Extraindo
k <- 1:8 %>% # 
  purrr::map(handling)

aux <- k %>% 
  purrr::map("ext")
names(aux) <- fill

aux <- aux %>% 
  tibble::as_tibble()

# Criando banco de dados tratado: 
fill <- fill_df %>% 
  dplyr::select(vars) %>% 
  dplyr::bind_cols(aux)

# CHECK NA
naniar::gg_miss_var(fill, show_pct = TRUE) +
  labs(x = "Variáveis", y = "% NA")

{
  Tipo    <- tibble::as.tibble(dummies::dummy(dfx$Tipo))
  Metodo  <- tibble::as.tibble(dummies::dummy(dfx$Metodo))
  Regiao  <- tibble::as.tibble(dummies::dummy(dfx$Regiao))
  }

# AJUSTE DO DATAFRAME
df_aux <- dfx %>%
  dplyr::select(-c(Bairro, Endereco, Corretor, # ALTA QTD DE LVLS
                   Tipo, Metodo, Distrito, Regiao, 
                   CEP,  # INCOERENTE PARA MODELGAGEM PREDITIVA
                   Data, # BAIXA RELEVANCIA 
                   id,    # INCOERENTE PARA MODELGAGEM PREDITIVA
                   Latitude, Longitude, Quartos_aux, Banheiros,    # FILL NA
                   Garagem, Terreno, AnoConstrucao, AreaConstruida # FILL NA
                   )) %>% 
  dplyr::bind_cols(list(Tipo, Metodo, Regiao)) %>% 
  dplyr::mutate(Latitude  = fill$Latitude,
                Longitude = fill$Longitude,
                Quartos_aux = as.integer(round(fill$Quartos_aux)), 
                Banheiros = as.integer(round(fill$Banheiros)), 
                Garagem   = as.integer(round(fill$Garagem)), 
                Terreno   = as.integer(round(fill$Terreno)),
                AnoConstrucao  = as.integer(round(fill$AnoConstrucao)), 
                AreaConstruida = fill$AreaConstruida,
                Tipo_h = as.factor(`Tipo)h`),
                Tipo_t = as.factor(`Tipo)t`), 
                Tipo_u = as.factor(`Tipo)u`),
                Metodo_PI = as.factor(`Metodo)PI`),
                Metodo_S  = as.factor(`Metodo)S`),
                Metodo_SA = as.factor(`Metodo)SA`),
                Metodo_SP = as.factor(`Metodo)SP`),
                Metodo_VB = as.factor(`Metodo)VB`),           
                Regiao_Eastern_Metropolitan = as.factor(`Regiao)Eastern Metropolitan`),
                Regiao_Eastern_Victoria = as.factor(`Regiao)Eastern Victoria`),
                Regiao_Northern_Metropolitan = as.factor(`Regiao)Northern Metropolitan`),     
                Regiao_Northern_Victoria = as.factor(`Regiao)Northern Victoria`),
                Regiao_South_Eastern_Metropolitan = as.factor(`Regiao)South-Eastern Metropolitan`),
                Regiao_Southern_Metropolitan = as.factor(`Regiao)Southern Metropolitan`),     
                Regiao_Western_Metropolitan  = as.factor(`Regiao)Western Metropolitan`),
                Regiao_Western_Victoria = as.factor(`Regiao)Western Victoria`), 
                Preco = log10(Preco)              # FEATURE ENGINEERING
                ) %>% 
  dplyr::select(-c(`Tipo)h`, `Tipo)t`, `Tipo)u`, 
                   `Metodo)PI`, `Metodo)S`, `Metodo)SA`,  `Metodo)SP`, `Metodo)VB`,
                   `Regiao)Eastern Metropolitan`, `Regiao)Eastern Victoria`,
                   `Regiao)Northern Metropolitan`, `Regiao)Northern Victoria`,
                   `Regiao)South-Eastern Metropolitan`, `Regiao)Southern Metropolitan`,
                   `Regiao)Western Metropolitan`, `Regiao)Western Victoria`))
dplyr::glimpse(df_aux)

# CHECK NA
anyNA(df_aux)

# CHECK ESTRUTURA
dplyr::glimpse(df_aux)

ggplot(df_aux, aes(x = Preco)) + 
  geom_density(fill = "blue", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(Preco)),
             color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("log10(Preço)") +
  theme_bw()

# CHECK POSSIVEIS IMOVEIS REPETIDOS
df_dup <- df %>% 
  dplyr::mutate(id = 1:nrow(df),
                Preco_orig = Preco,
                Lat_orig  = Latitudo,
                Long_orig = Longitude,
                Data = as.Date(Data)) %>% 
  tidyr::drop_na(Preco) %>% 
  dplyr::filter(Regiao   != "#N/A",
                Distrito != "#N/A",
                NumImoveis != "#N/A", 
                Distancia  != "#N/A") %>% 
  dplyr::select(Data, Preco_orig, Lat_orig, Long_orig, id) %>% 
  dplyr::mutate(Preco = df_aux$Preco,
                Latitude  = df_aux$Latitude,
                Longitude = df_aux$Longitude,
                Lat_long_orig = paste(Lat_orig, Long_orig),
                Lat_long_pos  = paste(round(Latitude, 4), round(Longitude, 4)),
                check   = ifelse(Lat_long_orig == Lat_long_pos, "check", "ok"),
                Pres_NA = ifelse(is.na(Latitude) | is.na(Longitude), "duplicada", "ponto_nao_duplicado")) %>% 
  dplyr::mutate(check = as.factor(check)) %>% 
  na.omit() %>% 
  dplyr::select(id, check, Pres_NA)
table(df_dup$Pres_NA)

# VERIFICACAO DE POSSIVEIS OUTLIERS ----
# AJUSTANDO DATAFRAME PARA CLUSTERING
df_num <- df_aux %>%
  na.omit() %>% 
  tibble::as_tibble() %>%
  dplyr::select_if(is.numeric) %>% 
  scale() %>% 
  tibble::as_tibble()

df_model <- df_aux %>%
  na.omit() %>% 
  tibble::as_tibble() %>%
  dplyr::select_if(is.factor) %>% 
  dplyr::bind_cols(df_num) %>% 
  as.data.frame()
#
rm(df_num)

# CLUSTERING
# ESTIMATVA DO NUMERO OTIMO DE CLUSTERS
{
  set.seed(1)
  # NUMERO MAXIMO DE CLUSTERS
  k_max <- 15
  
  # WITHIN CLUSTER SUM OF SQUARE
  wss <- sapply(1:k_max, 
                function(k){clustMixType::kproto(df_model, k)$tot.withinss})
}

wss <- data.frame(WSS = wss, 
                   K = 1:k_max)

# PLOT WSS
ggplot(wss) +
  geom_line(aes(x = K, y = WSS)) +
  geom_vline(xintercept = 6, color = "red", linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(2, k_max, 2)) +
  theme_bw()

# CLUSTERING COM K = 6
df_model <- clustMixType::kproto(df_model, 6)
df_aux_cluster <- df_aux %>%
  dplyr::mutate(cluster = as.factor(df_model$cluster))

# PLOT CLUSTER
df_aux_cluster %>% 
  ggplot() +
  geom_point(aes(x = Distancia, y = Preco, col = cluster), alpha = 0.3) +
  theme_bw()

df_aux_cluster %>% 
  ggplot() +
  geom_point(aes(x = Distancia, y = Preco, col = NumImoveis), alpha = 0.3) +
  facet_wrap(~cluster) +
  theme_bw()

df_aux_cluster %>% 
  ggplot() +
  geom_violin(aes(x = as.factor(Quartos), y = Preco, fill = as.factor(Quartos)),
              alpha = 0.2, show.legend = F) +
  facet_wrap(~cluster, nrow = 1) +
  theme_bw()

# PLOTS DOS CLUSTERS NO ESPACO GEORREFERENCIADO
# MAPA DE LOCALIZACAO IMOVEIS
ggmap::qmplot(Longitude, Latitude, data = df_aux_cluster, geom = "blank",
              maptype = "toner-background", darken = 0.7, legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .1, color = NA) +
  scale_fill_gradient2("Imóveis", 
                       low = "white", mid = "yellow", high = "red", midpoint = 13)

df_aux_cluster %>% 
  ggplot() +
  geom_point(aes(x = Longitude, y = Latitude, 
                 col = cluster, fill = Preco),
             alpha = 0.3, shape = 21) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw()

df_aux_cluster %>% 
  ggplot() +
  geom_point(aes(x = Longitude, y = Latitude, 
                 col = cluster, shape = cluster)) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw()

plot_out <- df_aux_cluster %>% 
  ggplot() +
  geom_point(aes(x = Longitude, y = Latitude, 
                 col = cluster, shape = cluster)) +
  facet_wrap(~cluster) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw()
plot_out
plotly::ggplotly(plot_out)

df_aux_cluster %>% 
  ggplot() +
  geom_point(aes(x = Longitude, y = Latitude, 
                 col = Preco, shape = cluster)) +
  facet_wrap(~cluster) +
  scale_color_gradient(low = "blue", high = "red", name = "Preço") +
  scale_shape_discrete(guide = FALSE) + 
  xlab("Longitude") + ylab("Latitude") +
  labs(subtitle = "Clusters") +
  theme_bw()


# REMOVENDO PONTOS MAIS DISTANTES DA MASSA DE DADOS POIS E ESPERADO QUE
# A LOCALIZACAO TENHA FORTE INFLUENCIA NO PRECO, CONTUDO E OBSERVADO QUE 
# EXISTEM PONTOS QUE SE DISTANCIAM DA MASSA DE DADOS, CLUSTER. 

p_out <- df_aux_cluster %>% 
  dplyr::mutate(out = ifelse(df_aux$Latitude == -37.45392, "out", "ok")) %>% 
  ggplot(aes(x = Longitude, y = Latitude)) +
  geom_point(aes(alpha = out), 
             shape = 21, size = 5, stroke = 1, col = "black", show.legend = F) +
  geom_point(aes(col = Preco, shape = cluster)) +
  facet_wrap(~cluster) +
  scale_color_gradient(low = "blue", high = "red", name = "Preço") +
  scale_shape_discrete(guide = FALSE) +
  scale_alpha_manual(values = c(0, 1)) +
  xlab("Longitude") + ylab("Latitude") +
  labs(subtitle = "Clusters") +
  theme_bw()

ann_text <- data.frame(Latitude = -37.5, Longitude = 144.59, lab = "Outlier",
                       cluster = factor(6, levels = c("1", "2", "3", "4", "5", "6")))
p_out + 
  geom_text(data = ann_text, label = "Outlier")


df_aux_cluster %>% 
  ggplot() +
  geom_point(aes(x = Longitude, y = Latitude, 
                 col = cluster, shape = cluster)) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw()


# TESTE - PREDICAO DE PRECOS ----
df_model <- df_aux %>% 
  dplyr::mutate_if(is.factor, as.numeric) %>% 
  dplyr::filter(Latitude != -37.45392 & Longitude != 144.5886)

{
  set.seed(1)
  
  teste <- df_aux[sample(nrow(df_model), nrow(df_model)*0.1), ]
  }

# SET DE TREINAMENTO E VALIDACAO
{
  set.seed (1) 
  index <- caret::createDataPartition(teste$Preco, 
                                      p = 0.75,       
                                      list = FALSE) 
  
  train <- teste[index, ]
  test  <- teste[-index, ]
  
  my_control <- caret::trainControl(method = "cv",
                                    number = 10,
                                    savePredictions = T)

  # MODELOS 
  model_list <- caretEnsemble::caretList(Preco~.,
                                         data = train,
                                         trControl = my_control,
                                         methodList = c("rf",
                                                        "xgbTree",
                                                        "xgbLinear"),
                                         preProcess = c("center", "scale"),
                                         importance = TRUE)

}

model_list$rf
model_list$xgbTree
model_list$xgbLinear

options(digits = 3)
mdl_res_RMSE <- data.frame(RF  = min(model_list$rf$results$RMSE),
                           XGT = min(model_list$xgbTree$results$RMSE),
                           XGL = min(model_list$xgbLinear$results$RMSE))
mdl_res_RMSE

resamples <- caret::resamples(model_list)
dotplot(resamples, metric = "RMSE")

# ANALISE DE PERFORMANCE DO MODELO
pred <- caretEnsemble::caretEnsemble(model_list)
caret::postResample(predict(pred, test), 
                    obs  = test$Preco)

# REDICAO DE PRECOS ----
df_model <- df_aux %>% 
  dplyr::mutate_if(is.factor, as.numeric) %>% 
  dplyr::filter(Latitude != -37.45392 & Longitude != 144.5886)

# SET DE TREINAMENTO E VALIDACAO
{
  set.seed (1) 
  index <- caret::createDataPartition(df_model$Preco, 
                                      p = 0.75,       
                                      list = FALSE) 
  
  train <- df_model[index, ]
  test  <- df_model[-index, ]
  
  my_control <- caret::trainControl(method = "cv",
                                    number = 10,
                                    savePredictions = T)
  
  # MODELOS 
  model_listF <- caretEnsemble::caretList(Preco~.,
                                          data = train,
                                          trControl = my_control,
                                          methodList = c("rf",
                                                         "xgbTree","xgbLinear"),
                                          preProcess = c("center", "scale"),
                                          importance = TRUE)
  
}

model_listF$rf
model_listF$xgbTree
model_listF$xgbLinear

options(digits = 3)
mdl_res_RMSE_F <- data.frame(RF  = min(model_listF$rf$results$RMSE),
                             XGT = min(model_listF$xgbTree$results$RMSE),
                             XGL = min(model_listF$xgbLinear$results$RMSE))
mdl_res_RMSE_F

resamplesF <- caret::resamples(model_listF)
dotplot(resamplesF, metric = "RMSE")

# ANALISE DE PERFORMANCE DO MODELO
predF <- caretEnsemble::caretEnsemble(model_listF)
caret::postResample(predict(predF, test), 
                    obs  = test$Preco)
