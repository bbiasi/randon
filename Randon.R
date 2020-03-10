
# INFORMACOES ----
# CODIGO DESENVOLVIDO EM R
# TESTE PARA VAGA DE CIENTISTA DE DADOS NA RANDON
# NOME: BRENNER BIASI SOUZA SILVA

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
                 reshape2, spacetime, ggmap, cowplot, WVPlots, purrr, caretEnsemble)
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
  dplyr::mutate(NumImoveis = as.numeric(as.character(NumImoveis)),
                Distancia  = as.numeric(as.character(Distancia)),
                Latitude   = as.numeric(as.character(Latitude)),
                Longitude  = as.numeric(as.character(Longitude))) %>%  
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
                Quartos_aux = fill$Quartos_aux, 
                Banheiros = fill$Banheiros, 
                Garagem   = fill$Garagem, 
                Terreno   = fill$Terreno,
                AnoConstrucao  = fill$AnoConstrucao, 
                AreaConstruida = fill$AreaConstruida,
                Preco = log10(Preco)) # FEATURE ENGINEERING

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

# PREDICAO DE PRECOS ----

X <- df_aux %>% 
  dplyr::select(-Preco)
y <- df_aux %>% 
  dplyr::select(Preco)

{
  set.seed (1) 
  part_index <- caret::createDataPartition(df_aux$Preco, 
                                           p = 0.75,        
                                           list = FALSE) 
  X_train <- X[part_index, ] 
  X_test  <- X[-part_index,] 
  y_train <- y[part_index ] 
  y_test  <- y[-part_index]
  
  my_control <- caret::trainControl(method = "cv",
                                    number = 10,
                                    savePredictions = "fianl",
                                    allowParallel   = TRUE)
}

{
  set.seed(1)
  model_list <- caretEnsemble::caretList(X_train,
                                         y_train,
                                         trControl = my_control,
                                         methodList = c("lm", "rf", 
                                                         "xgbTree", "xgbLinear"),
                                         tuneList = NULL,
                                         continue_on_fail = FALSE, 
                                         preProcess = c("center", "scale", "pca"))
}

{
  set.seed(1)
  model_spca <- caretEnsemble::caretList(X_train,
                                         y_train,
                                         trControl = my_control,
                                         methodList = c("lm", "rf", 
                                                        "xgbTree", "xgbLinear"),
                                         tuneList = NULL,
                                         continue_on_fail = FALSE, 
                                         preProcess = c("center", "scale"))
}



