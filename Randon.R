###
###
# ATENCAO: CODIGO DESENVOLVIDO EM R


# INFORMACOES PRELIMINARES ----
# NOME DO DEV.: BRENNER BIASI S. SILVA

# SOFTWARES E PC UTILIZADOS NESTE DESENVOLVIMENTO
# R version 3.6.2 (2019-12-12)
# RSTUDIO Version 1.2.5033
# macOS MOJAVE
# PLATAFORMA - x86_64-apple-darwin15.6.0


# ADEQUANDO O AMBIENTE DE TRABALHO ----

#### INSTRUCAO:

# O ARQUIVO `~/precos_imoveis.csv`, REFERENTE AO BANCO DE DADOS (DATASET), 
# DEVERA ESTAR NO MESMO DIRETORIO DE TRABALHO DESTE SCRIPT.


# ATENCAO: TODO O ENVIRONMENT ESERA PREVIAMENTE LIMPO
rm(list = ls()); gc()


# DATASET ----
# IMPORTANDO DATASET
df <- read.csv("precos_imoveis.csv", header = TRUE)

# INSTALANDO E CARREGANDO PACOTES REQUERIDOS PARA ESTE SCRIPT----
{
  if(!require("pacman")) install.packages("pacman")
  
  pacman::p_load(dplyr, tidyr, naniar, ggplot2, lubridate, stringr, 
                 dummies, missForest, tibble, quantmod, caret, Metrics, 
                 reshape2, spacetime, ggmap, cowplot, WVPlots, purrr, 
                 caretEnsemble, clustMixType, plotly, xgboost, glmnet, 
                 randomForest)
  }


# ANALISE E AJUSTE DA ESTRUTURA DO DATASET ----
# ESTRUTURA DO DATASET, CLASSE DE VARIAVEIS
dplyr::glimpse(df)

# TEM-SE:
# 34857 OBSERVACOES - IMOVEIS
# 21 VARIAVEIS

# AS VARIAVEIS SAO:
# 08 CATEGORICAS (INCLUINDO O CEP)
# 12 CONTINUAS
# 01 DATA DE REFERENCIA - TRANSACAO


# VERIFICACAO VISUAL DE VALOR AUSENTES (NA) NO DATASET
naniar::gg_miss_var(df, show_pct = TRUE) +
  labs(x = "Variáveis", y = "% NA")

# TEM-SE:
# 09 VARIAVEIS COM NA'S, INCLUINDO A VARIAVEL Preco, QUE SERA NOSSO [f(x)]


# - SELECAO INCIAL, TRANSFORMACAO E AJUSTE DE VARIAVEIS -
# NESTA ETAPA FOI CRIADO UMA COLUNA DE ID PARA CONTROLE DA POSICAO (ROW) DO 
# IMOVEL FRENTE AO DATASET ORIGINAL. POSTERIORMENTE FOI REALIZADO A EXTRACAO DO
# NOME DOS LOGRADOUROS DOS IMOVEIS, COMO PROCESSO DE FEATURE ENGINEERING. 
# TAMBEM FOI REALIADO A ADEQUADAO DAS VARIAVEIS CATEGORICAS (FACTOR), BEM 
# COMO OUTRAS ACOES, QUE ESTAO COMENTADAS AO LONGO DO CODIGO.

dfx <- df %>% 
  dplyr::mutate(id = 1:nrow(df), 
                Data = lubridate::parse_date_time(Data, '%d-%m-%Y'), 
                Data = as.Date(Data),  #  ADEQUACAO DA VARIAVEL Data
                Endereco = stringr::str_to_lower(Endereco), #  EXTRACAO DO NOME 
                Endereco = stringr::str_trim(Endereco),     # DOS LOGRADOUROS 
                Endereco = stringr::str_replace_all(Endereco, "[:digit:]", ""),
                Endereco = gsub(" *\\b[[:alpha:]]{1}\\b *", "", Endereco),
                Endereco = gsub("[[:punct:]]", "", Endereco),
                Endereco = stringr::str_trim(Endereco),
                Endereco = as.factor(Endereco),
                Tipo     = as.factor(Tipo), # ADEQUADAO DE VARIAVEIS
                Metodo   = as.factor(Metodo),
                Latitude = Latitudo,
                Regiao   = as.factor(Regiao),
                Distrito = as.factor(Distrito),
                NumImoveis = as.factor(NumImoveis),
                Distancia  = as.factor(Distancia)) %>%
  dplyr::filter(Regiao   != "#N/A", # REMOCAO DE ERROS DE TABULACAO
                Distrito != "#N/A",
                NumImoveis != "#N/A", 
                Distancia  != "#N/A") %>%
  dplyr::mutate(NumImoveis  = as.numeric(as.character(NumImoveis)),# ADEQUACAO
                Distancia   = as.numeric(as.character(Distancia)), #    DE
                Latitude    = as.numeric(as.character(Latitude)),  # VARIAVEIS
                Longitude   = as.numeric(as.character(Longitude))) %>% # RENAME
  dplyr::select(-Latitudo) %>%  # REMOCAO DA VARIAVEL DUPLICADA
  tidyr::drop_na(Preco)         # REMOCAO DE NA'S NA VARIAVEL PRECO


# A PRIORI, A UNICA VARIAVEI QUE E POSSIVEL OMITIR OS NA'S E NA VARIAVEL ALVO,
# QUE SERA O PRECO. AS OUTRAS VARIAVEIS COM NA'S, APESAR DOS ELEVADOS %, COMO 
# AreaConstruida E AnoConstrucao, TERAO SEUS DADOS FALTANTES PREENCHIDOS, POIS
# E ESPERADO QUE ESSSAS DUAS VARIAVEIS SEJAM IMPORTANTES PARA A PRECIDAO DE 
# PRECOS.


# CHECK DE NA'S
naniar::gg_miss_var(dfx, show_pct = TRUE) +
  labs(x = "Variáveis", y = "% NA")


# A VARIAVEL Terreno POSSUI ALGUMAS OBSERVACOES QUE CHAMAM ATENCAO. COMO TERRENO
# IGUAL A 0 (ZERO). CONTUDO ESSA INFORMACAO FOI MANTIDA, TENDO EM VISTA O
# RACIOCINIO QUE Terreno E A DIFERENCA ENRE TERRENO DISPONIVEL E AREA CONSTRUIDA
# NO TERRENO.


# ANALISE EXPLORATORIA DOS DADOS ----
# RESUMO GERAL
dfx %>% 
  summary()

# VARIAVEL ENDERECO
# A VARIAVEL ENDERECO, MESMO SENDO REDUZIDA AO NOME DOS LOGRADOUROS, APRESENTA
# DEMASIADA QUANTIDADE DE LEVELS. LOGO, APESAR DO NIVEL DE DETALHER SER
# INTERESSANTE, NAO ACRESCENTA TANTA INFORMACAO PARA MODELAGEM PREDITIVA.
length(table(dfx$Endereco))

# OUTRA VARIAVEL SEMELHANTE A DA FIGURA DO CORRETOR DE IMOVEL (Corretor)
# E DA QUANTIDA DE BAIRROS 
length(table(dfx$Corretor))
length(table(dfx$Bairro))


# AS DEMAIS VARIAVEIS CATEGORICAS
variaveis <- c("Tipo", "Metodo", "Distrito", "Regiao")

df_variaveis <- list()

for (i in 1:length(variaveis)) {
  
  df_temp <- dfx %>% 
    dplyr::group_by(get(paste0(variaveis[i]))) %>% 
    dplyr::summarise(Prec_med   = mean(Preco,      na.rm = TRUE),
                     Ban_med    = mean(Banheiros,  na.rm = TRUE),
                     Gar_med    = mean(Garagem,    na.rm = TRUE),
                     Quart_med  = mean(Quartos,    na.rm = TRUE),
                     Dist_med   = mean(Distancia,  na.rm = TRUE),
                     Terren_med = mean(Terreno,    na.rm = TRUE),
                     NImv_med   = mean(NumImoveis, na.rm = TRUE))
  
  df_temp$Variavel <- paste0(variaveis[i])
  
  df_variaveis[[i]] <- df_temp

}

df_variaveis  <- dplyr::bind_rows(df_variaveis)
df_variaveist <- df_variaveis %>%
  dplyr::rename("Categoria" = 1) %>%
  reshape2::melt(id.vars = c("Variavel", "Categoria"))

ggplot(df_variaveist, aes(x = variable, y = value)) +
  geom_jitter(aes(col = Categoria), alpha = 0.3, show.legend = F) +
  geom_violin(alpha = 0.1, show.legend = F) +
  facet_grid(variable~Variavel, scale = "free") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))

# AS INFORMACOES DE ESTATISTICA DESCRITIVA DAS VARIAVEIS CATEGORICAS, DE ACORDO
# COM AS SUAS CATEGORIAS EXISTENTES, PODEM SER  ENCONTRADAS NOS OBJETOS 
# df_variaveis E df_variaveist.

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

cowplot::plot_grid(p, p_log, align = 'hv', nrow = 1) # JUNTANDO PLOTS p E p_log

# A VARIAVEL PRECO APRESENTA ASSIMETRIA A ESQUERDA. PARA TANTO, A TRANSFORMACAO
# UTILIZANDO `log10()`, LOG NA BASE 10, APRESENTA SER UMA BOA SOLUCAO PARA
# TRANSFORMACAO DOS DADOS E POTENCIALIZACAO DE PREDICAO.

# TAL FATO SOBRE A TRANSOFORMACAO PODE SER VISUALIZADO ABAIXO, COM  O GANHO DE 
# INFORMACAO VISUAL A PARTIR DA TRANSFORMACAO DA VARIAVEL.


# GRAFICO DO PRECO VERSUS REGIAO
dfx %>% 
  ggplot(aes(x = Regiao, y = Preco)) + # VARIAVEL SEM TRANSFORMACAO LOG10
  geom_violin(fill = "blue", alpha = 0.2, draw_quantiles = 0.5) +
  coord_flip() +
  theme_bw()

dfx %>% 
  ggplot(aes(x = Regiao, y = log10(Preco))) + # VARIAVEL COM TRANSFORMACAO
  geom_violin(fill = "blue", alpha = 0.2, draw_quantiles = 0.5) +
  coord_flip() +
  theme_bw()

# PODEMOS CONSTATAR QUE OS MENORES PRECOS MEDIOS DE IMOVEIS ESTAO NA REGIAO DE 
# WESTERN VICTORIA. ALÉM DISTO, ESTA REGIAO, EM CONJUNTO COM AS REGIOES DE 
# NORTHERN VICTORIA E EASTERN VICTORIA, APRESENTAM BAIXA DISPERSAO NOS VALORES
# DOS IMOVEIS. A REGIAO COM MAIOR MEDIA DE PRECO, E TAMBEM COM MAIOR DISPERSAO,
# E A REGIAO DE SOUTHERN METROPOLITAN.

# PRECO VERSUS DISTRITO
dfx %>% 
  ggplot(aes(x = Distrito, y = log10(Preco))) +
  geom_violin(fill = "blue", alpha = 0.2, draw_quantiles = 0.5) +
  coord_flip() +
  theme_bw()

# SOBRE O PRECO EM RELACAO AO DISTRITO, OS MENORES PRECOS DE IMOVEIS ESTAO EM 
# Maribyrnong City Council, CONTUDO A MENOR MEDIA PERTENCE A Moorabool Shire 
# Council. 


# PRECO VERSUS DISTANCIA, METODO
p2 <- dfx %>% 
  ggplot(aes(x = Preco, y = Distancia)) +
  geom_point(col = "grey", alpha = 0.4) +
  geom_smooth() +
  facet_wrap(~Metodo, nrow = 1) +
  theme_bw()

p3 <- dfx %>% 
  ggplot(aes(x = log10(Preco), y = Distancia)) +
  geom_point(col = "grey", alpha = 0.4) +
  geom_smooth() +
  facet_wrap(~Metodo, scale = "free", nrow = 1) +
  theme_bw()

cowplot::plot_grid(p2, p3, align = 'hv', nrow = 2)

# COMO PODE SER CONSTATADO NO PLOT ACIMA, A VARIAVEL PRECO E DISTANCIA NAO 
# APRESENTAM BOA CORRELACAO. NAO HA UM PADRAO CLARO ERNTRE ESSAS VARIAVEIS, ATE
# MESMO UTILIZANDO O METODO DE VENDA COMO UMA FACETA.


# CONTUDO, ASSIM COMO REALIZADO PARA A VARIAVEL PRECO, A DENDIDADE DAS OUTRAS
# OUTRAS VARIAVEIS TAMBÉM FORAM ANALISADAS.

p_dis_d <- ggplot(dfx, aes(x = Distancia)) + 
  geom_density(fill = "blue", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(Distancia)),
             color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("Distância") +
  theme_bw()

p_dis_d_log <- ggplot(dfx, aes(x = ifelse(Distancia == 0, 
                                          0, log10(Distancia)))) + 
  geom_density(fill = "blue", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(ifelse(Distancia == 0, 
                                          0, log10(Distancia)))),
             color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("log10(Distância)") +
  theme_bw()

p_dis_d_ln <- ggplot(dfx, aes(x = ifelse(Distancia == 0, 
                                         0, log(Distancia)))) + 
  geom_density(fill = "blue", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(ifelse(Distancia == 0, 
                                          0, log(Distancia)))),
             color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("ln(Distância)") +
  theme_bw()

p_dis_d_sq <- ggplot(dfx, aes(x = ifelse(Distancia == 0, 
                                         0, sqrt(Distancia)))) + 
  geom_density(fill = "blue", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(ifelse(Distancia == 0, 
                                          0, sqrt(Distancia)))),
             color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("√(Distância)") +
  theme_bw()

cowplot::plot_grid(p_dis_d, p_dis_d_log, p_dis_d_ln, p_dis_d_sq,
                   align = 'hv', nrow = 2)

# PODE-SE OBSERVAR QUE ALEM DA TRANSFORMACAO LOG10, OUTRAS TRANSFORMACOES PODEM
# SER FEITAS. CONTUDO, TENDO EM VISTA O PRINCIPIO DA PARCIMONIA, DEMAIS 
# TRANSFORMACOES NAO SERAO EMPREGADAS, POIS OS ALGORITMOS A SEREM UTILIZADOS SAO 
# ROBUSTOS A OUTLIERS E VIES.

# PRECO VERSUS DISTANCIA, REGIAO
# VARIAVEIS PRECO E DISTANCIA COM TRANSFORMACOES
# geom_smooth() == REGRESSAO PREVISTA PARA A MASSA DE DADOS
p4 <- dfx %>% 
  ggplot(aes( x = log10(Preco), y = ifelse(Distancia == 0, 
                                          0, log10(Distancia)))) +
  geom_point(fill = "blue", alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~Regiao) +
  ylab("log10(Distância)") +
  theme_bw()

p5 <- dfx %>% 
  ggplot(aes(x = log10(Preco), y = Distancia)) +
  geom_point(fill = "blue", alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~Regiao) +
  ylab("Distância") +
  theme_bw()

p6 <- dfx %>% 
  ggplot(aes(x = log10(Preco), y = ifelse(Distancia == 0, 
                                         0, sqrt(Distancia)))) +
  geom_point(fill = "blue", alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~Regiao) +
  ylab("√(Distância)") +
  theme_bw()

cowplot::plot_grid(p4, p5, p6, align = 'hv')

# E PERCEPTIVEL TAMBEM QUE NAO HA TANTO GANHO DE INFORMACAO. OS DADOS CONTINUAM
# SEM APRESENTAR TENDENCIAS CLARA.


# PRECO VERSUS METODO
dfx %>% ggplot(aes(x = Metodo, y = log10(Preco))) +
  geom_violin(fill = "blue", alpha = 0.2, draw_quantiles = 0.5) +
  coord_flip() +
  theme_bw()

# EM RELACAO AO METODO DE VENDA, A MEDIA DE PRECO NAO APRESENTA MUITA DIFERENCA,
# POREM QUANDO O METODO E O `PI`, TEMOS UMA MAIOR DISPERSAO DE OBSERVACOES.
# SE `PI` FOR INTERPRETADO COMO IMOVEIS DE HERANCA, E `VB` COMO FLOOD PARA 
# LEILAO (ONDE O "COMPRADOR" INFLACIONA O PRECO), PODE-SE CONSTATAR O PORQUE
# DESSA ALTA DISPERSAO, POIS NAO HA UMA INTENCAO CLARA NA COMPRA DO IMOVEL, E 
# SIM UMA CONSEGUENCIA.


# PLOT DE DISPERSAO  EM FUNCAO DA REGIAO
dfx %>% 
  dplyr::select(Preco, Quartos, Distancia, NumImoveis, Regiao) %>% 
  WVPlots::PairPlot(c("Preco", "Quartos", "Distancia", "NumImoveis"), #VARIAVEIS
                    title = "Matriz de dispersão",
                    group_var = "Regiao")

dfx %>% 
  dplyr::select(Preco, Quartos, Distancia, NumImoveis, Regiao) %>% 
  dplyr::mutate(Preco = log10(Preco)) %>%           # PRECO TRANSFORMADO
  WVPlots::PairPlot(c("Preco", "Quartos", "Distancia", "NumImoveis"), 
                    title = "Matriz de dispersão",
                    group_var = "Regiao")


# PLOT DENSIDADE - DEMAIS VARIAVEIS
# QUARTOS
p_quart <- ggplot(dfx, aes(x = Quartos)) + 
  geom_density(fill = "blue", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(Quartos)),
             color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("Quarto") +
  theme_bw()

# NUMERO IMOVEIS NO BAIRRO
p_nimov <- ggplot(dfx, aes(x = NumImoveis)) + 
  geom_density(fill = "green", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(NumImoveis)),
             color = "red", linetype = "dashed", size = 1) +
  ylab("Densidade") + xlab("NumImoveis") +
  theme_bw()

cowplot::plot_grid(p_quart, p_nimov, align = 'hv')

# PRECO VERSUS TEMPO
dfx %>% 
  ggplot(aes(x = Data, y = Preco)) +
  geom_point(aes(col = Distancia), alpha = .5) +
  facet_wrap(~Regiao) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  theme_bw()

# PODEMOS PERCEBER QUE OS IMOVEIS MAIS LONGES DO CENTRO (MAIORES VALORES DE 
# Distancia) FORAM VENDIDOS MAIS RECENTE, PREDOMINAM NAS REGIOES DE Eastern 
# Victoria, Northern Victoria, South-Eastern Metropolitan e Western Victoria.
# IMOVEIS COM ESSAS CARACTERISTICAS TAMBEM NAO COSTUMAM APRESENTAR PRECO ALTO.

# DIANTE DISSO, VARIAS HIPOTESES PODEM SURGIR, COMO:
# - INFLUENCIA DO MODO DE VIDA;
# - PERSPECTIVA DE REDUCAO DO TEMPO DE DESLOCAMENTO A CIDADE/COMPROMISSOS;
# -  HA INFLUENCIA DO MERCADO FINANCEIRO?;
# ...
# CONTUDO, DIANTE DESTA PERSPECTIVA, FICA NOTORIO A RELEVANCIA DO POSICIONAMENTO
# GEOGRAFICO DOS IMOVEIS, COM GRUPO DE Regiao. 

dfx %>% 
  ggplot(aes(x = Data, y = log10(Preco))) +
  geom_point(aes(col = Distancia), alpha = .5) +
  facet_wrap(~Regiao) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  theme_bw()

#
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

# PRECO VERSUS DISTANCIA
dfx %>% 
  ggplot(aes(x = Distancia, y = Preco)) +
  geom_point(aes(col = Regiao), alpha = .5) +
  theme_bw()

dfx %>% 
  ggplot(aes(x = Distancia, y = log10(Preco))) +
  geom_point(aes(col = Regiao), alpha = .5) +
  theme_bw()

# AO MONTAR O GRAFIDO DE PRECO VERSUS A DISTANCIA, NAO E TOA PERCEPTIVEL ALGUMAS
# DAS INFORMACOES ANTERIORES. POREM, COMO JA SABEMOS QUE A VARIAVEL Distancia
# PODE SER RELEVANTE PARA A NOSSA ANALISE E COMO TEMOS O PONTO GEORREFERENCIADO
# DOS IMOVEIS, PROXIMO PASSO APRESENTA UMA ANALISE DO PRECO EM FUNCAO DA DISTRI-
# BUICAO ESPACIAL REAL SOBRE A CIDADE DE MELBOURNE, NA AUSTRALIA. QUE E A 
# CIDADE QUE CONTEM ESTES IMOVEIS.

# UTILIZANDO AS INFORMACOES DE LATITUDE E LONGITUDE DOS IMOVEIS, FOI CRIADO O 
# PRIMEIRO MAPA DE SITUACAO UTILIZANDO O PACOTE `ggmap`.
# MAPA DE LOCALIZACAO IMOVEIS
ggmap::qmplot(Longitude, Latitude, data = dfx, 
              geom = "density2d", size = I(2), alpha = .1) +
  theme(legend.position = "none")

# ESTE MAPA NOS PERMITE VERIRICAR A DENSIDADE DE IMOVEIS NA CIDADE. E, MESMO
# NAO CONHECENDO A CIDADEM PODEMOS PERCEBER QUE HA POUCOS IMOVEIS NA ZONA RURAL
# OU SUBURBIO DA CIDADADE. A MAIORIA ESTA PROXIMO AO CENTRO, AO LONGO DA 
# BAIA DE PORT PHILLIP.

# APESAR DA POSSIBILIDADE DE EMPREGO DE GEOESTATISTICA PARA A PREDICAO DO Preco,
# ESTA ABORDAGEM NAO FOI EMPREGADA EM FUNCAO DO CUSTO COMPUTACIONAL PARA 
# ALGORITMOS MAIS ROBUSTOS, COMO O HIBRIDO DE RANDOM FOREST COM IDW.


# INFLUENCIA DO MERCADO FINANCEIRO SOBRE O PRECO DOS IMOVEIS ----
# COM A INFORMACAO QUE QUE AS CASAS EM ALGUMAS REGIOES APRESENTARAM MENOR VALOR 
# DE PRECO EM PERIODO MAIS RECENTE, FOI BUSCADO VERIFICAR SE HOUVE ALGUMA 
# INFLUENCIA EXTERNA, COMO O OCORRIDO NOS EUA EM 2008.

# A PARTIR DESTA HIPOTESE, MESMO COM O RANGE TEMPORAL PEQUENO, FOI VERIFICADO 
# A COTACAO DO DOLAR AUSTRALIANO EM EM RELACAO AO REAL BRASILEIRO (POIS O Preco
# ESTA EM REAIS E OS IMOVEIS EM ANALISE ESTAO NA AUSTRALIA).

# HISTORICO DA COTACAO - TAXA CAMBIAL
quantmod::getSymbols("AUDBRL=x")

# AJUSTANDO ARQUIVO
colnames(`AUDBRL=X`) <- c("x", "xx", "xxx", "fechamento", "xxxx", "xxxxx")
AUD <- data.frame(Data = index(`AUDBRL=X`),
                  `AUDBRL=X`, row.names = NULL)

# EXTRAINDO HISTORICO DE PRECO DO DOLAR AUSTRALIANO AO FINAL DO DIAL
AUD <- AUD %>%
  dplyr::select(Data, fechamento) %>% 
  na.omit() # REMOVENDO VALORES AUSENTES


# MERGE DE DATASETS
df_ts <- AUD %>% 
  dplyr::left_join(dfx, by = "Data") %>% 
  tidyr::drop_na(Preco)

# PLOT PARA VERIFICACAO DE TENDENCIAS
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
  geom_point(aes(y = log10(Preco), col = Distancia), alpha = .5, 
             show.legend = F) +
  facet_wrap(~Regiao, ncol = 1) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  theme_bw()

cowplot::plot_grid(p_aud, p_ts, align = 'hv', 
                   rel_heights = c(.3, 1), nrow = 2)

# NAO FOI VERIFICADO A INFLUENCIA DO MERCADO FINANCEIRO NO PRECO DOS IMOVEIS.
# PORTANTO, ESSA ABORDAGEM SERA DESCONTINUADA.

#
rm(df_ts, `AUDBRL=X`, AUD) # REMOCAO DE OBJETOS

# FILL NA ----
# PARA PROSSEGUIR PARA A MODELAGEM, E NECESSARIO O PREENCHIMENTO DOS VALORES
# AUSENTES E A REESTRUTURACAO DE VARIAVEIS QUE POSSUAM LEVELS (VARIAVEIS CATE-
# GORICAS).

fill_df <- dfx %>% 
  dplyr::select(-c(Latitude, Longitude, Quartos_aux, Banheiros, 
                   Garagem, Terreno, AnoConstrucao, AreaConstruida, 
                   id))

# VARIAVEIS PARA PREDICAO DE OUTRAS - FILL NA
vars <- c("Quartos", 'Tipo', 'Preco', 'Metodo', 'NumImoveis', 
          'Distrito', "Distancia", 'Regiao') 

# SELECAO PARA PREENCHIMENTO
fill <- c("Latitude", "Longitude", "Quartos_aux", 'Banheiros', 
          'Garagem', 'Terreno', 'AnoConstrucao', 'AreaConstruida')

# CRIANDO FUNCAO PARA PREENCHIMENTO DOS NA'S A PARTIR DE MODELO DE RANDOM FOREST
handling <- function(x) { 
  
  fill_aux <- fill_df %>% 
    dplyr::select(vars) %>% 
    dplyr::mutate(ext = as.numeric(dfx[,fill[x]])) %>% 
    dplyr::mutate_if(is.factor, as.numeric) 
  
  
  fill_imp <- missForest::missForest(fill_aux)
  fill <- fill_imp$ximp
  
}

# EXTRAINDO
k <- 1:8 %>% # 
  purrr::map(handling)

aux <- k %>% 
  purrr::map("ext")
names(aux) <- fill

aux <- aux %>% 
  tibble::as_tibble()

# CRIANDO O NOVO DATASET PREENCHIDO
fill <- fill_df %>% 
  dplyr::select(vars) %>% 
  dplyr::bind_cols(aux)

# CHECK NA'S
naniar::gg_miss_var(fill, show_pct = TRUE) +
  labs(x = "Variáveis", y = "% NA")

# DUMMIFICACAO DEVARIAVEIS CATEGORICAS UTLIZANDO A TECNICA DE ONE HOT ENCODING
# NOTA: AS VARIAVEIS COM ELEVADA QTD DE CATEGORIAS NAO PASSARAM POR ESSE 
# PROCESSO, POIS IMPLICARIA EM UM NUMERO EXACERBADO DE VARIAVEIS, AUMENTANDO A 
# COMPLEXIDADE DO PROBLEMA AQUI ABORDADO.

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
                   `Metodo)PI`, `Metodo)S`, `Metodo)SA`,  `Metodo)SP`, 
                   `Metodo)VB`,
                   `Regiao)Eastern Metropolitan`, `Regiao)Eastern Victoria`,
                   `Regiao)Northern Metropolitan`, `Regiao)Northern Victoria`,
                   `Regiao)South-Eastern Metropolitan`, 
                   `Regiao)Southern Metropolitan`,
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

# CHECK POSSIVEIS IMOVEIS REPETIDOS - DADO O PREENCHIMENTO DAS VARIAVEIS LATI-
# TUDE E LONGITUDE.
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
                Pres_NA = ifelse(is.na(Latitude) | is.na(Longitude), 
                                 "duplicada", "ponto_nao_duplicado")) %>% 
  dplyr::mutate(check = as.factor(check)) %>% 
  na.omit() %>% 
  dplyr::select(id, check, Pres_NA)
table(df_dup$Pres_NA)

# VERIFICACAO DE POSSIVEIS OUTLIERS ----
# COMO TEMOS DADOS MISTOS, A SOLUCAO ESCOLHIDA PARA VERIFICACAO DE OUTLIERS FOI
# A REALIZACAO DE AGRUPAMENTO UTILIZANDO UMA DERIVACAO DO KMEANS, E POSTERIOR-
# MENTE FOI ANALISADO OS CLUSTERS DE ACORDO COM A SUA GEOLOCALIZACAO.

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
# ESTIMATVA DO NUMERO OTIMO DE CLUSTERS A PARTIR DA VARIANCIA INTERNA
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
  stat_density_2d(aes(fill = ..level..), 
                  geom = "polygon", alpha = .1, color = NA) +
  scale_fill_gradient2("Imóveis", 
                       low = "white", mid = "yellow", high = "red", 
                       midpoint = 13)

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
plotly::ggplotly(plot_out) # GRAFICO INTERATIVO

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
             shape = 21, size = 5, stroke = 1, 
             col = "black", show.legend = F) +
  geom_point(aes(col = Preco, shape = cluster)) +
  facet_wrap(~cluster) +
  scale_color_gradient(low = "blue", high = "red", name = "Preço") +
  scale_shape_discrete(guide = FALSE) +
  scale_alpha_manual(values = c(0, 1)) +
  xlab("Longitude") + ylab("Latitude") +
  labs(subtitle = "Clusters") +
  theme_bw()

# LOCALIZACAO DO OUTLIER, VERIFICADA A PARTIR DO GRAFICO INTERATIVO
ann_text <- data.frame(Latitude = -37.5, Longitude = 144.59, lab = "Outlier",
                       cluster = factor(6, 
                                        levels = c("1", "2", "3", 
                                                   "4", "5", "6")))
p_out + 
  geom_text(data = ann_text, label = "Outlier")


df_aux_cluster %>% 
  ggplot() +
  geom_point(aes(x = Longitude, y = Latitude, 
                 col = cluster, shape = cluster)) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw()


# PREDICAO DE PRECOS ----
df_model <- df_aux %>% 
  dplyr::mutate_if(is.factor, as.numeric) %>% # AJUSTE PARA EMPREGO DOS MODELOS
  dplyr::filter(Latitude != -37.45392 & Longitude != 144.5886) # OUTLIERS 
# OUTLIERS BASEADO NA GEOLOCALIZACAO


# OS MODELOS PREDITIVOS EMPREGADOS SERAO MODELOS BASEADOS EM ARVORES DE DECISAO,
# DEVIDO A ROBUSTEZ, ALTA CAPACIDADE DE PREDICAO E O TIPO DE DADO (CATEGORICO E
# CONTINUO). OS ALGORITMOS TESTADOS SAO: 
# RANDOM FOREST, xgbTree E xgbLinear.
# OS DOIS ULTIMOS ALGORITMOS SAO DERIVADOS DO XGBOOST.

# SET DE TREINAMENTO E VALIDACAO
# 75% DOS DADOS SERAO UTILIZADOS NO TREINAMENTO
# 25% NA FASE DE TESTE

{
  set.seed (1) 
  index <- caret::createDataPartition(df_model$Preco, p = 0.75, list = FALSE) 
  
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

# PARA O MODELO DE REGRESSAO, SERAO ANALISADAS AS METRIAS DE RMSE (ERRO 
# QUADRATICO MEDIO), MAE (ERRO MEDIO ABSOLUTO) E Rˆ2 (FIT MODEL).

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

# PODEMOS VISUALIZAR NO GRAFICO QUE O MODELO BASEADO NO ALGORITMO DE RANDOM 
# FOREST NOS RETORNA A MELHOR PERFORMANCE.
# COMO ESTAMOS REALIZANDO UMA REGRECAO, Preco E UMA VARIAVEL CONTINUA, NAO TEMOS
# A MATRIZ DE CONFUSAO, QUE E GERADA EM PROBLEMAS DE CLASIFICACAO.


# ANALISE DE PERFORMANCE DO MODELO
predF <- caretEnsemble::caretEnsemble(model_listF)
final_model_stack <- caret::postResample(predict(predF, test), 
                    obs  = test$Preco)

dotplot(final_model_stack, metric = "RMSE")

# APESAR DOS EXCELENTES VALORES DE PERFORMACE (PROXIMOS A ZERO), VALE A RESSALVA 
# DA NECESSIDADE DE TRANSFORMACAO DE VOLTA (BACK TRANSFORMAITON), DADO A 
# TRANSFORMACAO INICIAL REALIZADA PARA A VARIÁVEL Preco.

# O MODELO STACK (EMPILHAMENTO / SUPERLEARNING) APRESENTA EXCELENTE PERFORMANCE, 
# MAS ALTO CUSTO COMPUTACIONAL (TEMPO DE PROCESSAMENTO E CONSUMO DE MEMORIA).

# LOGO, O MODELO ESCOLHIDO PARA A REGRESSAO DOS PRECOS DOS IMOVEIS NA CIDADE
# DE MELBOURNE E O MODELO UTILIZANDO RANDOM FOREST.

# PLOT COM O ANTILOG DOS VALORES
plot(10^(predict(model_listF$rf, newdata = test)), 10^(test$Preco))

# PLOT DE RESIDUO - MODELO RANDOM FORESTE
plot(10^(test$Preco) - 10^(predict(model_listF$rf, newdata = test)))

# E POSSIVEL NOTAR QUE O MODELO APRESENTA BOA TENDENCIA DOS VALORES A SEREM
# PREDITOS. UM BOM VALOR DE Rˆ2 (PROXIMO A 1). 
# APARENTEMENTE TEMOS ALGUNS OUTROS OUTLIERS, CONTUDO NAO INFLUENCIAM TAO NEGA-
# TIVAMENTE A TENDENCIA DO MODELO/RESIDUO.

# PERFORMANCE - MODELO RANDOM FOREST - COM O ANTILOG
caret::postResample(10^(predict(model_listF$rf, newdata = test)),
                    obs  = 10^test$Preco)


# COM OS DADOS EM ESCALA REAL, PERCEBEMOS QUE O VALOR DO RMSE PODE SER CONSI-
# DERADO ALTO. POREM, O MODELO CONTINUA SENDO BEM REPRESENTATIVO, COM BOA 
# TENDENCIA, VIES. 

# A REDUCAO DO RMSE PODE SER FEITA ATRAVES DE SELECAO DE VARIAVEIS, COMO O 
# AUXILIO DO MODELO LASSO DE REGRESSAO, OU UM STEPWISE, PRINCIPALPENTE EM RAZAO 
# DA ELEVADA QUANTIDADE DE NA'S PREENCHIDA.

# PARA IMPLEMENTACAO EM PRODUCAO DESTE MODELO, SUGIRO QUE SEJA UTILIZADO APENAS
# O PACOTE caret PARA CONFIGURAR A RANDOM FOREST, POIS JA SABEMOS QUE TEREMOS 
# BONS RESULTADOS, MELHOR PERFORMANCE QUE OS DERIVADOS DE XGBOOST AQUI
# TREINADOS, MENOR COMPLEXIDADE E MENOR CUSTO COMPUTACIONAL, DADO A AUSEN-
# DO BOOST E DO GRADIENTE.

# APESAR DA EXTENSIVA ANALISE EXPLORATORIA, INCLUSIVE COM METODO NAO SUPERVISIO-
# NADO, NAO FOI POSSIVEL VERIFICAR DE MANEIRA NITIDA UM PADRAO DE Preco. COMO
# SUGESTAO, APESAR DO GRANDE CUSTO COMPUTACIONAL E AUMENTO DA COMPLEXIDADE DOS
# MODELOS GERADOS, TEM-SE A FUTURA POSSIBILIDADE DE EMPREGO DE METODO HIBRIDO,
# COMO O RF + IDW, PRINCIPALMENTE PELA POSSIBILIDADE DE MODELO INCREMENTAL DADO
# OS RESIDUOS DO MODELO PREDITOR.

# A NIVEL DE SUGESTAO EMPRESARIAL, A PREDICAO DE CLASSIFICACAO PARA O Metodo 
# "VB" (METODO DE VENDA DO IMOVEL) PODE SER IDEALIZADO COMO ETAPA DE PLANEJA-
# MENTO FINANCEIRO PARA AS CORRETORAS DE IMOVEIS.



# BRENNER BIASI SOUZA SILVA
# ESTE ARQUIVO TAMBEM ESTA DISPONIVEL EM:
# https://github.com/bbiasi/randon

