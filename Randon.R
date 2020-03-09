
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
if(!require("pacman")) install.packages("pacman")

pacman::p_load(dplyr, tidyr, naniar, ggplot2, lubridate, stringr, 
               dummies, missForest, tibble, quantmod, caret, Metrics, 
               reshape2, spacetime)

if(!require("gstat")) install.packages("gstat", dependencies = T) ; library(gstat)
if(!require("sp")) install.packages("sp", dependencies = T) ; library(sp)
if(!require("raster")) install.packages("raster", dependencies = T) ; library(raster)
if(!require("spm")) install.packages("spm", dependencies = c("Imports", "Suggests")) ; library(spm)


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
                NumImoveis = as.factor(NumImoveis)) %>%
  dplyr::filter(Regiao | Distrito | NumImoveis != "#N/A") %>% # REMOVENDO ABERRACAO
  dplyr::mutate(NumImoveis = as.numeric(as.character(NumImoveis))) %>%  
  tidyr::drop_na(Preco) 

naniar::gg_miss_var(dfx, show_pct = TRUE) +
  labs(x = "Variáveis", y = "% NA")

# FILL NA ----
# REALIZANDO DUMMIFICACAO
{
  Bairro   <- tibble::as.tibble(dummies::dummy(dfx$Bairro))
  Tipo     <- tibble::as.tibble(dummies::dummy(dfx$Tipo))
  Metodo   <- tibble::as.tibble(dummies::dummy(dfx$Metodo))
  Corretor <- tibble::as.tibble(dummies::dummy(dfx$Corretor))
  Distrito <- tibble::as.tibble(dummies::dummy(dfx$Distrito))
  Regiao   <- tibble::as.tibble(dummies::dummy(dfx$Regiao))
  }

dfx <- dfx %>%
  dplyr::select(-c(Bairro, Tipo, Metodo, Corretor, Distrito, Regiao, CEP, Latitudo)) %>% 
  dplyr::bind_cols(list(Bairro, Tipo, Metodo, Corretor, Distrito, Regiao))
remove(Bairro, Distrito, Tipo, Metodo, Corretor, Regiao)

fill_NA <- dfx %>% 
  dplyr::select(c(Latitude, Longitude, Quartos_aux, Banheiros, 
                  Garagem, Terreno, AnoConstrucao, AreaConstruida, id))

fill_df <- dfx %>% 
  dplyr::select(-c(Latitude, Longitude, Quartos_aux, Banheiros, 
                   Garagem, Terreno, AnoConstrucao, AreaConstruida, id))

# PREENCHENDO NA - LATITUDE
fill_lat <- fill_df %>% 
  dplyr::mutate(Latitude = dfx$Latitude)
fill_lat_imp <- missForest::missForest(fill_lat)

# naniar::gg_miss_var(fill_lat, show_pct = TRUE) +
#   labs(x = "Variáveis", y = "% NA")


iris.imp <- missForest(iris.mis)
iris.imp$ximp

iris.imp$OOBerror 
# NRMSE é um erro quadrático médio normalizado. 
#É usado para representar erros derivados da imputação de valores contínuos.
# O PFC (proporção de classificados falsamente) 

iris.err <- mixError(iris.imp$ximp, iris.mis, iris)

iris.err[1]
paste("Isso sugere que variáveis categóricas são imputadas com", 
      round((iris.err[2])*100),
      "% de erro e variáveis contínuas são imputadas com",
      round((iris.err[1])*100),
      "% de erro.")



# OBTENDO COTACAO HISTORICA DO DOLAR AUSTRALIANO ----
quantmod::getSymbols("AUDBRL=x")
#
# colnames(`USDBRL=X`) <- c("x", "xx", "xxx", "fechamento", "xxxx", "xxxxx")
# dolar <- data.frame(Data = index(`USDBRL=X`), 
#                     `USDBRL=X`, row.names = NULL)
# dolar <- dolar %>% 
#   dplyr::select(Data, fechamento)

# VISUALIZANDO CASAS NO MAPA ----
# LEAFLET ----
# install.packages("leaflet", dependencies = T)
# library(leaflet)
# 
# m <- leaflet() %>%
#   addTiles() %>%  #
#   addMarkers(lng = c(teste$Longitude), lat= c(teste$Latitude))
# m
# remove(m)

# IDW - PRED ----
teste <- dfx %>% 
  dplyr::select(Preco, Latitude, Longitude) %>% 
  na.omit()

set.seed(1)
index <- caret::createDataPartition(teste$Preco, p = 0.7, list = FALSE) 
train <- teste[index, ]
test  <- teste[-index, ]

sp::coordinates(train) <- ~Longitude+Latitude
sp::proj4string(train) <- sp::CRS("+proj=longlat +datum=WGS84")

testx <- test %>% 
  dplyr::select(-Preco)
sp::coordinates(testx)  <- ~Longitude+Latitude
sp::proj4string(testx)  <- sp::CRS("+proj=longlat +datum=WGS84")

oo <- gstat::idw(formula = Preco ~ 1, 
                 locations = train, newdata = testx, 
                 idp = 2.0 # PADRAO
                 )
result <- data.frame(res = oo@data$var1.pred)

# METRICAS DE DESEMPENHO E PLOT DO IDW COM APENAS LATITUDE E LONGITUDE
Metrics::mae(test$Preco,  result$res)
Metrics::rmse(test$Preco, result$res)

xx <- data.frame(real = test$Preco, 
                 predito = result$res)
ggplot2::ggplot(xx) +
  geom_point(aes(x = real, y = predito), alpha = 0.5)

# MAPA DE CALOR UTILIZANDO IDW ----
teste_mapa <- teste
sp::coordinates(teste_mapa) <- ~Longitude+Latitude
sp::proj4string(teste_mapa) <- sp::CRS("+proj=longlat +datum=WGS84")

# CRIANDO GRID P PLOT
grd              <- as.data.frame(spsample(teste_mapa, "regular", n = 50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  
fullgrid(grd)    <- TRUE  

sp::proj4string(grd) <- sp::proj4string(teste_mapa)
P.idw <- gstat::idw(Preco ~ 1, teste_mapa, newdata = grd, idp = 2.0)
r     <- raster::raster(P.idw)
plot(r)

# RF + IDW - teste ----
# DEMORA MUITO
# COMO FUNCIONA O PREIDICT DISTO?
# rfidwcv1 <- spm::rfidwcv(teste[, c(2, 3)], # LONGLAT
#                          teste[, c(2, 3)], # VARIAVEIS PREDITORAS
#                          teste[, 1], # Y,
#                          ntree = 1000,
#                          predacc = "ALL")
# 
# rfidwcv1






