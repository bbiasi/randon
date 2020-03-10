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