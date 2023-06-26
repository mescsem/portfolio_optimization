#+eval=FALSE
rm(list=ls()) #Limpiar ambiente
#LIBRERiAS############################################################################
library(readxl)                 #Leer excel
library(dplyr)                  #Manipulacion de datos
library(tidyr)                  #Manipulacion de datos
library(xts)                    #Manipulacion de datos (fecha)
library(PerformanceAnalytics)   #Calcular rendimientos mensuales, backtesting
library(normtest)               #Pruebas de bondad de ajuste
library(nortest)                #Pruebas de bondad de ajuste
library(moments)                #Pruebas de bondad de ajuste
library(ggplot2)                #Visualizacion
library(waffle)                 #Visualizacion (waffle chart)
library(ggcorrplot)             #Visualizacion (corrplot)
library(gridExtra)              #Crear grid simulacion basada en copulas
library(viridis)                #Paleta de colores
library(Redmonder)              #Paleta de colores
library(rcartocolor)            #Paleta de colores
library(scales)                 #Para cambiar escala de graficos a %
library(tseries)                #Markowitz
library(NMOF)                   #Markowitz (minimo riesgo)
library(Matrix)                 #Funcion nearPD
library(matrixStats)            #Funcion colSds
library(copula)                 #Generacion de copulas
library(actuar)                 #Distribucion log-logistica
library(jmuOutlier)             #Distribucion laplace
library(NlcOptim)               #Optimizacion no lineal

#FUNCIONES############################################################################
#Filtrar una base con la fecha y la variable deseada (P_: Precio, V_: Volumen)
base_id <- function(base, id_var) {
  base %>% 
    dplyr::select(date, starts_with(id_var))
}  

#Convertir estructura de dataframe a xts
dataframe_a_xts <- function(dataframe) {
  xts(dataframe[,-1], order.by=dataframe[,1])
}

#Convertir estructura de xts a dataframe
xts_a_dataframe <- function(xts) {
  data.frame(date=index(xts), coredata(xts))
}

#Funcion transformar una base a un formato "tidy"
tidy <- function(base,variable) {
  base %>%
    gather(key = "nemonico", value = !!variable, -date, factor_key = TRUE, na.rm = TRUE) %>%
    separate(col=nemonico,into=c("var","nemonico")) %>%
    dplyr::select(-var)
}  

#Renombrar todas las variables
renombrar <- function(base,id_var) {
  colnames(base) <- sub(id_var, "", colnames(base))
}

#Funcion para evaluar texto a formula
evaluar <- function(formula) {
  eval(parse(text=formula))
}  

#Funcion para hacer un redondeo inteligente (para que la suma de % en el waffle chart de 100%)
smart_round <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

#Funcion para calcular VaR
var <- function(data, alpha) {
  quantile(data,1-alpha)
}

#Funcion para calcular CVaR
cvar <- function(data, alpha) {
  mean((data)[data<  quantile(data,1-alpha)])
}

#Funcion para el ratio de Sharpe
sharpe <- function(rentabilidad, riesgo, rf) {
  (rentabilidad-rf)/riesgo
}

#Funcion para el ratio de Sortino
sortino <- function(rentabilidad, riesgoasim, rf) {
  (rentabilidad-rf)/riesgoasim
}

#A.PREPROCESAMIENTO DE LA INFORMACIoN, CREACIoN DE BASES Y CaLCULO DE INDICADORES############################
#Fijar el directorio de trabajo
setwd("C:\\Users\\user\\Desktop\\TESIS\\Tesis")

#Fijar parametros de horizonte para consolidar informacion
fecha_ini <- '2007-07-30'
fecha_fin <- '2020-07-01'

#Leer archivo con data de las acciones y guardarlo como listas, leer tabla resumen
nomarchivodata <- "Precio acciones - Mensual.xlsx"
nomarchivoresumen <- "Tabla resumen.xlsx"
nemonico <- excel_sheets(path = nomarchivodata)
acciones <- lapply(excel_sheets(nomarchivodata), read_excel, path = nomarchivodata)
tablaresumen <- read_excel(nomarchivoresumen, sheet = 1, col_names=TRUE)
nacciones <- length(nemonico)

#Cambiar nombre de las variables incorporandole un identificador con nemonico
#P:precio, V:volumen, SMAVG(15):media movil simple del volumen de los ultimos 15 meses
for (i in c(1:nacciones)) {
  names(acciones[[i]]) <- c("date", paste("P",nemonico[i], sep="_"), paste("V",nemonico[i], sep="_"),paste("SMAVG(15)",nemonico[i], sep="_"))
}

#Crear un consolidado con las 30 acciones analizadas
consolidado <- Reduce(function(x, y) merge(x, y, by="date"), acciones)

#Crear un consolidado para el precio
bdprecio <- base_id(consolidado,"P_")
bdprecio <- bdprecio%>% filter(date >=fecha_ini & date<=fecha_fin)

#Crear base (xts y dataframe) de los rendimientos (aritmeticos y logaritmicos) de las acciones
bdprecioxts <- dataframe_a_xts(bdprecio)
bdrendxts <- Return.calculate(bdprecioxts, method="discrete")
bdrendlogxts <- Return.calculate(bdprecioxts, method="log")
bdrend<- xts_a_dataframe(bdrendxts)
bdrendlog<- xts_a_dataframe(bdrendlogxts)

#Crear bases con formato "tidy"
tidy_precio <- tidy(bdprecio,"precio") 
tidy_rend <- tidy(bdrend,"rendimiento") 
tidy_rendlog <- tidy(bdrendlog,"rendimiento_log")
bases <- list(tidy_precio, tidy_rend, tidy_rendlog)
tidy_consolidado <- Reduce(function(x, y) merge(x, y, by=c("date","nemonico"),all.x = TRUE), bases)

#Renombrar variables sin el prefijo identificador
colnames(bdprecio)<- renombrar(bdprecio,"P_")
colnames(bdprecioxts)<- renombrar(bdprecioxts,"P_")
colnames(bdrendxts)<- renombrar(bdrendxts,"P_")
colnames(bdrendlogxts)<- renombrar(bdrendlogxts,"P_")
colnames(bdrend)<- renombrar(bdrend,"P_")
colnames(bdrendlog)<- renombrar(bdrendlog,"P_")

#Crear consolidado con informacion sumarizada y ordenada segun ratio de Sortino (Rf=0) (a priori mas atractivo)
fecha_fin_indicadores <- '2017-06-30'
tidy_consolidado_ind <- tidy_consolidado %>% filter(date<=fecha_fin_indicadores)

resumenconsolidado <- tablaresumen %>%
  inner_join(tidy_consolidado_ind %>%
               filter(!is.na(rendimiento)) %>%
               group_by(nemonico) %>%
               summarize(media_mes=mean(rendimiento),
                         media_anio=media_mes*12,
                         media_geom_mes=mean.geometric(rendimiento),
                         media_geom_anio=(1+media_geom_mes)^12-1,
                         desv_mes=sd(rendimiento),
                         desv_anio=desv_mes*12^0.5,
                         semidesv_mes=sd(rendimiento[rendimiento<0]),
                         semidesv_anio=semidesv_mes*12^0.5,
                         sesgo=skewness(rendimiento),
                         curtosis=PerformanceAnalytics::kurtosis(rendimiento, method="moment"),
                         sharpe=media_anio/desv_anio,
                         sortino=media_anio/semidesv_anio,
                         VaR95=quantile(rendimiento, 0.05)*12^0.5,
                         VaR99=quantile(rendimiento, 0.01)*12^0.5,
                         CVaR95=mean(rendimiento[rendimiento < quantile(rendimiento, 0.05)])*12^0.5,
                         CVaR99=mean(rendimiento[rendimiento < quantile(rendimiento, 0.01)])*12^0.5,
                         prop_perdida=mean(rendimiento<0)
                         ), by = c("Nemonico" = "nemonico")) %>%
  arrange(desc(sortino))

#Se crea un orden de los acciones (de activo mas atractivo a menos atractivo)
orden_nemonico <- unlist(resumenconsolidado[1]) 

#Ordenar por accion mas atractiva y separar en rendimientos in-sample y out-of-sample
rend_copia <- xts_a_dataframe(bdrendxts[,orden_nemonico])
rend_copia <- rend_copia %>% filter(date<=fecha_fin_indicadores)
rend_copia <- dataframe_a_xts(rend_copia)[-1,]

rend_outofsample <- xts_a_dataframe(bdrendxts[,orden_nemonico])
rend_outofsample <- rend_outofsample %>% filter(date>fecha_fin_indicadores)
rend_outofsample <- dataframe_a_xts(rend_outofsample)

#Calcular rendimientos de benchmark S&P/BVL
nomarchivospbvl <- "S&P BVL Peru General Index (PEN).xlsx"
spbvl <- dataframe_a_xts(data.frame(read_excel(path = nomarchivospbvl)))
spbvl <- xts_a_dataframe(Return.calculate(spbvl, method="discrete"))
spbvl_in_sample <- spbvl%>% filter(date >fecha_ini & date<fecha_fin_indicadores)
spbvl_in_sample <- dataframe_a_xts(spbvl_in_sample)[-1,]
colnames(spbvl_in_sample) <- "S&P/BVL" 
spbvl <- spbvl%>% filter(date>fecha_fin_indicadores & date <=fecha_fin)
spbvl <- dataframe_a_xts(spbvl)
colnames(spbvl) <- "S&P/BVL" 

#Borrar elementos del environment que ya no se usaran
rm(acciones, bases, consolidado, tablaresumen, tidy_precio, tidy_rendlog, tidy_rend,
   nemonico,i,nomarchivodata, nomarchivoresumen, nomarchivospbvl)

#B.MARKOWITZ#########################################################################
#B.1 MARKOWITZ-IN-SAMPLE#############################################################
#Crear vectores y matrices vacios almacenar informacion de la optimizacion media-varianza de Markowitz
media_markowitz_anual <- sd_markowitz_anual <- N_markowitz <- N_acc_port <- consolidado_pesos_markowitz <- NULL 

#Fijar numero de portafolios a optimizar
n_markowitz <- 100

start_time <- Sys.time()
#Realizar iteraciones aumentando el # de acciones disponibles (segun indicador Sortino)
for(k in 2:nacciones) {
  #Aplicacion del modelo de Markowitz para encontrar la rentabilidad del portafolio optimo de minimo riesgo
  rend <- rend_copia[,1:k]
  pesos_opt_min_riesgo_markowitz <- minvar(cov(rend), wmin = 0, wmax = 1, method = "qp")
  rend_opt_min_riesgo_markowitz <- (pesos_opt_min_riesgo_markowitz%*%colMeans(rend))[1,1]
  
  #El portafolio optimo de mayor rentabilidad sera una inversion del 100% en el activo con mayor rendimiento
  rend_opt_max_rend_markowitz <- max(colMeans(rend))
  pos_max <- which.max(colMeans(rend)) #Confirmamos que la 1era accion genera el portafolio mas rentable
  
  #Crear un "grid" para que el algoritmo itere y cree "n_markowitz" portafolios optimos sujetos a una rentabilidad minima
  #entre el portafolio de minimo riesgo global y el de maximo rendimiento global
  grid_markowitz <- seq(rend_opt_min_riesgo_markowitz, rend_opt_max_rend_markowitz, length.out = n_markowitz)  
  grid_markowitz[n_markowitz] <-  grid_markowitz[n_markowitz]-10^-8
  
  #Crear vectores para almacenar las medias y desviaciones estandar de los portafolios optimos
  media_markowitz <- sd_markowitz <- rep(NA, n_markowitz)
  pesos_markowitz <- matrix(NA, n_markowitz, nacciones)

  #Realizar la optimizacion media-varianza de Markowitz y almacenar informacion de media, sd y pesos
  for(i in 1:n_markowitz) {
    opt <- portfolio.optim(x = rend , pm = grid_markowitz[i])
    media_markowitz[i] <- opt$pm
    sd_markowitz[i] <- opt$ps
    pesos_markowitz[i,] <- append(opt$pw,rep(0,nacciones-k))
  }
  
  #Consolidar rentabilidad (media) anualizada y riesgo (sd) anualizado, y crear un consolidado de pesos
  media_markowitz_anual <- append(media_markowitz_anual, media_markowitz*12)
  sd_markowitz_anual <- append(sd_markowitz_anual, sd_markowitz*12^0.5)
  N_markowitz <- append(N_markowitz, seq(1,n_markowitz))
  N_acc_port <- append(N_acc_port, rep(k,n_markowitz))
  consolidado_pesos_markowitz <- rbind(consolidado_pesos_markowitz,pesos_markowitz)
}
end_time <- Sys.time()
tiempo_markowitz <- end_time - start_time

#Dar formato a los nombres de las variables, crear un consolido final y una version "tidy" del consolidado final
colnames(consolidado_pesos_markowitz) <- colnames(rend_copia)
consolidado_markowitz <- data.frame(cbind(N_markowitz,N_acc_port,media_markowitz_anual,sd_markowitz_anual,consolidado_pesos_markowitz))
names(consolidado_markowitz)[3:4] <- c("Media","Desviacion_Estandar")
markowitz.tidy <- consolidado_markowitz %>%
  gather(key="Nemonico", value="Peso", c(-N_markowitz,-N_acc_port,-Media,-Desviacion_Estandar),factor_key = TRUE, na.rm = TRUE)

#Definir escala de colores
colores <- c("#5f4690","#1d6996","#38a6a5","#0f8554","#000000","#73af48","#edad08","#000000","#e17c05","#000000","#cc503e","#94346e","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#6f4070","#000000","#000000","#000000","#000000","#000000","#666666","#000000","#000000")
names(colores) <- colnames(consolidado_pesos_markowitz)
colScale <- scale_fill_manual(name = "Nemonico",values = colores)
colScale2 <- scale_colour_manual(name = "Nemonico",values = colores)

#Plot de composicion optima segun rentabilidad (media)
plotlist_markowitz = NULL
for(k in 2:nacciones) {
  grafico <- ggplot(markowitz.tidy%>% filter(N_acc_port==k), aes(x=Media,y=Peso)) +
    geom_area(aes(fill=Nemonico),position = position_stack(reverse = T))+
    ggtitle(paste("n=",k,sep="")) +
    theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(limits=c(0.075,0.195),breaks=seq(0.08,0.18,0.02), labels = scales::percent_format(accuracy = 1))+
    theme(legend.position = "none") +
    colScale
  plotlist_markowitz[[k]] = grafico   
}

grid.arrange(grobs=plotlist_markowitz[2:9],ncol=4)
grid.arrange(grobs=plotlist_markowitz[10:17],ncol=4)
grid.arrange(grobs=plotlist_markowitz[18:25],ncol=4)
grid.arrange(grobs=plotlist_markowitz[26:30],ncol=4)

#Plot de la composicion del portafolo cuando hay 30 acciones
plotlist_markowitz[[30]]+
  ggtitle("Composicion de portafolios eficientes, segun nivel de rentabilidad - Markowitz")+
  theme(legend.position = "right")

#Plot de prontera eficiente segun num. de activos analizados
ggplot(markowitz.tidy, aes(x=Desviacion_Estandar,y=Media)) +
  geom_line(aes(color=factor(N_acc_port)),size=1.25) + 
  ggtitle("Frontera eficiente segun Num. de acciones disponibles - Markowitz") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
  scale_y_continuous(limits=c(0.06,0.20),breaks=seq(0.06,0.20,0.02), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(limits=c(0.12,0.30),breaks=seq(0.12,0.30,0.02), labels = scales::percent_format(accuracy = 1)) +
  labs(color = "Num. acciones disponibles") + xlab("Desviacion Estandar") + ylab("Media")+
  scale_color_viridis(discrete=TRUE, direction=-1) 

#Plot de prontera eficiente (N=30) + acciones
ggplot(resumenconsolidado, aes(desv_anio,media_anio, label=Nemonico, color = Nemonico)) +
  labs(title= "Frontera eficiente y acciones que lo conforman - Markowitz", x="Desviacion Estandar", y="Media")+
  geom_text(aes(fontface=2))+
  scale_color_manual(values=replace(colores, colores=="#000000", "#ffffff"))+
  scale_y_continuous(limits=c(-0.08,0.20),breaks=seq(-0.08,0.20,0.04), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(limits=c(0.12,0.5),breaks=seq(0.12,0.50,0.04), labels = scales::percent_format(accuracy = 1)) +
  geom_line(data=markowitz.tidy %>% filter(N_acc_port==nacciones), size=1.25, aes(x=Desviacion_Estandar,y=Media), color="#000000")

#Plot del posible rango de desviacion estandar del portafolio segun numero de activos
min_max_markowitz <- markowitz.tidy %>%
  group_by(N_acc_port) %>% 
  summarize(min_riesgo = min(Desviacion_Estandar), max_riesgo=max(Desviacion_Estandar))

ggplot(markowitz.tidy, aes(x=factor(N_acc_port),y=Desviacion_Estandar)) +
  geom_line(aes(color=factor(N_acc_port)),size=1) + 
  scale_color_viridis(discrete=TRUE, direction=-1) +
  ggtitle("Efecto de la diversificacion: Nivel de riesgo segun # de acciones disponibles - Markowitz") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
  scale_y_continuous(limits=c(0.12,0.30),breaks=seq(0.10,0.30,0.04), labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(name="Num. acciones disponibles", breaks=seq(2,30,2)) +
  geom_line(data =min_max_markowitz, aes(x=factor(N_acc_port),y=min_riesgo, group = 1), size=1.5,color="#F8766D") +
  geom_point(data=min_max_markowitz, aes(x=factor(N_acc_port),y=max_riesgo, group = 1)) +
  geom_point(data =min_max_markowitz, aes(x=factor(N_acc_port),y=min_riesgo, group = 1)) +
  labs(color = "Num. acciones disponibles") + ylab("Desviacion Estandar")

markowitz_in_sample <- (consolidado_markowitz%>% filter(N_acc_port==nacciones))[,-2]

#Calcular indicadores del portafolio
markowitz_in_sample_enc <- 1/rowSums(markowitz_in_sample[,4:(4+nacciones-1)]^2)
markowitz_in_sample <- cbind(markowitz_in_sample, markowitz_in_sample_enc)
colnames(markowitz_in_sample)[ncol(markowitz_in_sample)] <-  "ENC"
markowitz_in_sample$Sharpe <- markowitz_in_sample$Media / markowitz_in_sample$Desviacion_Estandar

max_sharpe_markowitz_in_sample <- max(markowitz_in_sample$Media/markowitz_in_sample$Desviacion_Estandar)
pos_max_sharpe_markowitz_in_sample <- which(markowitz_in_sample$Media/markowitz_in_sample$Desviacion_Estandar==max_sharpe_markowitz_in_sample)

pesos_markowitz_in_sample_min_std <- as.vector(t(markowitz_in_sample[1,4:(4+nacciones-1)]))
pesos_markowitz_in_sample_max_sharpe <- as.vector(t(markowitz_in_sample[pos_max_sharpe_markowitz_in_sample,4:(4+nacciones-1)]))
pesos_markowitz_in_sample_max_mean <- c(1,rep(0,nacciones-1))
pesos_markowitz_in_sample_naive <- c(rep((1/nacciones),nacciones))

#Resumen portafolios optimos
opt_media <- markowitz_in_sample[n_markowitz,]
opt_sd <- markowitz_in_sample[1,]
opt_sharpe <- markowitz_in_sample[pos_max_sharpe_markowitz_in_sample,]
  
opt_media$Indicador <- "MAX MEDIA"
opt_sd$Indicador <- "MIN SD"
opt_sharpe$Indicador <- "MAX SHARPE"

indicadores_opt_markowitz <- data.frame(rbind(opt_media, opt_sd, opt_sharpe))
indicadores_opt_markowitz.tidy <- data.frame(indicadores_opt_markowitz[,-c(1,2,3,34,35)]) %>%
  gather(key="Nemonico", value="Peso", c(-Indicador),factor_key = TRUE, na.rm = TRUE)

indicadores_opt_markowitz.tidy$Indicador <- factor(indicadores_opt_markowitz.tidy$Indicador, levels=c("MAX MEDIA","MIN SD", "MAX SHARPE"))

#Plot de composicion de portafolios optimos, segun indicadores propuestos
ggplot(indicadores_opt_markowitz.tidy, aes(x=factor(Indicador),y=Peso)) +
  geom_col(aes(fill=factor(Nemonico)),position = position_stack(reverse = T))+
  ggtitle(paste("n=",k,sep="")) +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "right") +
  colScale+
  ggtitle("Composicion de portafolios eficientes, segun indicador - Markowitz")+
  xlab("Indicador") 

#B.2 BACKTESTING#############################################################
#Naive
port_insample_markowitz_bh_naive <- Return.portfolio(rend_copia, weights = pesos_markowitz_in_sample_naive, wealth.index=TRUE, verbose = TRUE)
port_insample_markowitz_rebal_mes_naive <- Return.portfolio(rend_copia, weights = pesos_markowitz_in_sample_naive, wealth.index=TRUE, rebalance_on = "months", verbose = TRUE)
port_insample_markowitz_rebal_trim_naive <- Return.portfolio(rend_copia, weights = pesos_markowitz_in_sample_naive, wealth.index=TRUE, rebalance_on = "quarters", verbose = TRUE)

port_outofsample_markowitz_bh_naive <- Return.portfolio(rend_outofsample, weights = pesos_markowitz_in_sample_naive, wealth.index=TRUE,verbose = TRUE)
port_outofsample_markowitz_rebal_mes_naive <- Return.portfolio(rend_outofsample, weights = pesos_markowitz_in_sample_naive, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_markowitz_rebal_trim_naive <- Return.portfolio(rend_outofsample, weights = pesos_markowitz_in_sample_naive, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#Max mean
port_insample_markowitz_bh_max_mean <- Return.portfolio(rend_copia, weights = pesos_markowitz_in_sample_max_mean, wealth.index=TRUE,verbose=TRUE)
port_insample_markowitz_rebal_mes_max_mean <- Return.portfolio(rend_copia, weights = pesos_markowitz_in_sample_max_mean, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_insample_markowitz_rebal_trim_max_mean <- Return.portfolio(rend_copia, weights = pesos_markowitz_in_sample_max_mean, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

port_outofsample_markowitz_bh_max_mean <- Return.portfolio(rend_outofsample, weights = pesos_markowitz_in_sample_max_mean, wealth.index=TRUE, verbose=TRUE)
port_outofsample_markowitz_rebal_mes_max_mean <- Return.portfolio(rend_outofsample, weights = pesos_markowitz_in_sample_max_mean, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_markowitz_rebal_trim_max_mean <- Return.portfolio(rend_outofsample, weights = pesos_markowitz_in_sample_max_mean, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#Min Std
port_insample_markowitz_bh_min_std <- Return.portfolio(rend_copia, weights = pesos_markowitz_in_sample_min_std, wealth.index=TRUE,verbose = TRUE)
port_insample_markowitz_rebal_mes_min_std <- Return.portfolio(rend_copia, weights = pesos_markowitz_in_sample_min_std, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_insample_markowitz_rebal_trim_min_std <- Return.portfolio(rend_copia, weights = pesos_markowitz_in_sample_min_std, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

port_outofsample_markowitz_bh_min_std <- Return.portfolio(rend_outofsample, weights = pesos_markowitz_in_sample_min_std, wealth.index=TRUE,verbose = TRUE)
port_outofsample_markowitz_rebal_mes_min_std <- Return.portfolio(rend_outofsample, weights = pesos_markowitz_in_sample_min_std, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_markowitz_rebal_trim_min_std <- Return.portfolio(rend_outofsample, weights = pesos_markowitz_in_sample_min_std, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#Max Sharpe
port_insample_markowitz_bh_max_sharpe <- Return.portfolio(rend_copia, weights = pesos_markowitz_in_sample_max_sharpe, wealth.index=TRUE, verbose=TRUE)
port_insample_markowitz_rebal_mes_max_sharpe <- Return.portfolio(rend_copia, weights = pesos_markowitz_in_sample_max_sharpe, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_insample_markowitz_rebal_trim_max_sharpe <- Return.portfolio(rend_copia, weights = pesos_markowitz_in_sample_max_sharpe, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

port_outofsample_markowitz_bh_max_sharpe <- Return.portfolio(rend_outofsample, weights = pesos_markowitz_in_sample_max_sharpe, wealth.index=TRUE, verbose=TRUE)
port_outofsample_markowitz_rebal_mes_max_sharpe <- Return.portfolio(rend_outofsample, weights = pesos_markowitz_in_sample_max_sharpe, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_markowitz_rebal_trim_max_sharpe <- Return.portfolio(rend_outofsample, weights = pesos_markowitz_in_sample_max_sharpe, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#S&P/BVL
port_insample_spbvl<- Return.portfolio(spbvl_in_sample, weights = c(1), wealth.index=TRUE, verbose = TRUE)
port_outofsample_spbvl <- Return.portfolio(spbvl, weights = c(1), wealth.index=TRUE, verbose = TRUE)

#Resultados Anualizados - In sample
resumen_backtesting_markowitz_in_sample <- cbind(
  #Naive
table.AnnualizedReturns(port_insample_markowitz_bh_naive$returns),
table.AnnualizedReturns(port_insample_markowitz_rebal_mes_naive$returns),
table.AnnualizedReturns(port_insample_markowitz_rebal_trim_naive$returns),

  #Max Mean
table.AnnualizedReturns(port_insample_markowitz_bh_max_mean$returns),
table.AnnualizedReturns(port_insample_markowitz_rebal_mes_max_mean$returns),
table.AnnualizedReturns(port_insample_markowitz_rebal_trim_max_mean$returns),

  #Min Std
table.AnnualizedReturns(port_insample_markowitz_bh_min_std$returns),
table.AnnualizedReturns(port_insample_markowitz_rebal_mes_min_std$returns),
table.AnnualizedReturns(port_insample_markowitz_rebal_trim_min_std$returns),

  #Max Sharpe
table.AnnualizedReturns(port_insample_markowitz_bh_max_sharpe$returns),
table.AnnualizedReturns(port_insample_markowitz_rebal_mes_max_sharpe$returns),
table.AnnualizedReturns(port_insample_markowitz_rebal_trim_max_sharpe$returns),

  #S&P/BVL
table.AnnualizedReturns(port_insample_spbvl$returns)
)

colnames(resumen_backtesting_markowitz_in_sample) <- 
  c("Naive - BH", "Naive - R1M", "Naive - R3M", 
    "Max Mean - BH", "Max Mean - R1M", "Max Mean - R3M",
    "Min Std - BH", "Min Std - R1M", "Min Std - R3M",
    "Max Sharpe - BH", "Max Sharpe - R1M", "Max Sharpe - R3M", "S&P/BVL") 

#Resultados Anualizados - Out of sample
resumen_backtesting_markowitz_out_of_sample <- cbind(
  #Naive
  table.AnnualizedReturns(port_outofsample_markowitz_bh_naive$returns),
  table.AnnualizedReturns(port_outofsample_markowitz_rebal_mes_naive$returns),
  table.AnnualizedReturns(port_outofsample_markowitz_rebal_trim_naive$returns),
  
  #Max Mean
  table.AnnualizedReturns(port_outofsample_markowitz_bh_max_mean$returns),
  table.AnnualizedReturns(port_outofsample_markowitz_rebal_mes_max_mean$returns),
  table.AnnualizedReturns(port_outofsample_markowitz_rebal_trim_max_mean$returns),
  
  #Min Std
  table.AnnualizedReturns(port_outofsample_markowitz_bh_min_std$returns),
  table.AnnualizedReturns(port_outofsample_markowitz_rebal_mes_min_std$returns),
  table.AnnualizedReturns(port_outofsample_markowitz_rebal_trim_min_std$returns),
  
  #Max Sharpe
  table.AnnualizedReturns(port_outofsample_markowitz_bh_max_sharpe$returns),
  table.AnnualizedReturns(port_outofsample_markowitz_rebal_mes_max_sharpe$returns),
  table.AnnualizedReturns(port_outofsample_markowitz_rebal_trim_max_sharpe$returns),

  #S&P/BVL
  table.AnnualizedReturns(port_outofsample_spbvl$returns)
)

colnames(resumen_backtesting_markowitz_out_of_sample) <- 
  c("Naive - BH", "Naive - R1M", "Naive - R3M", 
    "Max Mean - BH", "Max Mean - R1M", "Max Mean - R3M",
    "Min Std - BH", "Min Std - R1M", "Min Std - R3M",
    "Max Sharpe - BH", "Max Sharpe - R1M", "Max Sharpe - R3M", "S&P/BVL") 

#ind. de riqueza out-of-sample
ind_riqueza <- cbind(
  #Naive
port_outofsample_markowitz_bh_naive$wealthindex,
port_outofsample_markowitz_rebal_mes_naive$wealthindex,
port_outofsample_markowitz_rebal_trim_naive$wealthindex,

  #Max mean
port_outofsample_markowitz_bh_max_mean$wealthindex,
port_outofsample_markowitz_rebal_mes_max_mean$wealthindex,
port_outofsample_markowitz_rebal_trim_max_mean$wealthindex,

  #Min Std
port_outofsample_markowitz_bh_min_std$wealthindex,
port_outofsample_markowitz_rebal_mes_min_std$wealthindex,
port_outofsample_markowitz_rebal_trim_min_std$wealthindex,

  #Max Sharpe
port_outofsample_markowitz_bh_max_sharpe$wealthindex,
port_outofsample_markowitz_rebal_mes_max_sharpe$wealthindex,
port_outofsample_markowitz_rebal_trim_max_sharpe$wealthindex,

  #S&P BVL
port_outofsample_spbvl$wealthindex
)

colnames(ind_riqueza) <- 
  c("Naive - BH", "Naive - R1M", "Naive - R3M", 
    "Max Mean - BH", "Max Mean - R1M", "Max Mean - R3M",
    "Min Std - BH", "Min Std - R1M", "Min Std - R3M",
    "Max Sharpe - BH", "Max Sharpe - R1M", "Max Sharpe - R3M", "S&P/BVL") 

#Plot de Ind. Riqueza Out of sample
ggplot(
  (xts_a_dataframe(ind_riqueza) %>%
  gather(key="Criterio", value="Riqueza", c(-date), factor_key = TRUE, na.rm = TRUE)),
  aes(x=date, y=Riqueza, colour=Criterio)) +
  scale_y_continuous(limits=c(0.7,2), breaks=seq(0.75,2,0.25)) +
  geom_line(size=1.2) +
  ggtitle("Backtesting Out of Sample Markowitz - Comparacion de ind. Riqueza") +
  xlab("Fecha") +
  scale_colour_manual(name = "Criterio",values = c("#ffa900","#ff7600","#ff2500",
                          "#f8e620","#f8e620","#f8e620",
                          "#6fb0d7","#317fbc","#08326e",
                          "#6dcd58","#39ba76","#20908d",
                          "#000005"))

#Eliminar variables que ya no se usaran:
rm(start_time,sd_markowitz_anual, sd_markowitz, rend_opt_min_riesgo_markowitz, rend_opt_max_rend_markowitz, pos_max, pesos_opt_min_riesgo_markowitz, n_markowitz,
   N_markowitz, N_acc_port, media_markowitz, media_markowitz_anual, k, i, grid_markowitz, end_time, opt, grafico, consolidado_pesos_markowitz, pesos_markowitz, plotlist_markowitz, port_insample_markowitz_bh_max_mean, port_insample_markowitz_bh_max_sharpe, port_insample_markowitz_bh_min_std, port_insample_markowitz_bh_naive, port_insample_markowitz_rebal_mes_max_mean, port_insample_markowitz_rebal_mes_max_sharpe,port_insample_markowitz_rebal_mes_min_std,port_insample_markowitz_rebal_mes_naive,
   port_insample_markowitz_rebal_trim_max_mean, port_insample_markowitz_rebal_trim_max_sharpe, port_insample_markowitz_rebal_trim_min_std, port_insample_markowitz_rebal_trim_naive, port_outofsample_markowitz_bh_max_mean, port_outofsample_markowitz_bh_max_sharpe, port_outofsample_markowitz_bh_min_std,
   port_outofsample_markowitz_bh_naive, port_outofsample_markowitz_rebal_mes_max_mean, port_outofsample_markowitz_rebal_mes_max_sharpe, port_outofsample_markowitz_rebal_mes_naive, port_outofsample_markowitz_rebal_trim_max_mean, port_outofsample_markowitz_rebal_trim_max_sharpe, port_outofsample_markowitz_rebal_trim_min_std, port_outofsample_markowitz_rebal_trim_naive, pos_max_sharpe_markowitz_in_sample, max_sharpe_markowitz_in_sample, markowitz_in_sample_enc)

#C.SIMULACIoN BASADA EN CoPULAS-IN-SAMPLE#######################################################
#C.1.SIMULACIoN DE LOS RENDIMIENTOS##################################################
nomarchivodistribuciones <- "Distribucion Acciones - Simulacion copulas.xlsx"
distribuciones <- read_excel(nomarchivodistribuciones, sheet = 1, col_names=TRUE)

#Bondad de ajuste de copulas (Corre en +7horas)
t_gof_ini <- Sys.time()
gfn <- gofCopula(normalCopula(dim = 30), as.matrix(rend_copia), N = 1000)
gfc <- gofCopula(claytonCopula(dim = 30), as.matrix(rend_copia), N = 1000)
gft <- gofCopula(tCopula(dim = 30, df=9, df.fixed=TRUE), as.matrix(rend_copia), N = 1000)
t_gof_fin <- Sys.time()

#Fijar numero de portafolios a optimizar y el numero de simulaciones
n_simcop <- 100
nsim=10000

#Definir distribuciones
ajuste_h <- distribuciones$`AJUSTE HORIZONTAL`
margins <- distribuciones$MODELO
paramMargins <- NULL

for (i in c(1:nacciones)) {
  parametros <- evaluar(distribuciones[i,"PARaMETROS"])
  paramMargins <- append(paramMargins, list(parametros))
}

#Transformar matriz de correlacion a lista
lista_cor <- NULL
matriz_cor <- cor(rend_copia, method="kendall")
for (i in c(1:(nacciones-1))) {
  lista_cor <- unname(append(lista_cor, matriz_cor[i,(i+1):nacciones]))
}

#Generar copula
copula <- tCopula(param=lista_cor, dim = nacciones, df=9, dispstr = "un")
mvdc <- mvdc(copula=copula, margins=margins, paramMargins=paramMargins)

#Simular rendimientos a partir de la copula 
set.seed(1) #Fijar semilla
sim <- rMvdc(nsim, mvdc) + rep(ajuste_h, each = nsim)
colnames(sim) <- orden_nemonico

#Graficar distribuciones marginales de probabilidad
for (i in c(1:nacciones)) {
  grafico3 <- ggplot(data.frame(sim), aes(x= data.frame(sim)[,i])) + 
    geom_histogram(bins=30, aes(y = ..density.., fill = ..count..)) +
    xlab("") + ylab("") + 
    scale_x_continuous(limits=c(-0.55,+0.8), breaks=c(-0.5,-0.25,0,0.25,0.5,0.75),labels = scales::percent_format(accuracy = 1)) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    ggtitle(paste(colnames(data.frame(sim)[i]), " (Simulado)")) + theme(legend.title=element_blank()) + theme(legend.position="none")
  plot(grafico3) 
}

#C.2.OPTIMIZACIoN#######################################
#Crear vectores y matrices vacios almacenar informacion de la optimizacion
media_simcop_anual <- mediageom_simcop_anual <- sd_simcop_anual <- semisd_simcop_anual <- N_simcop <- N_acc_port <- consolidado_pesos_simcop <- NULL 
sharpe_simcop_anual <- sortino_simcop_anual <- var95_simcop_anual <- var99_simcop_anual <- cvar95_simcop_anual <- cvar99_simcop_anual <- NULL

start_time <- Sys.time()
#Realizar iteraciones aumentando el # de acciones disponibles (segun indicador Sortino)
for(k in 2:nacciones) {
  #Optimizacion para encontrar la rentabilidad del portafolio optimo de minima desviacion estandar
  objfun=function(x){
    return(sd(sim[,1:k]%*%x))
  }
  
  confun=function(x){
    f=NULL
    f=rbind(f,sum(x)-1)
    return(list(ceq=f,c=NULL))
  }
  
  x0= rep(1/k,k)[1:k]
  lb=rep(0,k)
  ub=rep(1,k)
  
  solucion <- solnl(x0,objfun=objfun,confun=confun, lb=lb,ub=ub)
  rend_opt_min_simcop <- mean(sim[,1:k]%*%solucion$par)
  
  #El portafolio optimo de mayor rentabilidad sera una inversion del 100% en el activo con mayor rendimiento
  rend_opt_max_simcop <- max(colMeans(sim[,1:k]))
  pos_max <- which.max(colMeans(sim[,1:k]))   #Ahora la 6ta accion (BAP) genera el portafolio mas rentable

  #Crear un "grid" para que el algoritmo itere y cree 100 portafolios optimos sujetos a una rentabilidad minima
  #entre el portafolio de minima desviacion estandar y el de maximo rendimiento global
  grid <- seq(rend_opt_min_simcop, rend_opt_max_simcop, length.out = n_simcop)  
  grid[n_simcop] <- grid[n_simcop]-10^-8

  #Crear vectores para almacenar valores de los portafolios optimos
  media_simcop <- mediageom_simcop <- sd_simcop <-   semisd_simcop <- sharpe_simcop <- sortino_simcop <- var95_simcop <- var99_simcop <- cvar95_simcop <- cvar99_simcop <- rep(NA, n_simcop)
  pesos_simcop <- matrix(NA, n_simcop, k)

  #Realizar la optimizacion MIN SD s.t. MEAN y almacenar informacion
  for(i in 1:n_simcop) {
    r=grid[i]
    
    objfun=function(x){
      return(sd(sim[,1:k]%*%x))
    }
    
    confun=function(x){
      f=NULL
      f=rbind(f,colSums(sim[,1:k]%*%x)-r*nsim)
      f=rbind(f,sum(x)-1)
      return(list(ceq=f,c=NULL))
    }
    
    x0= rep(0,k)[1:k]
    lb=rep(0,k)
    ub=rep(1,k)
    
    solucion <- solnl(x0,objfun=objfun,confun=confun, lb=lb,ub=ub)
    media_simcop[i] <- grid[i]
    mediageom_simcop[i] <- mean.geometric(sim[,1:k]%*%solucion$par)
    sd_simcop[i] <- solucion$fn
    semisd_simcop[i] <- sd((sim[,1:k]%*%solucion$par)[(sim[,1:k]%*%solucion$par)<0])
    sharpe_simcop[i] <- sharpe(media_simcop[i], sd_simcop[i],0)
    sortino_simcop[i] <- sortino(media_simcop[i], semisd_simcop[i],0)
    var95_simcop[i]<- var(sim[,1:k]%*%solucion$par,0.95)
    var99_simcop[i]<- var(sim[,1:k]%*%solucion$par,0.99)
    cvar95_simcop[i]<- cvar(sim[,1:k]%*%solucion$par,0.95)
    cvar99_simcop[i]<- cvar(sim[,1:k]%*%solucion$par,0.99)
    pesos_simcop[i,] <- solucion$par
  }

  #Consolidar la informacion y crear un consolidado de pesos
  media_simcop_anual <- append(media_simcop_anual, media_simcop*12)
  mediageom_simcop_anual <- append(mediageom_simcop_anual, (1+mediageom_simcop)^12-1)
  sd_simcop_anual <- append(sd_simcop_anual, sd_simcop*12^0.5)
  semisd_simcop_anual <- append(semisd_simcop_anual, semisd_simcop*12^0.5)
  sharpe_simcop_anual <- append(sharpe_simcop_anual, sharpe_simcop*12^0.5)
  sortino_simcop_anual <- append(sortino_simcop_anual, sortino_simcop*12^0.5)
  var95_simcop_anual <- append(var95_simcop_anual, var95_simcop*12^0.5)
  var99_simcop_anual <- append(var99_simcop_anual, var99_simcop*12^0.5)
  cvar95_simcop_anual <- append(cvar95_simcop_anual, cvar95_simcop*12^0.5)
  cvar99_simcop_anual <- append(cvar99_simcop_anual, cvar99_simcop*12^0.5)
  N_simcop <- append(N_simcop, seq(1,n_simcop))
  N_acc_port <- append(N_acc_port, rep(k,n_simcop))
  pesos_simcop <- cbind(pesos_simcop,matrix(0, ncol = nacciones-k, nrow = n_simcop))
  consolidado_pesos_simcop <- rbind(consolidado_pesos_simcop,pesos_simcop)
}
end_time <- Sys.time()
tiempo_simcop_sd <- end_time - start_time

#Dar formato a los nombres de las variables, crear un consolido final y una version "tidy" del consolidado final
colnames(consolidado_pesos_simcop) <- colnames(rend_copia)
consolidado_simcop <- data.frame(cbind(N_simcop,N_acc_port,consolidado_pesos_simcop, media_simcop_anual, mediageom_simcop_anual, sd_simcop_anual, semisd_simcop_anual, sharpe_simcop_anual, sortino_simcop_anual, var95_simcop_anual, var99_simcop_anual, cvar95_simcop_anual, cvar99_simcop_anual))

#Guardar los resultados en otra variable, ya que las siguientes optimizaciones se reecribiran sobre variables intermedias
consolidado_simcop_sd <- consolidado_simcop
simcop_sd.tidy <- consolidado_simcop_sd %>%
  gather(key="Nemonico", value="Peso", c(-N_simcop,-N_acc_port,-media_simcop_anual,-mediageom_simcop_anual, -sd_simcop_anual,-semisd_simcop_anual, -sharpe_simcop_anual, -sortino_simcop_anual, -var95_simcop_anual, -var99_simcop_anual, -cvar95_simcop_anual, -cvar99_simcop_anual),factor_key = TRUE, na.rm = TRUE)

#Plot de composicion optima segun rentabilidad (media)
plotlist_simcop_sd = NULL
for(k in 2:nacciones) {
  grafico <- ggplot(simcop_sd.tidy%>% filter(N_acc_port==k), aes(x=media_simcop_anual,y=Peso)) +
    geom_area(aes(fill=Nemonico),position = position_stack(reverse = T))+
    ggtitle(paste("n=",k,sep="")) +
    theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(limits=c(0.075,0.195),breaks=seq(0.08,0.18,0.02), labels = scales::percent_format(accuracy = 1))+
    theme(legend.position = "none") +
    colScale+xlab("Media")
  plotlist_simcop_sd[[k]] = grafico   
}

grid.arrange(grobs=plotlist_simcop_sd[2:9],ncol=4)
grid.arrange(grobs=plotlist_simcop_sd[10:17],ncol=4)
grid.arrange(grobs=plotlist_simcop_sd[18:25],ncol=4)
grid.arrange(grobs=plotlist_simcop_sd[26:30],ncol=4)

#Plot de la composicion del portafolo cuando hay 30 acciones
plotlist_simcop_sd[[30]]+
  ggtitle("Composicion de portafolios eficientes, segun nivel de rentabilidad, minimizando Desviacion Estandar - Copulas")+
  theme(legend.position = "right")

#Plot de prontera eficiente segun num. de activos analizados
ggplot(simcop_sd.tidy, aes(x=sd_simcop_anual,y=media_simcop_anual)) +
  geom_line(aes(color=factor(N_acc_port)),size=1.25) + 
  ggtitle("Frontera eficiente segun Num. de acciones disponibles") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
  scale_y_continuous(limits=c(0.06,0.20),breaks=seq(0.06,0.20,0.02), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(limits=c(0.12,0.30),breaks=seq(0.12,0.30,0.02), labels = scales::percent_format(accuracy = 1)) +
  labs(color = "Num. acciones disponibles") + xlab("Desviacion Estandar") + ylab("Media")+
  scale_color_viridis(discrete=TRUE, direction=-1)

#Plot de posible rango de desviacion estandar del portafolio, segun numero de activos
min_max_simcop_sd <- simcop_sd.tidy %>%
  group_by(N_acc_port) %>% 
  summarize(min_sd = min(sd_simcop_anual), max_sd=max(sd_simcop_anual))

ggplot(simcop_sd.tidy, aes(x=factor(N_acc_port),y=sd_simcop_anual)) +
  geom_line(aes(color=factor(N_acc_port)),size=1) + 
  scale_color_viridis(discrete=TRUE, direction=-1) +
  ggtitle("Efecto de la diversificacion: Nivel de riesgo segun # de acciones disponibles - Copulas") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
  scale_y_continuous(limits=c(0.12,0.30),breaks=seq(0.10,0.30,0.04), labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(name="Num. acciones disponibles", breaks=seq(2,30,2)) +
  geom_line(data =min_max_simcop_sd, aes(x=factor(N_acc_port),y=min_sd, group = 1), size=1.5,color="#F8766D") +
  geom_point(data=min_max_simcop_sd, aes(x=factor(N_acc_port),y=max_sd, group = 1)) +
  geom_point(data =min_max_simcop_sd, aes(x=factor(N_acc_port),y=min_sd, group = 1)) +
  labs(color = "Num. acciones disponibles") + ylab("Desviacion estandar")

#Calculo de indicadores del portafolio
consolidado_simcop_sd30 <- consolidado_simcop_sd[N_acc_port==nacciones,]
consolidado_simcop_sd_enc <- 1/rowSums(consolidado_simcop_sd30[,3:(3+nacciones-1)]^2)
consolidado_simcop_sd30 <- cbind(consolidado_simcop_sd30, consolidado_simcop_sd_enc)
colnames(consolidado_simcop_sd30)[ncol(consolidado_simcop_sd30)] <-  "ENC"

#GRaFICOS DE EVOLUCIoN INDICADORES ANTE UNA VARIACIoN LINEAL DE MEDIA DEL PORTAFOLIO
#Media Anual vs Desv.Est. Anual
ggplot(simcop_sd.tidy %>% filter(N_acc_port==30), aes(x=media_simcop_anual,y=sd_simcop_anual))+
  geom_line(size=1, color="#F8766D") +
  ggtitle("Evolucion Desv. Estandar respecto a Media") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
  scale_y_continuous(name="Desv. Estandar Anual.", labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(name="Media Anual.", labels = scales::percent_format(accuracy = 1)) 

#Media Anual vs Semi-Desv. Anual.
ggplot(simcop_sd.tidy %>% filter(N_acc_port==30), aes(x=media_simcop_anual,y=semisd_simcop_anual))+
  geom_line(size=1, color="#CD9600") +
  ggtitle("Evolucion Semi-Desv. Estandar respecto a Media") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
  scale_y_continuous(name="Semi-Desv. Estandar Anual.", labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(name="Media Anual.", labels = scales::percent_format(accuracy = 1)) 

#Media Anual vs Sharpe Anual.
ggplot(simcop_sd.tidy %>% filter(N_acc_port==30), aes(x=media_simcop_anual,y=sharpe_simcop_anual))+
  geom_line(size=1, color="#7CAE00") +
  ggtitle("Evolucion Ratio Sharpe respecto a Media") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
  scale_y_continuous(name="Ratio Sharpe Anual.", labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(name="Media Anual.", labels = scales::percent_format(accuracy = 1)) 

#Media Anual vs Sortino Anual.
ggplot(simcop_sd.tidy %>% filter(N_acc_port==30), aes(x=media_simcop_anual,y=sortino_simcop_anual))+
  geom_line(size=1, color="#00BE67") +
  ggtitle("Evolucion Ratio Sortino respecto a Media") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
  scale_y_continuous(name="Ratio Sortino Anual.", labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(name="Media Anual.", labels = scales::percent_format(accuracy = 1)) 

#Media Anual. vs VaR95 anual
ggplot(simcop_sd.tidy %>% filter(N_acc_port==30), aes(x=media_simcop_anual,y=var95_simcop_anual))+
  geom_line(size=1, color= "#00BFC4") +
  ggtitle("Evolucion VaR95 respecto a Media") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
  scale_y_continuous(name="VaR95 Anual.", labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(name="Media Anual.", labels = scales::percent_format(accuracy = 1)) 

#Media Anual. vs VaR99 anual
ggplot(simcop_sd.tidy %>% filter(N_acc_port==30), aes(x=media_simcop_anual,y=var99_simcop_anual))+
  geom_line(size=1, color= "#00A9FF") +
  ggtitle("Evolucion VaR99 respecto a Media") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
  scale_y_continuous(name="VaR99 Anual.", labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(name="Media Anual.", labels = scales::percent_format(accuracy = 1)) 

#Media Anual. vs CVaR95 anual
ggplot(simcop_sd.tidy %>% filter(N_acc_port==30), aes(x=media_simcop_anual,y=cvar95_simcop_anual))+
  geom_line(size=1, color= "#C77CFF") +
  ggtitle("Evolucion CVaR95 respecto a Media") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
  scale_y_continuous(name="CVaR95 Anual.", labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(name="Media Anual.", labels = scales::percent_format(accuracy = 1)) 

#Media Anual. vs CVaR99 anual
ggplot(simcop_sd.tidy %>% filter(N_acc_port==30), aes(x=media_simcop_anual,y=cvar99_simcop_anual))+
  geom_line(size=1, color= "#FF61CC") +
  ggtitle("Evolucion CVaR99 respecto a Media") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
  scale_y_continuous(name="CVaR99 Anual.", labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(name="Media Anual.", labels = scales::percent_format(accuracy = 1)) 

#OPTIMOS GLOBALES SEGuN INDICADOR
pos_opt_media <- which.max((consolidado_simcop_sd %>% filter(N_acc_port==30))$media_simcop_anual)
pos_opt_mediageom <- which.max((consolidado_simcop_sd %>% filter(N_acc_port==30))$mediageom_simcop_anual)
pos_opt_sd <-  which.min((consolidado_simcop_sd %>% filter(N_acc_port==30))$sd_simcop_anual)
pos_opt_semisd <- which.min((consolidado_simcop_sd %>% filter(N_acc_port==30))$semisd_simcop_anual)
pos_opt_sharpe <- which.max((consolidado_simcop_sd %>% filter(N_acc_port==30))$sharpe_simcop_anual)
pos_opt_sortino <- which.max((consolidado_simcop_sd %>% filter(N_acc_port==30))$sortino_simcop_anual)
pos_opt_var95 <- which.max((consolidado_simcop_sd %>% filter(N_acc_port==30))$var95_simcop_anual)
pos_opt_var99 <- which.max((consolidado_simcop_sd %>% filter(N_acc_port==30))$var99_simcop_anual)
pos_opt_cvar95 <- which.max((consolidado_simcop_sd %>% filter(N_acc_port==30))$cvar95_simcop_anual)
pos_opt_cvar99 <- which.max((consolidado_simcop_sd %>% filter(N_acc_port==30))$cvar99_simcop_anual)

opt_media <-(consolidado_simcop_sd %>% filter(N_acc_port==30))[pos_opt_media,-c(1:2)]
opt_mediageom <- (consolidado_simcop_sd %>% filter(N_acc_port==30))[pos_opt_mediageom,-c(1:2)]
opt_sd <- (consolidado_simcop_sd %>% filter(N_acc_port==30))[pos_opt_sd,-c(1:2)]
opt_semisd <- (consolidado_simcop_sd %>% filter(N_acc_port==30))[pos_opt_semisd,-c(1:2)]
opt_sharpe <- (consolidado_simcop_sd %>% filter(N_acc_port==30))[pos_opt_sharpe,-c(1:2)]
opt_sortino <- (consolidado_simcop_sd %>% filter(N_acc_port==30))[pos_opt_sortino,-c(1:2)]
opt_var95 <- (consolidado_simcop_sd %>% filter(N_acc_port==30))[pos_opt_var95,-c(1:2)]
opt_var99 <- (consolidado_simcop_sd %>% filter(N_acc_port==30))[pos_opt_var99,-c(1:2)]
opt_cvar95 <- (consolidado_simcop_sd %>% filter(N_acc_port==30))[pos_opt_cvar95,-c(1:2)]
opt_cvar99 <- (consolidado_simcop_sd %>% filter(N_acc_port==30))[pos_opt_cvar99,-c(1:2)]

opt_media$Indicador <- "MAX MEDIA"
opt_mediageom$Indicador <- "MAX MEDIA GEOM"
opt_sd$Indicador <- "MIN SD"
opt_semisd$Indicador <- "MIN SEMI-SD"
opt_sharpe$Indicador <- "MAX SHARPE"
opt_sortino$Indicador <- "MAX SORTINO"
opt_var95$Indicador <- "MIN VAR95"
opt_var99$Indicador <- "MIN VAR99"
opt_cvar95$Indicador <- "MIN CVAR95"
opt_cvar99$Indicador <- "MIN CVAR99"

indicadores_opt <- data.frame(rbind(opt_media,opt_mediageom, opt_sd,opt_semisd,opt_sharpe, opt_sortino, opt_var95,opt_var99, opt_cvar95, opt_cvar99  ))
indicadores_opt.tidy <- data.frame(indicadores_opt) %>%
  gather(key="Nemonico", value="Peso", c(-Indicador,-media_simcop_anual,-mediageom_simcop_anual,-sd_simcop_anual,-semisd_simcop_anual, -sharpe_simcop_anual, -sortino_simcop_anual, -var95_simcop_anual, -var99_simcop_anual, -cvar95_simcop_anual, -cvar99_simcop_anual),factor_key = TRUE, na.rm = TRUE)
indicadores_opt.tidy$Indicador <- factor(indicadores_opt.tidy$Indicador, levels=c("MAX MEDIA", "MAX MEDIA GEOM","MIN SD","MIN SEMI-SD", "MAX SHARPE", "MAX SORTINO", "MIN VAR95", "MIN VAR99", "MIN CVAR95", "MIN CVAR99"))

#Grafico de composicion de portafolios optimos, segun los indicadores
ggplot(indicadores_opt.tidy, aes(x=factor(Indicador),y=Peso)) +
  geom_col(aes(fill=factor(Nemonico)),position = position_stack(reverse = T))+
  ggtitle(paste("n=",k,sep="")) +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "right") +
  colScale+
  ggtitle("Composicion de portafolios eficientes, segun indicador - Copulas")+
  xlab("Indicador") 


#C.3.BACKTESTING#################################
#Determinacion de los pesos de los portafolios optimos
pesos_simcop_in_sample_naive <- rep(1/nacciones,nacciones)
pesos_simcop_in_sample_max_mean <- as.numeric(t(opt_media[,1:30]))
pesos_simcop_in_sample_max_meangeom <- as.numeric(t(opt_mediageom[,1:30]))
pesos_simcop_in_sample_min_std <- as.numeric(t(opt_sd[,1:30]))
pesos_simcop_in_sample_min_semistd <- as.numeric(t(opt_semisd[,1:30]))
pesos_simcop_in_sample_max_sharpe <- as.numeric(t(opt_sharpe[,1:30]))
pesos_simcop_in_sample_max_sortino <- as.numeric(t(opt_sortino[,1:30]))
pesos_simcop_in_sample_min_var95 <- as.numeric(t(opt_var95[,1:30]))
pesos_simcop_in_sample_min_var99 <- as.numeric(t(opt_var99[,1:30]))
pesos_simcop_in_sample_min_cvar95 <- as.numeric(t(opt_cvar95[,1:30]))
pesos_simcop_in_sample_min_cvar99 <- as.numeric(t(opt_cvar99[,1:30]))

#Naive
port_insample_simcop_bh_naive <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_naive, wealth.index=TRUE, verbose = TRUE)
port_insample_simcop_rebal_mes_naive <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_naive, wealth.index=TRUE, rebalance_on = "months", verbose = TRUE)
port_insample_simcop_rebal_trim_naive <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_naive, wealth.index=TRUE, rebalance_on = "quarters", verbose = TRUE)

port_outofsample_simcop_bh_naive <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_naive, wealth.index=TRUE,verbose = TRUE)
port_outofsample_simcop_rebal_mes_naive <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_naive, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_simcop_rebal_trim_naive <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_naive, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#Max mean
port_insample_simcop_bh_max_mean <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_max_mean, wealth.index=TRUE,verbose=TRUE)
port_insample_simcop_rebal_mes_max_mean <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_max_mean, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_insample_simcop_rebal_trim_max_mean <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_max_mean, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

port_outofsample_simcop_bh_max_mean <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_max_mean, wealth.index=TRUE, verbose=TRUE)
port_outofsample_simcop_rebal_mes_max_mean <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_max_mean, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_simcop_rebal_trim_max_mean <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_max_mean, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#Max mean geom
port_insample_simcop_bh_max_meangeom <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_max_meangeom, wealth.index=TRUE,verbose=TRUE)
port_insample_simcop_rebal_mes_max_meangeom <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_max_meangeom, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_insample_simcop_rebal_trim_max_meangeom <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_max_meangeom, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

port_outofsample_simcop_bh_max_meangeom <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_max_meangeom, wealth.index=TRUE, verbose=TRUE)
port_outofsample_simcop_rebal_mes_max_meangeom <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_max_meangeom, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_simcop_rebal_trim_max_meangeom <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_max_meangeom, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#Min Std
port_insample_simcop_bh_min_std <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_std, wealth.index=TRUE,verbose = TRUE)
port_insample_simcop_rebal_mes_min_std <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_std, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_insample_simcop_rebal_trim_min_std <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_std, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

port_outofsample_simcop_bh_min_std <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_std, wealth.index=TRUE,verbose = TRUE)
port_outofsample_simcop_rebal_mes_min_std <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_std, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_simcop_rebal_trim_min_std <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_std, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#Min Semi-Std
port_insample_simcop_bh_min_semistd <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_semistd, wealth.index=TRUE,verbose = TRUE)
port_insample_simcop_rebal_mes_min_semistd <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_semistd, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_insample_simcop_rebal_trim_min_semistd <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_semistd, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

port_outofsample_simcop_bh_min_semistd <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_semistd, wealth.index=TRUE,verbose = TRUE)
port_outofsample_simcop_rebal_mes_min_semistd <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_semistd, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_simcop_rebal_trim_min_semistd <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_semistd, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#Max Sharpe
port_insample_simcop_bh_max_sharpe <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_max_sharpe, wealth.index=TRUE, verbose=TRUE)
port_insample_simcop_rebal_mes_max_sharpe <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_max_sharpe, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_insample_simcop_rebal_trim_max_sharpe <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_max_sharpe, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

port_outofsample_simcop_bh_max_sharpe <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_max_sharpe, wealth.index=TRUE, verbose=TRUE)
port_outofsample_simcop_rebal_mes_max_sharpe <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_max_sharpe, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_simcop_rebal_trim_max_sharpe <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_max_sharpe, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#Max Sortino
port_insample_simcop_bh_max_sortino <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_max_sortino, wealth.index=TRUE, verbose=TRUE)
port_insample_simcop_rebal_mes_max_sortino <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_max_sortino, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_insample_simcop_rebal_trim_max_sortino <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_max_sortino, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

port_outofsample_simcop_bh_max_sortino <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_max_sortino, wealth.index=TRUE, verbose=TRUE)
port_outofsample_simcop_rebal_mes_max_sortino <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_max_sortino, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_simcop_rebal_trim_max_sortino <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_max_sortino, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#Min VaR95
port_insample_simcop_bh_min_var95 <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_var95, wealth.index=TRUE, verbose=TRUE)
port_insample_simcop_rebal_mes_min_var95 <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_var95, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_insample_simcop_rebal_trim_min_var95 <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_var95, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

port_outofsample_simcop_bh_min_var95 <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_var95, wealth.index=TRUE, verbose=TRUE)
port_outofsample_simcop_rebal_mes_min_var95 <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_var95, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_simcop_rebal_trim_min_var95 <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_var95, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#Min VaR99
port_insample_simcop_bh_min_var99 <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_var99, wealth.index=TRUE, verbose=TRUE)
port_insample_simcop_rebal_mes_min_var99 <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_var99, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_insample_simcop_rebal_trim_min_var99 <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_var99, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

port_outofsample_simcop_bh_min_var99 <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_var99, wealth.index=TRUE, verbose=TRUE)
port_outofsample_simcop_rebal_mes_min_var99 <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_var99, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_simcop_rebal_trim_min_var99 <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_var99, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#Min CVaR95
port_insample_simcop_bh_min_cvar95 <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_cvar95, wealth.index=TRUE, verbose=TRUE)
port_insample_simcop_rebal_mes_min_cvar95 <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_cvar95, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_insample_simcop_rebal_trim_min_cvar95 <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_cvar95, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

port_outofsample_simcop_bh_min_cvar95 <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_cvar95, wealth.index=TRUE, verbose=TRUE)
port_outofsample_simcop_rebal_mes_min_cvar95 <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_cvar95, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_simcop_rebal_trim_min_cvar95 <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_cvar95, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#Min CVaR99
port_insample_simcop_bh_min_cvar99 <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_cvar99, wealth.index=TRUE, verbose=TRUE)
port_insample_simcop_rebal_mes_min_cvar99 <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_cvar99, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_insample_simcop_rebal_trim_min_cvar99 <- Return.portfolio(rend_copia, weights = pesos_simcop_in_sample_min_cvar99, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

port_outofsample_simcop_bh_min_cvar99 <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_cvar99, wealth.index=TRUE, verbose=TRUE)
port_outofsample_simcop_rebal_mes_min_cvar99 <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_cvar99, wealth.index=TRUE,rebalance_on = "months", verbose = TRUE)
port_outofsample_simcop_rebal_trim_min_cvar99 <- Return.portfolio(rend_outofsample, weights = pesos_simcop_in_sample_min_cvar99, wealth.index=TRUE,rebalance_on = "quarters", verbose = TRUE)

#S&P/BVL
port_outofsample_spbvl <- Return.portfolio(spbvl, weights = c(1), wealth.index=TRUE, verbose = TRUE)

#Resultados Anualizados - In sample
resumen_backtesting_simcop_in_sample <- cbind(
  #Naive
  table.AnnualizedReturns(port_insample_simcop_bh_naive$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_mes_naive$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_trim_naive$returns),
  
  #Max Mean
  table.AnnualizedReturns(port_insample_simcop_bh_max_mean$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_mes_max_mean$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_trim_max_mean$returns),

  #Max Mean Geom
  table.AnnualizedReturns(port_insample_simcop_bh_max_meangeom$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_mes_max_meangeom$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_trim_max_meangeom$returns),
  
  #Min Std
  table.AnnualizedReturns(port_insample_simcop_bh_min_std$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_mes_min_std$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_trim_min_std$returns),
  
  #Min Semi-Std
  table.AnnualizedReturns(port_insample_simcop_bh_min_semistd$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_mes_min_semistd$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_trim_min_semistd$returns),
  
  #Max Sharpe
  table.AnnualizedReturns(port_insample_simcop_bh_max_sharpe$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_mes_max_sharpe$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_trim_max_sharpe$returns),
  
  #Max Sortino
  table.AnnualizedReturns(port_insample_simcop_bh_max_sortino$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_mes_max_sortino$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_trim_max_sortino$returns),
  
  #Min VaR95
  table.AnnualizedReturns(port_insample_simcop_bh_min_var95$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_mes_min_var95$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_trim_min_var95$returns),   
  
  #Min VaR99
  table.AnnualizedReturns(port_insample_simcop_bh_min_var99$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_mes_min_var99$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_trim_min_var99$returns),  

  #Min CVaR95
  table.AnnualizedReturns(port_insample_simcop_bh_min_cvar95$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_mes_min_cvar95$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_trim_min_cvar95$returns),  
  
  #Min CVaR99
  table.AnnualizedReturns(port_insample_simcop_bh_min_cvar99$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_mes_min_cvar99$returns),
  table.AnnualizedReturns(port_insample_simcop_rebal_trim_min_cvar99$returns)  
  )

colnames(resumen_backtesting_simcop_in_sample) <- 
  c("Naive - BH", "Naive - R1M", "Naive - R3M", 
    "Max Mean - BH", "Max Mean - R1M", "Max Mean - R3M",
    "Max MeanGeom - BH", "Max MeanGeom - R1M", "Max MeanGeom - R3M",
    "Min Std - BH", "Min Std - R1M", "Min Std - R3M",
    "Min Semi-Std - BH", "Min Semi-Std - R1M", "Min Semi-Std - R3M",
    "Max Sharpe - BH", "Max Sharpe - R1M", "Max Sharpe - R3M",
    "Max Sortino - BH", "Max Sortino - R1M", "Max Sortino - R3M",
    "Min VaR95 - BH", "Min VaR95 - R1M", "Min VaR95 - R3M",    
    "Min VaR99 - BH", "Min VaR99 - R1M", "Min VaR99 - R3M",  
    "Min CVaR95 - BH", "Min CVaR95 - R1M", "Min CVaR95 - R3M",  
    "Min CVaR99 - BH", "Min CVaR99 - R1M", "Min CVaR99 - R3M" 
    ) 

#Resultados Anualizados - Out of sample
resumen_backtesting_simcop_out_of_sample <- cbind(
  #Naive
  table.AnnualizedReturns(port_outofsample_simcop_bh_naive$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_mes_naive$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_trim_naive$returns),
  
  #Max Mean
  table.AnnualizedReturns(port_outofsample_simcop_bh_max_mean$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_mes_max_mean$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_trim_max_mean$returns),
  
  #Max Mean Geom
  table.AnnualizedReturns(port_outofsample_simcop_bh_max_meangeom$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_mes_max_meangeom$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_trim_max_meangeom$returns),
  
  #Min Std
  table.AnnualizedReturns(port_outofsample_simcop_bh_min_std$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_mes_min_std$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_trim_min_std$returns),
  
  #Min Semi-Std
  table.AnnualizedReturns(port_outofsample_simcop_bh_min_semistd$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_mes_min_semistd$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_trim_min_semistd$returns),
  
  #Max Sharpe
  table.AnnualizedReturns(port_outofsample_simcop_bh_max_sharpe$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_mes_max_sharpe$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_trim_max_sharpe$returns),
  
  #Max Sortino
  table.AnnualizedReturns(port_outofsample_simcop_bh_max_sortino$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_mes_max_sortino$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_trim_max_sortino$returns),
  
  #Min VaR95
  table.AnnualizedReturns(port_outofsample_simcop_bh_min_var95$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_mes_min_var95$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_trim_min_var95$returns),   
  
  #Min VaR99
  table.AnnualizedReturns(port_outofsample_simcop_bh_min_var99$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_mes_min_var99$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_trim_min_var99$returns),  
  
  #Min CVaR95
  table.AnnualizedReturns(port_outofsample_simcop_bh_min_cvar95$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_mes_min_cvar95$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_trim_min_cvar95$returns),  
  
  #Min CVaR99
  table.AnnualizedReturns(port_outofsample_simcop_bh_min_cvar99$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_mes_min_cvar99$returns),
  table.AnnualizedReturns(port_outofsample_simcop_rebal_trim_min_cvar99$returns),
  
  #S&P/BVL
  table.AnnualizedReturns(port_outofsample_spbvl$returns)
)

colnames(resumen_backtesting_simcop_out_of_sample) <- 
  c("Naive - BH", "Naive - R1M", "Naive - R3M", 
    "Max Mean - BH", "Max Mean - R1M", "Max Mean - R3M",
    "Max MeanGeom - BH", "Max MeanGeom - R1M", "Max MeanGeom - R3M",
    "Min Std - BH", "Min Std - R1M", "Min Std - R3M",
    "Min Semi-Std - BH", "Min Semi-Std - R1M", "Min Semi-Std - R3M",
    "Max Sharpe - BH", "Max Sharpe - R1M", "Max Sharpe - R3M",
    "Max Sortino - BH", "Max Sortino - R1M", "Max Sortino - R3M",
    "Min VaR95 - BH", "Min VaR95 - R1M", "Min VaR95 - R3M",    
    "Min VaR99 - BH", "Min VaR99 - R1M", "Min VaR99 - R3M",  
    "Min CVaR95 - BH", "Min CVaR95 - R1M", "Min CVaR95 - R3M",  
    "Min CVaR99 - BH", "Min CVaR99 - R1M", "Min CVaR99 - R3M" ,
    "S&P/BVL") 

#indice de riqueza out-of-sample
ind_riqueza_simcop <- cbind(
  #Naive
  port_outofsample_simcop_bh_naive$wealthindex,
  port_outofsample_simcop_rebal_mes_naive$wealthindex,
  port_outofsample_simcop_rebal_trim_naive$wealthindex,
  
  #Max mean
  port_outofsample_simcop_bh_max_mean$wealthindex,
  port_outofsample_simcop_rebal_mes_max_mean$wealthindex,
  port_outofsample_simcop_rebal_trim_max_mean$wealthindex,
  
  #Max mean geom
  port_outofsample_simcop_bh_max_meangeom$wealthindex,
  port_outofsample_simcop_rebal_mes_max_meangeom$wealthindex,
  port_outofsample_simcop_rebal_trim_max_meangeom$wealthindex,
  
  #Min Std
  port_outofsample_simcop_bh_min_std$wealthindex,
  port_outofsample_simcop_rebal_mes_min_std$wealthindex,
  port_outofsample_simcop_rebal_trim_min_std$wealthindex,
  
  #Min Semi-Std
  port_outofsample_simcop_bh_min_semistd$wealthindex,
  port_outofsample_simcop_rebal_mes_min_semistd$wealthindex,
  port_outofsample_simcop_rebal_trim_min_semistd$wealthindex,
  
  #Max Sharpe
  port_outofsample_simcop_bh_max_sharpe$wealthindex,
  port_outofsample_simcop_rebal_mes_max_sharpe$wealthindex,
  port_outofsample_simcop_rebal_trim_max_sharpe$wealthindex,
  
  #Max Sortino
  port_outofsample_simcop_bh_max_sortino$wealthindex,
  port_outofsample_simcop_rebal_mes_max_sortino$wealthindex,
  port_outofsample_simcop_rebal_trim_max_sortino$wealthindex,
  
  #Min VaR95
  port_outofsample_simcop_bh_min_var95$wealthindex,
  port_outofsample_simcop_rebal_mes_min_var95$wealthindex,
  port_outofsample_simcop_rebal_trim_min_var95$wealthindex,
  
  #Min VaR99
  port_outofsample_simcop_bh_min_var99$wealthindex,
  port_outofsample_simcop_rebal_mes_min_var99$wealthindex,
  port_outofsample_simcop_rebal_trim_min_var99$wealthindex,
  
  #Min CVaR95
  port_outofsample_simcop_bh_min_cvar95$wealthindex,
  port_outofsample_simcop_rebal_mes_min_cvar95$wealthindex,
  port_outofsample_simcop_rebal_trim_min_cvar95$wealthindex,
  
  #Min CVaR99
  port_outofsample_simcop_bh_min_cvar99$wealthindex,
  port_outofsample_simcop_rebal_mes_min_cvar99$wealthindex,
  port_outofsample_simcop_rebal_trim_min_cvar99$wealthindex,
  
  #S&P BVL
  port_outofsample_spbvl$wealthindex
)

colnames(ind_riqueza_simcop) <- 
  c("Naive - BH", "Naive - R1M", "Naive - R3M", 
    "Max Mean - BH", "Max Mean - R1M", "Max Mean - R3M",
    "Max MeanGeom - BH", "Max MeanGeom - R1M", "Max MeanGeom - R3M",
    "Min Std - BH", "Min Std - R1M", "Min Std - R3M",
    "Min Semi-Std - BH", "Min Semi-Std - R1M", "Min Semi-Std - R3M",
    "Max Sharpe - BH", "Max Sharpe - R1M", "Max Sharpe - R3M",
    "Max Sortino - BH", "Max Sortino - R1M", "Max Sortino - R3M",
    "Min VaR95 - BH", "Min VaR95 - R1M", "Min VaR95 - R3M",    
    "Min VaR99 - BH", "Min VaR99 - R1M", "Min VaR99 - R3M",  
    "Min CVaR95 - BH", "Min CVaR95 - R1M", "Min CVaR95 - R3M",  
    "Min CVaR99 - BH", "Min CVaR99 - R1M", "Min CVaR99 - R3M" ,
    "S&P/BVL") 

#Plot de Ind. Riqueza - Mean, SD, Sharpe
ggplot(
  (xts_a_dataframe(ind_riqueza_simcop)[,c("date","Naive...BH","Naive...R1M","Naive...R3M","Max.Mean...BH","Max.Mean...R1M","Max.Mean...R3M","Min.Std...BH","Min.Std...R1M","Min.Std...R3M","Max.Sharpe...BH","Max.Sharpe...R1M","Max.Sharpe...R3M","S.P.BVL")] %>%
     gather(key="Criterio", value="Riqueza", c(-date), factor_key = TRUE, na.rm = TRUE)),
  aes(x=date, y=Riqueza, colour=Criterio)) +
  scale_y_continuous(limits=c(0.70,2), breaks=seq(0.75,2,0.25))+
  geom_line(size=1) +
  ggtitle("Backtesting Out of Sample - Comparacion de Ind. Riqueza") +
  xlab("Fecha") +
  scale_colour_manual(name = "Criterio",values = c("#ffa900","#ff7600","#ff2500",  #naive
                                                   "#f8e620","#f8e620","#f8e620",  #mean  
                                                 "#6fb0d7","#317fbc","#08326e",    #sd
                                                 "#6dcd58","#39ba76","#20908d",    #sharpe
                                                 "#000005"))                       #s&p/bvl


#Plot de Ind. Riqueza - Todos los indicadores
ggplot(
  (xts_a_dataframe(ind_riqueza_simcop)[,c("date","Naive...BH","Max.Mean...BH","Max.MeanGeom...BH","Min.Std...BH","Min.Semi.Std...BH","Max.Sharpe...BH","Max.Sortino...BH","Min.VaR95...BH","Min.VaR99...BH","Min.CVaR95...BH","Min.CVaR99...BH","S.P.BVL")] %>%
     gather(key="Criterio", value="Riqueza", c(-date), factor_key = TRUE, na.rm = TRUE)),
  aes(x=date, y=Riqueza, colour=Criterio)) +
  geom_line(size=1.2) +
  ggtitle("Backtesting Out of Sample - Comparacion de Ind. Riqueza") +
  xlab("Fecha") +
  scale_colour_manual(name = "Criterio",values = c("#ff1604",  #naive
                                                   "#f8e620",  #mean  
                                                   "#e6823e",  #mean geom  
                                                   "#1f1f99",  #sd
                                                   "#1f1f99",  #semisd  
                                                   "#43935b",  #sharpe
                                                   "#48d960",  #sortino
                                                   "#666666",  #var95
                                                   "#c4c4c4",  #var99
                                                   "#1f1f99",  #cvar95
                                                   "#52ccc2",  #cvar99
                                                   "#000005")) #s&p/bvl


#Plot comparativo backtesting - Markowitz vs Simulacion
backtesting_simcop <- (xts_a_dataframe(ind_riqueza_simcop))[,c("date","Naive...BH","Max.Mean...BH","Min.Std...BH","Max.Sharpe...BH","S.P.BVL")] %>%
gather(key="Criterio", value="Riqueza", c(-date), factor_key = TRUE, na.rm = TRUE)
backtesting_simcop$Criterio=paste("Simulacion - ",backtesting_simcop$Criterio)

backtesting_markowitz <- ((xts_a_dataframe(ind_riqueza))[,c("date","Naive...BH","Max.Mean...BH","Min.Std...BH","Max.Sharpe...BH","S.P.BVL")] %>%
       gather(key="Criterio", value="Riqueza", c(-date), factor_key = TRUE, na.rm = TRUE))
backtesting_markowitz$Criterio=paste("Markowitz - ",backtesting_markowitz$Criterio)

ggplot(
  rbind(backtesting_markowitz,backtesting_simcop),
  aes(x=date, y=Riqueza, colour=Criterio)) +
  geom_line(size=1.2) +
  ggtitle("Backtesting Out of Sample - Comparacion de Ind. Riqueza") +
  xlab("Fecha") +
  scale_colour_manual(name = "Criterio",values = c("#ffffcc",  "#d6e7d5", "#cfd3eb",   "#eccdca", "#d0d0d0",
                                                   "#f8e620",  "#43935b",  "#1f1f99",   "#ff1604", "#000005" )) 
                                      
#D.GRaFICOS COMPLEMENTARIOS (ESTADiSTICA DESCRIPTIVA)############################################################################
#WAFFLE CHART DE COMPOSICIoN DE ACTIVOS SELECCIONADOS A CONFORMAR EL PORTAFOLIO
conteo_sector <- resumenconsolidado %>%
  dplyr::count(Sector) %>%
  mutate(percent = smart_round(n/sum(n)*100))

porcen_sector <- conteo_sector$percent
names(porcen_sector) <- conteo_sector$Sector
porcen_sector<- sort(porcen_sector, decreasing=TRUE)

waffle(porcen_sector, colors=c("#00B8AA", "#374649", "#FD625E", "#F2C811", "#a66999", "#8AD4EB", "#FE9666")) +
  ggtitle("Distribucion de acciones, segun sector al que pertenecen") +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom", legend.text = element_text(size = 11))
rm(porcen_sector, conteo_sector)


#GRaFICOS DE DISPERSIoN
#Diagrama de cajas Rendimientos simples
tidy_consolidado_ind$nemonico <- factor(tidy_consolidado_ind$nemonico, levels = rev(orden_nemonico),ordered = TRUE)
ggplot(tidy_consolidado_ind %>% filter(!is.na(rendimiento)), aes(x = nemonico, y = rendimiento)) +
  geom_boxplot(varwidth = TRUE) + coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(title= "Diagrama de cajas de los rendimentos simples", x="Nemonico", y="Rendimiento simple")

#Media vs Desviacion estandar
ggplot(resumenconsolidado, aes(desv_anio,media_anio, label=Nemonico, color = Sector)) +
  labs(title= "Grafico de dispersion - Desviacion Estandar vs Media de los rendimientos", x="Desviacion Estandar", y="Media")+
  geom_text(aes(fontface=2))+
  scale_color_manual(values=c("#a66999", "#F2C811", "#FE9666", "#FD625E", "#8AD4EB", "#00B8AA", "#374649"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))
  
#Media vs Semi-Desviacion estandar
ggplot(resumenconsolidado, aes(semidesv_anio,media_anio, label=Nemonico, color = Sector)) +
  labs(title= "Grafico de dispersion - Semi-Desviacion Estandar vs Media de los rendimientos", x="Semi-Desviacion Estandar", y="Media")+
  geom_text(aes(fontface=2))+
  scale_color_manual(values=c("#a66999", "#F2C811", "#FE9666", "#FD625E", "#8AD4EB", "#00B8AA", "#374649"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))


#PRUEBAS DE BONDAD DE AJUSTE
#Sesgo vs Curtosis
ggplot(resumenconsolidado, aes(sesgo,curtosis, label=Nemonico, color = Sector)) +
  labs(title= "Grafico de dispersion - Sesgo vs Curtosis", x="Sesgo", y="Curtosis")+
  geom_text(aes(fontface=2))+
  scale_x_continuous(limits=c(-1,4)) +
  scale_y_continuous(limits=c(0,23)) +
  scale_color_manual(values=c("#a66999", "#F2C811", "#FE9666", "#FD625E", "#8AD4EB", "#00B8AA", "#374649"))+
  geom_hline(yintercept=3) + geom_vline(xintercept =0)

#Ploteo de todas las acciones ajustadas a la distribucion normal
rend_copia_norm <- xts_a_dataframe(rend_copia)[,-1]
for (i in c(1:nacciones)) {
  grafico2 <- ggplot(rend_copia_norm, aes(x= rend_copia_norm[,i])) + 
    geom_histogram(bins=30, aes(y = ..density.., fill = ..count..)) +
    stat_function(size=1, fun = dnorm, colour = "firebrick",
                  args = list(mean = mean(rend_copia_norm[,i]),
                              sd = sd(rend_copia_norm[,i]))) +
    xlab("") + ylab("") + scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    ggtitle(colnames(rend_copia_norm[i])) + theme(legend.title=element_blank()) + theme(legend.position="none")
  plot(grafico2) 
}

#Pruebas de bondad de ajuste (normalidad)  
ad_test <- NULL
lillie_test <- NULL 
jb_test <- NULL

for (i in c(1:nacciones)) {
  ad_test <- append(ad_test, ad.test(rend_copia_norm[,i])$p.value)
  lillie_test <- append(lillie_test, lillie.test(rend_copia_norm[,i])$p.value)
  jb_test <- append(jb_test, jarque.test(rend_copia_norm[,i])$p.value)
}

#Consolida los p-value de las pruebas
testnorm <- rbind(ad_test, lillie_test, jb_test)
colnames(testnorm) <- orden_nemonico
testnorm <- data.frame(testnorm)
testnorm > 0.05
rowSums(testnorm > 0.05)
rm(rend_copia_norm, ad_test, lillie_test, jb_test, grafico2, i)

#AUTOCORRELACIoN
#Plot de log-retornos
plot.zoo(rend_copia[,c(1,3,5,7,9,2,4,6,8,10)], type = "h", main="", xlab="")
plot.zoo(rend_copia[,c(11,13,15,17,19,12,14,16,18,20)], type = "h", main="", xlab="")
plot.zoo(rend_copia[,c(21,23,25,27,29,22,24,26,28,30)], type = "h", main="", xlab="")

#ACF
rend_copia_acf <- xts_a_dataframe(rend_copia)[,-1]
#Plot de acf(rendimientos)
for (i in c(1:nacciones)) {
    acfplot <- acf(rend_copia_acf[,i], lag.max=10, plot=F)
    plot(acfplot,main=paste(orden_nemonico[i],"- Rendimientos"))
}

#Plot de acf(abs(rendimientos))
for (i in c(1:nacciones)) {
  acfplot <- acf(abs(rend_copia_acf[,i]), lag.max=10, plot=F)
  plot(acfplot,main=paste(orden_nemonico[i],"-  Rendimientos Absolutos"))
}

#Prueba Ljung-Box
ljung_box <- NULL
ljung_box_abs <- NULL

for (i in c(1:nacciones)) {
  ljung_box <- append(ljung_box, Box.test(rend_copia_acf[,i], lag = 10, type = "Ljung")$p.value)
  ljung_box_abs <- append(ljung_box_abs, Box.test(abs(rend_copia_acf[,i]), lag = 10, type = "Ljung")$p.value)
}

testacf <- rbind(ljung_box, ljung_box_abs)
colnames(testacf) <- orden_nemonico
testacf <- data.frame(testacf)
rm(rend_copia_acf, acfplot, ljung_box, ljung_box_abs, grafico2, i)


#CORRPLOT
#Pearson
ggcorrplot(cor(rend_copia, method="pearson"), hc.order = FALSE, legend.title = "Correlacion", title="Matriz de correlacion - Pearson") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10))
#Kendall
ggcorrplot(cor(rend_copia, method = "kendall"), hc.order = FALSE, legend.title = "Correlacion", title="Matriz de correlacion - Kendall") +
theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10))
#Spearman
ggcorrplot(cor(rend_copia, method="spearman"), hc.order = FALSE, legend.title = "Correlacion", title="Matriz de correlacion - Spearman") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10))

#Validacion estructura copulas
#Real
ggcorrplot(cor(rend_copia, method="kendall"), hc.order = FALSE, legend.title = "Correlacion", title="Matriz de correlacion (Real)") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10))
#Simulado
ggcorrplot(cor(sim, method="kendall"), hc.order = FALSE, legend.title = "Correlacion", title="Matriz de correlacion (Simulado)") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_text(size = 10))