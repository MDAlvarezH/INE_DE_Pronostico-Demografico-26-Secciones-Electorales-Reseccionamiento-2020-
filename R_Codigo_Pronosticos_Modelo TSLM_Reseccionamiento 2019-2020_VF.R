#### R_Codigo_Pronosticos (modelo TSLM)_Reseccionamiento 2019-2020 (26 secciones) ####
### INE-DERFE-COC-DE-EVALUACION DEMOGRAFICA ###
# Autor: Miguel David Alvarez Hernández
# Ultima versión : 04/02/2020


########## Paquetes y setup ###########


library(pacman)
p_load(tidyverse,
       forecast,
       imputeTS,
       gridExtra,
       ggthemes,
       RColorBrewer,
       colorRamps,
       scales,
       directlabels,
       haven,
       plyr,
       readr, 
       timetk, 
       sweep, 
       tidyquant)
#Prevenir notación científica
options(scipen=999) 
#Directorio de trabajo
setwd("C:/Users/miguel.alvarez/Google Drive/INE/DERFE/Estudios_2020/Pronostico Reseccionamiento_2020/Codigo_Datos")


#################### Datos ################


#historico LNE 26 de secciones (ultimo corte a dic 2019)
LNE_secciones <- read_csv("Datos_26secciones.csv")

#26 secciones del programa 2019
SECCIONES_RESECCIONAMIENTO_2019 <- read_csv("Datos_SECCIONES_RESECCIONAMIENTO_2019.csv")

# Base de datos areas por seccion (en base a la cartografía de las mza)
secc_area_19 <- read_csv("Datos_secc_areamz_19.csv")

#union de SECCIONES_RESECCIONAMIENTO_2019 y secc_area_19
areas_tb <- merge(secc_area_19, SECCIONES_RESECCIONAMIENTO_2019, by=c("EST","DTO","MUN","SECC"))

#union LNE_secciones y areas_tb
datos <- merge(areas_tb, LNE_secciones, by=c("EST","DTO","MUN", "SECC"))

#seleccionamos la serie a partir de 12/2012 para eliminar partes irregulares de la serie de tiempo
datos2 = datos[-c(6:67)]
View(datos2)

#formato tidy (panel)
panel <- datos2 %>% group_by(EST,DTO,MUN,SECC) %>% gather("Fecha", "LNE", 6:90) #intervalo de columnas 3-108 para no elegir las columnas de area

#añadimos una columna con el dato de la densidad
panel <- panel %>%
  mutate(Densidad = LNE/Area_Km2)

#añadimos una columna Seccion como variable factor (para distinguir en las gráficas)

panel[,'EST'] <- lapply(panel[,'EST'] , factor)

panel[,'DTO'] <- lapply(panel[,'DTO'] , factor)

panel[,'MUN'] <- lapply(panel[,'MUN'] , factor)

panel[,'SECC'] <- lapply(panel[,'SECC'] , factor)


#se transforma la columna de las fechas a formato Date
panel <- panel %>% group_by(EST,DTO,MUN,SECC) %>%
  mutate(Tiempo = as.Date(panel$Fecha, "%d/%m/%Y"))

#eliminamos la columna Fecha y reordenamos las columnas
panel = subset(panel, select = -c(Fecha))
col_order <- c("EST","DTO","MUN","SECC", "Tiempo","Area_Km2",
               "LNE", "Densidad")
panel <- panel[, col_order]
View(panel)


################### Analisis gráfico (26 secciones) ###################


#agrupamos por estado y seccion
panel2 <- panel %>% 
  group_by(EST,DTO,MUN,SECC) 

#grafica de series de tiempo LNE 
png(file="Rplot_timeseries_LNE_secc_total.png",
    width=700, height=400)
ggplot(data = panel2, aes(x = Tiempo, y = LNE, group=SECC, colour=SECC )) + 
  labs(title = "Evolución Lista Nominal (26 secciones)",
       subtitle = "(dic 2012-dic 2019)", x = "Año", y = "Lista Nominal") +
  geom_line() +
  geom_dl(aes(label = SECC), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +
  theme_light() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + #ticks cada 2 años
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(legend.position="none", 
        legend.direction = "horizontal", 
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(2.5,"cm"))
dev.off()


#grafica de series de tiempo Densidad
png(file="Rplot_timeseries_Densidad_secc_total.png",
    width=700, height=400)
ggplot(data = panel2, aes(x = Tiempo, y = Densidad, group=SECC, colour=SECC)) + 
  labs(title = "Evolución Densidad (26 secciones)",
       subtitle = "(dic 2012-dic 2019)", x = "Año", y = "Densidad (personas por km2)") +
  geom_line() +
  geom_dl(aes(label = SECC), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +
  theme_light() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + #ticks cada 2 años
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(legend.position="none", 
        legend.direction = "horizontal", 
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(2.5,"cm"))
dev.off()

#casos específicos de secciones de Nuevo León
seccNL <- panel2 %>% filter(EST == 19)

ggplot(data = seccNL, aes(x = Tiempo, y = LNE, group=SECC, colour=SECC)) + 
  labs(title = "Evolución Lista Nominal (Nuevo León)",
       subtitle = "(dic 2012-dic 2019)", x = "Año", y = "Lista Nominal") +
  geom_line() +
  geom_dl(aes(label = SECC), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +
  theme_light() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + #ticks cada 2 años
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(legend.position="none")

ggplot(data = seccNL, aes(x = Tiempo, y = Densidad, group=SECC, colour=SECC)) + 
  labs(title = "Evolución Densidad (Nuevo León)",
       subtitle = "(dic 2012-dic 2019)", x = "Año", y = "Densidad (personas por km2)") +
  geom_line() +
  geom_dl(aes(label = SECC), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +
  theme_light() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + #ticks cada 2 años
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(legend.position="none")



############## Modelo TSLM (pronósticos LNE 26 secciones) ###################

#solo seleccionamos las columnas necesarias para el pronóstico
panel_LNE = subset(panel, select = -c(Area_Km2,Densidad))
View(panel_LNE)

#lista de tibbles por cada una de las 26 secciones
listasecc <-split(panel_LNE, panel_LNE$SECC)
#Ejemplo del primer item (dataframe de la lista listasecc)
listasecc[[1]]

#parámetros del pronóstico
h <- 127 #127 meses a partir de 12/2019 (ultimo dato pronosticado a jul 2030)
inicio <- c(2012, 12) #inicio de la serie de tiempo
fin <- c(2019, 12) #fin de la serie de tiempo
perio <- 12 #datos mensuales
ic <- 95 #intervalo de predicción (95%)

# Ejemplo con la secc 13
#entrenamiento del modelo
b<- listasecc$`13`
ts_train<-tk_ts(b$LNE, start=inicio, end=fin, frequency=perio)
fit.lin <- tslm(ts_train ~ trend)
#pronostico para 127 meses
fcasts.lin <- forecast(fit.lin, h = h, level = c(95))
alto <- fcasts.lin$upper
bajo <- fcasts.lin$lower
puntual <- fcasts.lin$mean
#dataframe con los resultados
a<- data.frame(EST=rep(first(b$EST),length(alto)),DTO=rep(first(b$DTO),length(alto)),MUN=rep(first(b$MUN),length(alto)),SECC=rep(first(b$SECC),length(alto)), date=as.Date(time(alto)), est.punt=as.matrix(puntual), cota.inf = as.matrix(bajo), cota.sup = as.matrix(alto))
View(a)

#grafica
autoplot(ts_train) +
  autolayer(fitted(fit.lin),series="Histórico") +
  autolayer(fcasts.lin, series="Pronóstico", PI=TRUE) +
  theme_light() +
  scale_x_continuous(breaks = c(seq(2012, 2030, 2))) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  labs(title = "Pronóstico (Modelo Regresión Lineal)",
       x = "Año", y = "Lista Nominal") 

#autoplot(fcasts.lin, PI = TRUE, showgap = FALSE) +
 # autolayer(fitted(fit.lin), series = "Lineal") + 
  #theme_light()




#### Funciones de pronóstico, visualización y pruebas con TSLM (para LNE) ########


#función para obtener pronostico
f_tslmforecast <- function(serie, i, f, p, h, ic) { 
  ts_train <- tk_ts(serie$LNE, start=i, end=f, frequency=p)
  fit.lin <- tslm(ts_train ~ trend)
  fcasts.lin <- forecast(fit.lin, h = h, level = c(ic))
  alto <- fcasts.lin$upper
  bajo <- fcasts.lin$lower
  puntual <- fcasts.lin$mean
  #dataframe con los resultados
  fresults<- data.frame(EST=rep(first(serie$EST),length(alto)),DTO=rep(first(serie$DTO),length(alto)),MUN=rep(first(serie$MUN),length(alto)),SECC=rep(first(serie$SECC),length(alto)), date=as.Date(time(alto)), est.punt=as.matrix(puntual), cota.inf = as.matrix(bajo), cota.sup = as.matrix(alto))
  return(fresults)
  }

#función para graficar resultados
f_plotforecast <- function(serie, i, f, p, h, ic) { 
  ts_train <- tk_ts(serie$LNE, start=i, end=f, frequency=p)
  fit.lin <- tslm(ts_train ~ trend)
  fcasts.lin <- forecast(fit.lin, h = h, level = c(ic))
  #plot
  titulo_grafica = paste("Estado:",first(serie$EST),"Sección:", first(serie$SECC), sep=" ")
  grafica <- autoplot(ts_train) +
    autolayer(fitted(fit.lin),series="Histórico") +
    autolayer(fcasts.lin, series="Pronóstico", PI=TRUE) +
    theme_light() +
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
    labs(title = titulo_grafica,
         subtitle = "Ajuste y pronóstico (modelo TSLM)",
         x = "Año", y = "Lista Nominal")
  return(grafica)
}

#función para graficar pruebas del pronostico
f_testforecast <- function(serie, i, f, p, h, ic) { 
  ts_train <- tk_ts(serie$LNE, start=i, end=f, frequency=p)
  fit.lin <- tslm(ts_train ~ trend)
  fcasts.lin <- forecast(fit.lin, h = h, level = c(ic))
  ajuste <- summary(lm(LNE ~ Tiempo , data = serie))
  print(ajuste)
  check <- checkresiduals(fit.lin, lag.max=80)
  return(check)
}


#ejemplo con la sección 13
b<- listasecc$`13`
f_tslmforecast(b,inicio, fin, perio, h, ic)
f_plotforecast(b,inicio, fin, perio, h, ic)
f_testforecast(b,inicio, fin, perio, h, ic)



################# Resultados pronosticos LNE 26 secciones ###############

#parametros del pronóstico
h <- 127 #127 meses a partir de 12/2019 (ultimo dato pronosticado a jul 2030)
inicio <- c(2012, 12) #inicio de la serie de tiempo
fin <- c(2019, 12) #fin de la serie de tiempo
perio <- 12 #datos mensuales
ic <- 95 #intervalo de predicción del 95%

#se crea lista vacía para guardar los resultados del pronóstico
forecast_results <- list()

#for loop para iterar sobre cada una de las 26 secciones la función de pronóstico
for(k in 1:length(listasecc)) {        
  res = f_tslmforecast(listasecc[[k]],inicio, fin, perio, h, ic)
  forecast_results[[k]] <- res
}

#Se unen por renglones cada uno de los 26 dataframes resultantes
total_forecast_results <- do.call("rbind", forecast_results)
View(total_forecast_results)
#guardar resultados en csv
#write.csv(total_forecast_results,'C:\\Users\\miguel.alvarez\\Google Drive\\INE\\DERFE\\Estudios_2020\\Pronostico Reseccionamiento_2020\\Codigo_Datos\\Resultados_forecast_tslm_26secc_total.csv', row.names = FALSE)

#Filtramos para obtener la última estimacion a enero 200
jul2030_forecast_results <- filter(total_forecast_results, date == "2030-07-01")
View(jul2030_forecast_results)
#guardar resultados en csv
#write.csv(jul2030_forecast_results,'C:\\Users\\miguel.alvarez\\Google Drive\\INE\\DERFE\\Estudios_2020\\Pronostico Reseccionamiento_2020\\Codigo_Datos\\Resultados_forecast_tslm_26secc_jul2030.csv', row.names = FALSE)


################## Resultados gráficos de pronosticos ##############

#se crea lista vacía para guardar los resultados del pronóstico
forecast_plot_results <- list()

#for loop para iterar sobre cada una de las 26 secciones la función de gráfica
for(k in 1:length(listasecc)) {        
  res = f_plotforecast(listasecc[[k]],inicio, fin, perio, h, ic)
  forecast_plot_results[[k]] = res
  file_name = paste("Rplot_tslm_forecast_secc_", first(listasecc[[k]]$SECC), ".png", sep="")
  #guarda las gráficas generadas en el directorio del código
  png(file_name, width=750, height=500)
  #muestra cada una de las gráficas generadas
  print(forecast_plot_results[[k]])
  dev.off()
}

#Gráfica de red para las 26 secciones
png(file="Rplot_tslm_forecast_secc_total.png",
    width=3000, height=2900)
grid.arrange(grobs = forecast_plot_results, ncol = 3) 
dev.off()

#Also see: Multiple plot function (https://stackoverflow.com/questions/24387376/r-error-could-not-find-function-multiplot-using-cookbook-example)






############################### Referencias #######################


##### Referencias sobre graficos de series de tiempo

# https://stackoverflow.com/questions/10706753/how-do-i-arrange-a-variable-list-of-plots-using-grid-arrange
# https://www.datamentor.io/r-programming/saving-plot/
# https://stackoverflow.com/questions/2851327/convert-a-list-of-data-frames-into-one-data-frame
# https://robjhyndman.com/hyndsight/forecast7-ggplot2/
# https://stackoverflow.com/questions/47454063/add-lines-to-autoplot-in-r
# http://www.sthda.com/english/articles/32-r-graphics-essentials/128-plot-time-series-data-using-ggplot/
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_ts.html
# http://www.sthda.com/english/wiki/ggfortify-extension-to-ggplot2-to-handle-some-popular-packages-r-software-and-data-visualization
# https://stackoverflow.com/questions/26034177/save-multiple-ggplots-using-a-for-loop

########### Referencias sobre análisis y modelado de series de tiempo

# https://otexts.com/fpp2/
# https://datascienceplus.com/time-series-analysis-in-r-part-1-the-time-series-object/
# https://cran.rstudio.com/web/packages/sweep/vignettes/SW00_Introduction_to_sweep.html
# https://rstudio-pubs-static.s3.amazonaws.com/379009_476ca1f91996405db196c4f9fbfbcdb5.html
# https://otexts.com/fpp2/prediction-intervals.html

########### Referencias sobre pronosticos con modelos de regresion 

# https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals
# https://stats.stackexchange.com/questions/154346/fitted-confidence-intervals-forecast-function-r
# https://otexts.com/fpp2/forecasting-regression.html
# 