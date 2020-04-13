#### R_Codigo_Pronosticos (modelo logistico)_Reseccionamiento 2019-2020 (26 secciones) ####
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
       tidyquant,
       growthrates)
#Prevenir notación científica
options(scipen=999) 
#Directorio de trabajo
setwd("C:/Users/miguel.alvarez/Google Drive/INE/DERFE/Estudios_2020/Pronostico Reseccionamiento_2020/Codigo_Datos")

#################### Datos ################


#historico LNE de las 26 secciones en reseccionamiento (ultimo dato dic2019)
LNE_secciones <- read_csv("Datos_26secciones.csv")

#26 secciones del programa 2019
SECCIONES_RESECCIONAMIENTO_2019 <- read_csv("Datos_SECCIONES_RESECCIONAMIENTO_2019.csv")

# Base de datos areas por seccion (calculo de areas de manzanas)
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

#datos con corte de dic 2019
dic2019 <- filter(panel, Tiempo == "2019-12-01")
#datos con corte de dic 2012
dic2012 <- filter(panel, Tiempo == "2012-12-01")



############ Modelo logistico (pronósticos LNE para las 26 secciones) ##########

#lista de tibbles por cada una de las 26 secciones
listasecc <-split(panel, panel$SECC)
#Ejemplo del primer item (dataframe de la lista listasecc)
a <- listasecc[[1]]
View(a)

#parámetros del pronóstico
inicio <- as.Date("2020/01/1") #inicio del pronóstico
fin <- as.Date("2030/07/1") #fin del pronóstico
h <- 127 #127 meses el horizonte de pronóstico
p <- c(y0 = 3000, mumax = 0.2, K = 9000) #parámetros del modelo (y0, mumax, K)
lower <- c(y0 = 100, mumax = 0, K = 3000) #condición de frontera inferior
upper <- c(y0 = 7000, mumax = 5, K = 15000) #condición de frontera superior


#ejemplo con la secc 13 (listasecc[[1]])
#ajuste del modelo logistico a los datos observacionales
x=seq(0, nrow(a)-1) #el modelo sólo admite valores numéricos (se crea una escala temporal numérica)
y=a$Densidad #datos observacionales
fit1 <- fit_growthmodel(FUN = grow_logistic, p = p, x, y,
                        lower = lower, upper = upper)
#coeficientes resultantes (parametros que obtienen el mejor ajuste a los datos)
print(coef(fit1))
#plot
plot(fit1)
#plot log(y)
plot(fit1, log = "y")




##### Funciones de pronostico y visualizacion con modelo logistico (para LNE) ########

#función para ajustar e interpolar modelo logístico
f_logisticmodel_forecast <- function(df, p, lower, upper, inicio, fin, h) { 
  x = seq(0, nrow(df)-1) #secuencia temporal
  y = df$Densidad        #datos observacionales
  #ajuste del modelo
  fit = fit_growthmodel(FUN = grow_logistic, p = p, x, y,
                        lower = lower, upper = upper)
  coeficientes = coef(fit) #parametros mejor ajuste
  time_fit_forecast = seq(0, length(x)-1+h) #secuencia temporal para pronostico
  interpolacion_fit = grow_logistic(time_fit_forecast, coeficientes) #ajuste y valores pronostico
  time_forecast = seq(inicio, fin, "months") #secuencia temporal formato date
  values_forecast = interpolacion_fit[-(1:nrow(df)),"y"] #seleccion valores pronostivo
  sd_values_forecast = sd(values_forecast) #desviacion estandar serie de tiempo
  #dataframe con los resultados
  fresults = data.frame(EST=rep(first(df$EST),length(time_forecast)),
                        DTO=rep(first(df$DTO),length(time_forecast)),
                        MUN=rep(first(df$MUN),length(time_forecast)),
                        SECC=rep(first(df$SECC),length(time_forecast)), 
                        date=time_forecast, 
                        est.punt.densidad=values_forecast,
                        est.punt.LNE = values_forecast*first(df$Area_Km2),
                        cota.inf.LNE = (values_forecast - sd_values_forecast)*first(df$Area_Km2),
                        cota.sup.LNE = (values_forecast + sd_values_forecast)*first(df$Area_Km2))
  return(fresults)
}

#ejemplo con la secc13 (a):
res_secc13 = f_logisticmodel_forecast(a, p, lower, upper, inicio, fin, h)
View(res_secc13)


#función para graficar ajuste e interpolación modelo logístico
f_plotforecast <- function(df,p,lower,upper,inicio,fin,h) { 
  x = seq(0, nrow(df)-1) #secuencia temporal
  y = df$Densidad        #datos observacionales
  #ajuste del modelo
  fit = fit_growthmodel(FUN = grow_logistic, p = p, x, y,
                        lower = lower, upper = upper)
  coeficientes = coef(fit) #parametros mejor ajuste
  fit2 = grow_logistic(x, coeficientes)[,"y"] #ajuste y valores pronostico
  df$ajuste_logistico = fit2*first(df$Area_Km2) #se añade columna con valores de ajuste
  res_forecast = f_logisticmodel_forecast(df,p,lower,upper,inicio,fin,h) #resultados pronostico
  #line-plot
  titulo_grafica = paste("Estado:", first(df$EST),"Sección:", first(df$SECC), sep=" ")
  grafica <- ggplot() + 
    geom_line(data = df, aes(x = Tiempo, y = LNE), color = 'black') + #linea serie de tiempo
    geom_line(data = df, aes(x = Tiempo, y = ajuste_logistico), color = "red") + #linea ajuste
    geom_line(data = res_forecast, aes(x = date, y = est.punt.LNE), color = "red") + #linea pronostico
    geom_line(data = res_forecast, aes(x = date, y = cota.inf.LNE), color = "blueviolet", linetype = "dashed") + #cota.inf (68% confidence interval)
    geom_line(data = res_forecast, aes(x = date, y = cota.sup.LNE), color = "blueviolet", linetype = "dashed") + #cota.sup (68% confidence interval)
    theme_light() +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") + #ticks cada 2 años
    scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
    labs(title = titulo_grafica,
         subtitle = "Ajuste y pronóstico (modelo logístico)",
         x = "Año", y = "Lista Nominal")
  return(grafica)
}

#ejemplo con la seccion 13 (a)
f_plotforecast(a, p, lower, upper, inicio, fin, h)



################# Resultados pronosticos LNE 26 secciones ###############

#se crea lista vacía para guardar los resultados completos del pronóstico
forecast_results <- list()

#for loop para iterar sobre cada una de las 26 secciones la función de pronóstico
for(k in 1:length(listasecc)) {
  dat = listasecc[[k]]
  res = f_logisticmodel_forecast(dat, p, lower, upper, inicio, fin, h)
  forecast_results[[k]] <- res
}

#ejemplo
forecast_results[[1]]

#Se unen por renglones cada uno de los 26 dataframes resultantes de forecast_results
total_forecast_results <- do.call("rbind", forecast_results)
View(total_forecast_results)
#guardar resultados en csv
#write.csv(total_forecast_results,'C:\\Users\\miguel.alvarez\\Google Drive\\INE\\DERFE\\Estudios_2020\\Pronostico Reseccionamiento_2020\\Codigo_Datos\\Resultados_forecast_logistic_26secc_total.csv', row.names = FALSE)

#Filtramos para obtener la última estimacion a jul 2030
jul2030_forecast_results <- filter(total_forecast_results, date == "2030-07-01")
View(jul2030_forecast_results)
#guardar resultados en csv
#write.csv(jul2030_forecast_results,'C:\\Users\\miguel.alvarez\\Google Drive\\INE\\DERFE\\Estudios_2020\\Pronostico Reseccionamiento_2020\\Codigo_Datos\\Resultados_forecast_logistic_26secc_jul2030.csv', row.names = FALSE)


################## Resultados gráficos de pronosticos ##############

#se crea lista vacía para guardar los resultados del pronóstico
forecast_plot_results <- list()

#for loop para iterar sobre cada una de las 26 secciones la función de gráfica
for(k in 1:length(listasecc)) {        
  res = f_plotforecast(listasecc[[k]],p,lower,upper,inicio,fin,h)
  forecast_plot_results[[k]] = res
  file_name = paste("Rplot_logistic_forecast_secc_", first(listasecc[[k]]$SECC), ".png", sep="")
  #guarda las gráficas generadas en el directorio del código
  png(file_name, width=750, height=500)
  #muestra cada una de las gráficas generadas
  print(forecast_plot_results[[k]])
  dev.off()
}

#Gráfica de red para las 26 secciones
png(file="Rplot_logistic_forecast_secc_total.png",
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

########### Referencias sobre modelos de poblacion (paquete growthrates)

# https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals
# https://stats.stackexchange.com/questions/154346/fitted-confidence-intervals-forecast-function-r
# https://otexts.com/fpp2/forecasting-regression.html
# http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/
# https://tpetzoldt.github.io/growthrates/doc/Introduction.html
# https://www.rdocumentation.org/packages/growthrates/versions/0.8.1
