if (!require(readxl)) install.packages("readxl")

library(readxl)

archivo <- file.choose()

datos<-read_excel(archivo)

#En el desarrollo me di cuenta que uso varias veces esto, asique averigue como se hace.
crear_frecuencias_acumuladas <- function(frecuencia_absoluta){
  return(cumsum(frecuencia_absoluta))
}


#Creacion de tabla plataformas utilizadas y obtencion de valores absolutos y relativos.
tabla_plataforma<-table(datos$Plataforma_Trabajo)
frec_abs_plat<- as.vector(tabla_plataforma)
frec_rel_plat <- as.vector(round(prop.table(frec_abs_plat), 3))

tabla_plataforma_final<-data.frame(
  Plataforma = names(tabla_plataforma),
  Frec_Absoluta = frec_abs_plat,
  Frec_Relativa = frec_rel_plat
)

#Creacion de tabla tickets resueltos y obtencion de valores absolutos y relativos.
tabla_tickets<-table(datos$Tickets_Soporte)
frec_abs_tick<-as.vector(tabla_tickets)
frec_rel_tick <- as.vector(round(prop.table(tabla_tickets), 3))

tabla_tickets_final<-data.frame(
  Numero_De_Tickets = names(tabla_tickets),
  Frecuencia_Absoluta = frec_abs_tick,
  Frecuencia_Absoluta_Acum = crear_frecuencias_acumuladas(frec_abs_tick),
  Frecuencia_Relativa = frec_rel_tick,
  Frecuencia_Relativa_Acum = crear_frecuencias_acumuladas(frec_rel_tick)
)
 
 #Creacion de tabla Tiempos de conexión y obtencion de valores absolutos y relativos.
 tiempos <- datos$Tiempo_Conexion_Min
 
 num_clases <- ceiling(1 + 3.322 * log10(length(tiempos)))
 rango <- range(tiempos)
 amplitud<-ceiling((rango[2] - rango[1]) / num_clases)
 
 breaks<- seq(floor(rango[1]), ceiling(rango[2]) + amplitud, by = amplitud)
 
 clases <- cut(tiempos, breaks = breaks, right = FALSE)
 tabla_tiempo <- table(clases)
 
 frec_abs_tiempo <- as.vector(tabla_tiempo)
 frec_rel_tiempo<- as.vector(round(prop.table(tabla_tiempo), 3))
 
 tabla_tiempo_final<- data.frame(
   Intervalo = names(tabla_tiempo),
   Frecuencia_Absoluta = frec_abs_tiempo,
   Frecuencia_Absoluta_Acum = crear_frecuencias_acumuladas(frec_abs_tiempo),
   Frecuencia_Relativa = frec_rel_tiempo,
   Frecuencia_Relativa_Acum = crear_frecuencias_acumuladas(frec_rel_tiempo)
)

tiempo_promedio<- round(mean(datos$Tiempo_Conexion_Min), 2)


#Resultados finales.
tabla_plataforma_final
tabla_tickets_final
tabla_tiempo_final 
tiempo_promedio

#La plataforma de comunicación mas utilizada es "Slack", con un 34% de los usuarios.
#En una semana se resolvieron 103 tickets.
#El tiempo promedio de conexión fue de 72.84 minutos.

# Aprendiendo R, Semana 3.
# Analisis variable tickets.
variable_tickets<-datos$Tickets_Soporte
print(variable_tickets)

faltantes_variable_tickets <- sum(is.na(datos$Tickets_Soporte))
cat("Datos faltantes: ", faltantes_variable_tickets, "\n")

print(summary(variable_tickets))

media_tickets<-mean(variable_tickets, na.rm = TRUE)

mediana_tickets<-median(variable_tickets, na.rm = TRUE)

install.packages("modeest")
library("modeest")

moda_tickets<-mlv(variable_tickets, method = "mfv")

desvio_estandar_tickets<-sd(variable_tickets, na.rm = TRUE)

varianza_tickets<-var(variable_tickets, na.rm = TRUE)

cx_var_tickets<-(desvio_estandar_tickets / media_tickets) * 100

message("\nDatos estadisticos Tickets: ")
tabla_stats_tickets<-data.frame(
  Media = round(media_tickets, 4),
  Mediana = round(mediana_tickets, 4), 
  Moda = paste(moda_tickets, collapse = ", "),
  Varianza = round(varianza_tickets, 4),
  Desvio_Estandar = round(desvio_estandar_tickets, 4),
  Coef_Variacion_Tickets = round(cx_var_tickets, 4)
)
print(tabla_stats_tickets, row.names = FALSE)

cuartiles_tickets <- quantile(variable_tickets, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

rango_intercuartil_tickets<-IQR(variable_tickets, na.rm = TRUE)
cuartiles_tickets
cat("RIC: ", rango_intercuartil_tickets, "\n")


# Analisis de la variable tiempo de conexion.

marcas_clase_tiempo<-( head(breaks, -1) + tail(breaks, -1) ) / 2
print(marcas_clase_tiempo)

media_continua<-sum(marcas_clase_tiempo * frec_abs_tiempo) / sum(frec_abs_tiempo)

# Moda
i_modal_tiempo<-which.max(frec_abs_tiempo)
L_m<-breaks[i_modal_tiempo]
f_m<-frec_abs_tiempo[i_modal_tiempo]
f_1<-ifelse(i_modal_tiempo == 1, 0, frec_abs_tiempo[i_modal_tiempo - 1])
f_2<-ifelse(i_modal_tiempo == length(frec_abs_tiempo), 0, frec_abs_tiempo[i_modal_tiempo + 1])

moda_continua_tiempo<-L_m + ((f_m - f_1) / ((f_m - f_1) + (f_m - f_2))) * amplitud

# Mediana
frecuencia_acumulada_tiempo<-crear_frecuencias_acumuladas(frec_abs_tiempo)
n_total<-sum(frec_abs_tiempo)
n_2<-n_total / 2
clase_mediana_index<-which(frecuencia_acumulada_tiempo >= n_2)[1]
L<- breaks[clase_mediana_index]
F_anterior<-ifelse(clase_mediana_index == 1, 0, frecuencia_acumulada_tiempo[clase_mediana_index - 1])
f_mediana<-frec_abs_tiempo[clase_mediana_index]
mediana_tiempo<- L + ((n_2 - F_anterior) / f_mediana) * amplitud

# Medidas de dispersion
varianza_continua<-sum(frec_abs_tiempo * (marcas_clase_tiempo - media_continua)^2) / (n_total - 1)
desvio_estandar_tiempo<-sqrt(varianza_continua)
cx_var_tiempo<-(desvio_estandar_tiempo / media_continua) * 100

message("\nResultados Variable Tiempo de Conexion: ")
tiempo_stats<-data.frame(
  Media = round(media_continua, 4),
  Moda = round(moda_continua_tiempo, 4),
  Mediana = round(mediana_tiempo, 4),
  Varianza = round(varianza_continua, 4),
  Desvio_Estandar = round(desvio_estandar_tiempo, 4),
  Coef_Variacion_pct = round(cx_var_tiempo, 4)
)

print(tiempo_stats, row.names = FALSE)

message("\nDatos estadisticos Tickets: ")
print(tabla_stats_tickets, row.names = FALSE)

