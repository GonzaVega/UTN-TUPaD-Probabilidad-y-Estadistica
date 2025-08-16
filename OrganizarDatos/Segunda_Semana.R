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
 
