paquetes <- c("readxl", "ggplot2", "scales", "modeest")

# Instalacion de todas las librerias necesarias en un lugar.
for (pkg in paquetes) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
archivo<-file.choose()
archivo

datos<-read_excel(archivo)
datos

# Funciones
crear_frecuencias_acumuladas <- function(frecuencia_absoluta){
  return(cumsum(frecuencia_absoluta))
}

crear_tabla_discreta <- function(source, columna, nombre_variable = "Variable"){
  tabla<-table(source[[columna]])
  frec_abs<-as.vector(tabla)
  frec_rel<-as.vector(round(prop.table(tabla), 4))
  
  df<-(data.frame(
    Variable = names(tabla),
    Frecuencia_Absoluta = frec_abs,
    Frecuencia_Absoluta_Acum = crear_frecuencias_acumuladas(frec_abs),
    Frecuencia_Relativa = frec_rel,
    Frecuencia_Relativa_Acum = crear_frecuencias_acumuladas(frec_rel)
  ))
  
  names(df)[1]<- nombre_variable
  return(df)
}

crear_tabla_continua <- function(source, columna, nombre_variable = "Variable") {
  datos <- source[[columna]]
  
  num_intervalos <- ceiling(1 + 3.322 * log10(length(datos)))
  
  min_valor <- floor(min(datos))
  max_valor <- ceiling(max(datos))
  
  ancho <- ceiling((max_valor - min_valor) / num_intervalos)
  
  limites <- seq(min_valor, max_valor, by = ancho)
  
  if(max(limites) < max_valor) {
    limites <- c(limites, max(limites) + ancho)
  }
  
  cortes <- cut(datos, breaks = limites, include.lowest = TRUE, right = FALSE)
  tabla <- table(cortes)
  
  frec_abs <- as.vector(tabla)
  frec_rel <- as.vector(round(prop.table(tabla), 4))
  
  df <- data.frame(
    Variable = names(tabla),
    Frecuencia_Absoluta = frec_abs,
    Frecuencia_Absoluta_Acum = crear_frecuencias_acumuladas(frec_abs),
    Frecuencia_Relativa = frec_rel,
    Frecuencia_Relativa_Acum = crear_frecuencias_acumuladas(frec_rel)
  )
  
  names(df)[1] <- nombre_variable
  return(df)
}

# Manejo de datos.
# Datos de satisfacción.

niveles<-read_excel(archivo, sheet = "nivel de satisfacción")
niveles <- data.frame(
  Nivel = niveles[[1]],          
  Descripcion = niveles[[2]]     
)

datos_temp<-datos
datos_temp$`SATISFACCIÓN CON LA CARRERA` <- niveles$Descripcion[match(datos$`SATISFACCIÓN CON LA CARRERA`, niveles$Nivel)]

nivel_orden <- c("Muy satisfecho", "Satisfecho", "Insatisfecho", "Muy insatisfecho")
datos_temp$`SATISFACCIÓN CON LA CARRERA` <- factor(
  datos_temp$`SATISFACCIÓN CON LA CARRERA`, 
  levels = nivel_orden
)

tabla_satisfaccion <- crear_tabla_discreta(
  datos_temp,
  "SATISFACCIÓN CON LA CARRERA",
  "Nivel de satisfacción"
)

print(tabla_satisfaccion)

# Datos de tiempo de estudio.

tabla_tiempo<-crear_tabla_continua(
  datos,
  "TIEMPO SEMANAL en HS. DEDIC. EST.",
  "Intervalos de tiempo"
)

print(tabla_tiempo)

# Funciones para medidas descriptivas.

medidas_cuantitativas <- function(source, columna) {
  datos <- source[[columna]]
  
  moda <- as.numeric(names(sort(table(datos), decreasing = TRUE)[1]))
  
  medidas <- c(
    Media = mean(datos, na.rm = TRUE),
    Mediana = median(datos, na.rm = TRUE),
    Moda = moda,
    Varianza = var(datos, na.rm = TRUE),
    Desvio_Estandar = sd(datos, na.rm = TRUE),
    Rango = max(datos, na.rm = TRUE) - min(datos, na.rm = TRUE),
    Coef_Variacion = sd(datos, na.rm = TRUE) / mean(datos, na.rm = TRUE)
  )
  
  # Cuartiles
  cuartiles <- quantile(datos, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  
  resultados <- data.frame(
    Medida = c(names(medidas), names(cuartiles)),
    Valor = round(c(medidas, cuartiles), 4),
    row.names = NULL
  )
  
  return(resultados)
}


medidas_cualitativas <- function(source, columna) {
  x <- source[[columna]]
  x <- x[!is.na(x)]  
  
  tabla <- table(x)
  
  moda <- names(tabla)[which.max(tabla)]
  
  x_num<-as.numeric(x)
  
  mediana_num <- quantile(x_num, probs = 0.5, type = 1)
  q1_num <- quantile(x_num, probs = 0.25, type = 1)
  q3_num <- quantile(x_num, probs = 0.75, type = 1)
  
  mediana <- levels(x)[mediana_num]
  q1 <- levels(x)[q1_num]
  q3 <- levels(x)[q3_num]
  
  resultados <- data.frame(
    Medida = c("Moda", "Mediana", "Q1", "Q3"),
    Valor = c(moda, mediana, q1, q3)
  )
  
  return(resultados)
}

res_satisfaccion <- medidas_cualitativas(datos_temp, "SATISFACCIÓN CON LA CARRERA")
print(res_satisfaccion)

res_tiempo <- medidas_cuantitativas(datos, "TIEMPO SEMANAL en HS. DEDIC. EST.")
print(res_tiempo)

# Graficos de variables.

# Histograma

brks <- c(1, 5, 9, 13, 17, 21, 25, 29, 33)
histograma_tiempo<-ggplot(data.frame(tiempo = datos[["TIEMPO SEMANAL en HS. DEDIC. EST."]]), aes(x = tiempo)) +
  geom_histogram(breaks = brks, closed = "right", fill = "skyblue", color = "black") +
  labs(title = "Histograma tiempos de estudio",
       x = "Tiempo (hrs.)",
       y = "Frecuencia")

# Diagrama Circular.

porcentajes<-c(tabla_satisfaccion[["Frecuencia_Relativa"]])
nombres<- c(tabla_satisfaccion[["Nivel de satisfacción"]])

df_satisfacción<- data.frame(nivel_de_Satisfacción = nombres, porcentaje = porcentajes)

diagrama_circular_satisfaccion<-ggplot(df_satisfacción, aes(x = "", y = porcentaje, fill = nivel_de_Satisfacción)) + geom_bar(stat = "identity", width = 1, color ="white") + coord_polar("y") + geom_text(aes(label = percent(porcentaje, accuracy = 0.1)), position = position_stack(vjust = 0.5)) + labs(title = "Nivel de Satisfacción", fill = "Nivel de Satisfacción") + theme_void()

print(res_tiempo)
print(res_satisfaccion)
print(niveles)
