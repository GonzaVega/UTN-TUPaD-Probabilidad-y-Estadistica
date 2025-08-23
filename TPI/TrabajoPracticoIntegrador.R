if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
} else {
  library(readxl)
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
  # Extraer los datos de la columna
  datos <- source[[columna]]
  
  # Calcular número de intervalos usando regla de Sturges
  num_intervalos <- ceiling(1 + 3.322 * log10(length(datos)))
  
  # Encontrar el mínimo y máximo redondeando a enteros
  min_valor <- floor(min(datos))
  max_valor <- ceiling(max(datos))
  
  # Calcular el ancho de intervalo (redondeado hacia arriba para tener intervalos enteros)
  ancho <- ceiling((max_valor - min_valor) / num_intervalos)
  
  # Crear límites con números enteros
  limites <- seq(min_valor, max_valor, by = ancho)
  # Asegurar que el último valor esté incluido
  if(max(limites) < max_valor) {
    limites <- c(limites, max(limites) + ancho)
  }
  
  # Crear los cortes y la tabla de frecuencias
  cortes <- cut(datos, breaks = limites, include.lowest = TRUE, right = FALSE)
  tabla <- table(cortes)
  
  # Calcular frecuencias
  frec_abs <- as.vector(tabla)
  frec_rel <- as.vector(round(prop.table(tabla), 4))
  
  # Crear data frame con los resultados
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
