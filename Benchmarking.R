#ANALISIS DEA DE LOS GORES con la libreria Benchmarking

install.packages("Benchmarking")

#Abriendo las librerias a usar
library(readxl)
library(deaR)
library(Benchmarking)
library(dplyr)

#verificación de la carpeta oficial
getwd()
#direccionamos la carpeta de datos

setwd("C:/Users/MIGUEL PAREDES/Desktop/INVESTIGACIÓN/2025/ARTURO - COMPLUTENCE -MAESTRIA")

#Revisando los archivos en la carpeta direccionada
list.files()

#Importar la base de datos
base<-read_xlsx('DATA_.xlsx', sheet='DEA')
head(base)
str(base)

#preparando las entradas y salidas
inputs=base[, c("PIM", "GPT")] %>% as.matrix()
outputs=base[, c("IC", "PC", "PI", "OA", "IDH")] %>% as.matrix()

#1-CON ORIENTACIÓN A LOS INPUTS
#CCR (CRS) con retornos constantes a escala
#para estimar la eficiencia técnica global
resultado_CCR_in <- dea(X = inputs, Y = outputs, RTS = "crs", ORIENTATION = "in")
summary(resultado_CCR_in)

#BCC (VRS) con retornos variables a escala
#para estimar la eficiencia técnica pura y analizar la influencia del tamaño de operación.
resultado_BCC_in <- dea(X = inputs, Y = outputs, RTS = "vrs", ORIENTATION = "in")
summary(resultado_BCC_in)

# GRÁFICO EFICIENCIA  CCR (ORIENTADA A INSUMOS)
efficiency_CCR_in <- resultado_CCR_in$eff
#agregado los nombres

regiones <- c("ANCASH","APURIMAC","AYACUCHO","CAJAMARCA","CALLAO","CUSCO","JUNIN",
              "LA LIBERTAD","LIMA","LORETO","MOQUEGUA","PASCO","PIURA","SAN MARTIN",
              "TACNA","TUMBES","UCAYALI")

# --- BARRAS (?? input-oriented) ---
par(mar = c(8, 4, 4, 10))  # espacio a la derecha ya no es tan necesario
bp <- barplot(
  efficiency_CCR_in,
  main = "Eficiencia técnica global (CCR, con retornos constantes a escala)",
  ylab = "Eficiencia (??)",
  col = "steelblue",
  names.arg = regiones,
  las = 2,
  ylim = c(0, 1.05)   # escala adecuada para ?????(0,1]
)

# Etiquetas de valor sobre las barras
text(x = bp, y = efficiency_CCR_in, labels = round(efficiency_CCR_in, 2),
     pos = 3, cex = 0.8, col = "black")

# --- LÍNEAS GUÍA ---
umbral_baja  <- 0.75
umbral_media <- 0.50
umbral_alta  <- 0.25

## Líneas horizontales y texto a la derecha
abline(h = 1, col = "green", lty = 2, lwd = 2)
text(x = par("usr")[2] + 1, y = 1, labels = "Eficiencia = 1.00", 
     col = "green", pos = 4, cex = 0.7, xpd = TRUE)

abline(h = 0.75, col = "orange", lty = 3, lwd = 2)
text(x = par("usr")[2] + 1, y = 0.75, labels = "Baja ineficiencia (?? = 0.75)", 
     col = "orange", pos = 4, cex = 0.7, xpd = TRUE)

abline(h = 0.50, col = "orange", lty = 3, lwd = 2)
text(x = par("usr")[2] + 1, y = 0.50, labels = "Ineficiencia media (?? = 0.50)", 
     col = "orange", pos = 4, cex = 0.7, xpd = TRUE)

abline(h = 0.25, col = "red", lty = 3, lwd = 2)
text(x = par("usr")[2] + 1, y = 0.25, labels = "Alta ineficiencia (?? = 0.25)", 
     col = "red", pos = 4, cex = 0.7, xpd = TRUE)


# GRÁFICO EFICIENCIA  BCC (ORIENTADA A INSUMOS)
efficiency_BCC_in <- resultado_BCC_in$eff

# Nombres de regiones
regiones <- c("ANCASH","APURIMAC","AYACUCHO","CAJAMARCA","CALLAO","CUSCO","JUNIN",
              "LA LIBERTAD","LIMA","LORETO","MOQUEGUA","PASCO","PIURA","SAN MARTIN",
              "TACNA","TUMBES","UCAYALI")

# --- BARRAS (?? input-oriented) ---
par(mar = c(8, 4, 4, 10))  
bp <- barplot(
  efficiency_BCC_in,
  main = "Eficiencia técnica pura (BCC, con retornos variables a escala)",
  ylab = "Eficiencia (??)",
  col = "steelblue",
  names.arg = regiones,
  las = 2,
  ylim = c(0, 1.05)   # escala adecuada para ?????(0,1]
)

# Etiquetas de valor sobre las barras
text(x = bp, y = efficiency_BCC_in, labels = round(efficiency_BCC_in, 2),
     pos = 3, cex = 0.8, col = "black")

# --- LÍNEAS GUÍA ---
umbral_baja  <- 0.75
umbral_media <- 0.50
umbral_alta  <- 0.25

# Límites horizontales del área de barras
x_min <- min(bp) - 0.5
x_max <- max(bp) + 0.5

# Dibujar líneas dentro del área del gráfico
segments(x_min, 1,        x_max, 1,        col="green",  lty=2, lwd=2)
segments(x_min, umbral_baja,  x_max, umbral_baja,  col="orange", lty=3, lwd=2)
segments(x_min, umbral_media, x_max, umbral_media, col="orange", lty=3, lwd=2)
segments(x_min, umbral_alta,  x_max, umbral_alta,  col="red",    lty=3, lwd=2)

# --- ETIQUETAS EXTERNAS A LA DERECHA ---
offset_x <- 0.05 * (par("usr")[2] - par("usr")[1])  # distancia hacia afuera

text(x_max + offset_x, 1, 
     "Eficiencia = 1.00", 
     col = "green", pos = 4, cex = 0.75, xpd = TRUE)

text(x_max + offset_x, umbral_baja, 
     "Baja ineficiencia (?? = 0.75)", 
     col = "orange", pos = 4, cex = 0.75, xpd = TRUE)

text(x_max + offset_x, umbral_media, 
     "Ineficiencia media (?? = 0.50)", 
     col = "orange", pos = 4, cex = 0.75, xpd = TRUE)

text(x_max + offset_x, umbral_alta, 
     "Alta ineficiencia (?? = 0.25)", 
     col = "red", pos = 4, cex = 0.75, xpd = TRUE)


#==============================================================================
#2-CON ORIENTACIÓN A LOS OUTPUTS 
# DEA-CCR (Retornos constantes a escala)
resultado_CCR = dea(X = inputs, Y = outputs, RTS = "crs", ORIENTATION = "out")
summary(resultado_CCR)

# DEA-BCC (Retornos variables a escala)
resultado_BCC <- dea(X = inputs, Y = outputs, RTS = "vrs", ORIENTATION = "out")
summary(resultado_BCC)

# Análisis gráfico de eficiencia
efficiency_CCR <- resultado_CCR$eff
barplot(efficiency_CCR, main="Eficiencia relativa (CCR)", ylab="Eficiencia", col="steelblue")

#agregado los nombres

regiones <- c("ANCASH","APURIMAC","AYACUCHO","CAJAMARCA","CALLAO","CUSCO","JUNIN",
              "LA LIBERTAD","LIMA","LORETO","MOQUEGUA","PASCO","PIURA","SAN MARTIN",
              "TACNA","TUMBES","UCAYALI")

efficiency_CCR <- resultado_CCR$eff
par(mar = c(8, 4, 4, 10)) # abajo, izq, arriba, der
bp=barplot(efficiency_CCR,
           main = "Eficiencia relativa (CCR)",
           ylab = "Eficiencia",
           col = "steelblue",
           names.arg = regiones,   # Nombres en el eje X
           las = 2,)               # Las = 2 ??? texto vertical para que no se monte

#mejorando el gráfico para su mayor comprensión
# Agregar líneas horizontales constantes
abline(h = 1,  col = "green",   lty = 2, lwd = 2)   # línea en eficiencia = 1
abline(h = 5,  col = "purple", lty = 3, lwd = 2)   # línea en 5
abline(h = 10, col = "blue",  lty = 3, lwd = 2)   # línea en 10
abline(h = 15, col = "red",lty = 3, lwd = 2)   # línea en 15

# Añadir valores de eficiencia sobre las barras
text(x = bp, 
     y = efficiency_CCR, 
     labels = round(efficiency_CCR, 2),  # dos decimales
     pos = 3,  # 3 = arriba de la barra
     cex = 0.8,  # tamaño del texto
     col = "black")

#leyenda
legend("topright",
       inset = c(-0.32, 0),  # mueve fuera del área del gráfico
       legend = c("Alta ineficiencia = 15",
                  "Media ineficiencia = 10",
                  "Baja ineficiencia = 5",
                  "Eficiencia = 1"),
       col    = c("red", "purple", "blue", "green"),
       lty    = c(2,3,3,3),
       lwd    = 2,
       bty    = "n",
       cex    = 0.8,        # tamaño del texto (1 = normal, <1 más pequeño)
       xpd    = TRUE)      # Permite dibujar fuera de la región del graph
