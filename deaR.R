#ANALISIS DEA DE LOS GORES

#Abriendo las librerias a usar
library(readxl)
library(deaR)

#verificación de la carpeta oficial
getwd()
#direccionamos la carpeta de datos

setwd("C:/Users/MIGUEL PAREDES/Desktop/INVESTIGACIÓN/2025/ARTURO - COMPLUTENCE -MAESTRIA")

#Revisando los archivos en la carpeta direccionada
list.files()

#Importar la base de datos
base<-read_xlsx('DATA_.xlsx', sheet='DEA_1')
head(base)
str(base)

#asignar o generar el modelo DEA
db=make_deadata(base, dmus= 1, ni= 2, no= 5)
db

#Primer calculo (eficiencia global)
result_crs <- deaR::model_basic(db, orientation = "io", rts = "crs")

#Calculo de la eficiencia global
eff_CRS_io <- deaR::efficiencies(result_crs)
eff_CRS_io

#Se aprecia que solo la region Lima alcanzó la eficiencia global

# Slacks y targets en deaR
sl_CRS <- deaR::slacks(result_crs)
tg_CRS <- deaR::targets(result_crs)
sl_CRS
tg_CRS

# Evitar notación científica
options(scipen = 999)

# Analizar el nivel de proporcionalidad en la relación de los imput y output 
RTS_VRS<-deaR::rts(result_crs)
RTS_VRS

#Eficiencia tecnica (se calcula con retornos variables)
result_vrs <- deaR::model_basic(db, orientation = "io", rts = "vrs")
eff_VRS_io <- deaR::efficiencies(result_vrs)
eff_VRS_io

# Slacks y targets en deaR
sl_VRS <- deaR::slacks(result_vrs)
tg_VRS <- deaR::targets(result_vrs)
sl_VRS
tg_VRS

## =========================
## RESÚMENES Y VISUALIZACIÓN
## =========================

# Nombres de DMU directamente desde el objeto deaR
DMU <- base[[1]]  # primera columna de tu data frame original

## --- Eficiencias y RTS ---
Eff_CRS_io <- deaR::efficiencies(result_crs)   # CCR/CRS (input-oriented)
Eff_VRS_io <- deaR::efficiencies(result_vrs)  # BCC/VRS (input-oriented)
Eff_scale <- eff_CRS_io / eff_VRS_io    # eficiencia de escala
RTS_VRS <- deaR::rts(result_crs)           # "irs", "drs" o "crs" bajo VRS

## --- Slacks y Targets (CRS y VRS) ---
sl_CRS <- deaR::slacks(result_crs)
sl_VRS <- deaR::slacks(result_vrs)

tg_CRS <- deaR::targets(result_crs)
tg_VRS <- deaR::targets(result_vrs)

# Convertir a data.frame y redondear
sl_in_CRS  <- as.data.frame(round(sl_CRS$slack_input,  2))
sl_out_CRS <- as.data.frame(round(sl_CRS$slack_output, 2))
sl_in_VRS  <- as.data.frame(round(sl_VRS$slack_input,  2))
sl_out_VRS <- as.data.frame(round(sl_VRS$slack_output, 2))

tg_in_CRS  <- as.data.frame(round(tg_CRS$target_input,  2))
tg_out_CRS <- as.data.frame(round(tg_CRS$target_output, 2))
tg_in_VRS  <- as.data.frame(round(tg_VRS$target_input,  2))
tg_out_VRS <- as.data.frame(round(tg_VRS$target_output, 2))

# Renombrar columnas para claridad (opcional)
names(sl_in_CRS)  <- paste0("s_in_",  names(sl_in_CRS))
names(sl_out_CRS) <- paste0("s_out_", names(sl_out_CRS))
names(sl_in_VRS)  <- paste0("s_in_",  names(sl_in_VRS))
names(sl_out_VRS) <- paste0("s_out_", names(sl_out_VRS))

names(tg_in_CRS)  <- paste0("t_in_",  names(tg_in_CRS))
names(tg_out_CRS) <- paste0("t_out_", names(tg_out_CRS))
names(tg_in_VRS)  <- paste0("t_in_",  names(tg_in_VRS))
names(tg_out_VRS) <- paste0("t_out_", names(tg_out_VRS))

# ==========================
# TABLA RESUMEN POR CADA DMU
# ==========================
resumen_DMU <- data.frame(
  DMU = DMU,
  Eff_CRS_io = round(eff_CRS_io, 2),
  Eff_VRS_io = round(eff_VRS_io, 2),
  Eff_Escala = round(Eff_scale, 2),
  RTS_VRS = RTS_VRS,
  check.names = FALSE
)
print(resumen_DMU)


# ===================================
# TABLAS POR MÉTRICA (SLACKS/TARGETS)
# ===================================
# Une slacks/targets a nivel DMU (puedes exportar como anexos)
res_slacks_CRS <- data.frame(DMU, sl_in_CRS, sl_out_CRS, check.names = FALSE)
res_slacks_VRS <- data.frame(DMU, sl_in_VRS, sl_out_VRS, check.names = FALSE)

res_targets_CRS <- data.frame(DMU, tg_in_CRS, tg_out_CRS, check.names = FALSE)
res_targets_VRS <- data.frame(DMU, tg_in_VRS, tg_out_VRS, check.names = FALSE)

# Exportar si quieres dejar listo para anexos
write.csv(resumen_DMU,     "DEA_resumen_DMU.csv",     row.names = FALSE)
write.csv(res_slacks_CRS,  "DEA_slacks_CRS.csv",      row.names = FALSE)
write.csv(res_slacks_VRS,  "DEA_slacks_VRS.csv",      row.names = FALSE)
write.csv(res_targets_CRS, "DEA_targets_CRS.csv",     row.names = FALSE)
write.csv(res_targets_VRS, "DEA_targets_VRS.csv",     row.names = FALSE)

# ==========
# GRÁFICOS
# ==========
options(scipen = 999)
op <- par(no.readonly = TRUE); on.exit(par(op))

# 1) Eficiencia técnica total (CRS, input-oriented)
par(mar = c(9,4,4,2))
bp1 <- barplot(eff_CRS_io,
               names.arg = DMU, las = 2,
               main = "Eficiencia técnica total (CRS, orientación a insumos)",
               ylab = "Eficiencia (??)",
               col = "steelblue")
abline(h = 1, lty = 2)
text(x = bp1, y = eff_CRS_io, labels = round(eff_CRS_io, 2), pos = 3, cex = 0.8)

# 2) Eficiencia técnica pura (VRS, input-oriented)
par(mar = c(9,4,4,2))
bp2 <- barplot(eff_VRS_io,
               names.arg = DMU, las = 2,
               main = "Eficiencia técnica pura (VRS, orientación a insumos)",
               ylab = "Eficiencia (??)",
               col = "skyblue")
abline(h = 1, lty = 2)
text(x = bp2, y = eff_VRS_io, labels = round(eff_VRS_io, 2), pos = 3, cex = 0.8)

# 3) Eficiencia de escala (CRS/VRS)
par(mar = c(9,4,4,2))
bp3 <- barplot(Eff_scale,
               names.arg = DMU, las = 2,
               main = "Eficiencia de escala (CRS / VRS)",
               ylab = "Índice",
               col = "darkseagreen2")
abline(h = 1, lty = 2)
text(x = bp3, y = Eff_scale, labels = round(Eff_scale, 2), pos = 3, cex = 0.8)

# 4) Distribución de retornos a escala (VRS)
par(mar = c(5,4,4,2))
tbl_rts <- table(RTS_VRS)  # cuenta "irs", "drs", "crs"
barplot(tbl_rts,
        main = "Distribución de retornos a escala (VRS)",
        ylab = "Número de DMUs",
        col = "tan")

etiquetas <- c("IRS" = "Retornos Crecientes",
               "DRS" = "Retornos Decrecientes",
               "CRS" = "Retornos Constantes")

tbl_rts <- table(RTS_VRS)
names(tbl_rts) <- etiquetas[names(tbl_rts)]

# Paleta de colores agradable
colores <- c("steelblue", "tomato", "seagreen")

par(mar = c(6, 5, 4, 2)) # margenes más amplios para etiquetas
bp <- barplot(tbl_rts,
              main = "Distribución de Retornos a Escala (VRS)",
              ylab = "Número de Gobiernos Regionales",
              col = colores,
              ylim = c(0, max(tbl_rts) + 2),
              las = 2,  # texto vertical para etiquetas
              cex.names = 0.9)

# Añadir valores encima de cada barra
text(x = bp, y = tbl_rts, label = tbl_rts, pos = 3, cex = 0.9, font = 2)

# Añadir línea de referencia opcional
abline(h = seq(0, max(tbl_rts), by = 1), col = "gray90", lty = 3)
