library(lavaan)
library(readxl)
library(psych)

# Cargar los datos desde el archivo Excel
datos <- read_excel("encuesta.xlsx")

# Seleccionar las columnas relevantes
variables <- datos[, c("motviaje", "frcuenc_viaj", "preferencia", "calidad", "precio_pasaje", "dispocion_pago", "tiempo_viaje", "nueva_empresa")]


# Definir el modelo de CFA
modelo_cfa <- '
  Factor1 =~ motviaje + frcuenc_viaj + preferencia
  Factor2 =~ calidad + precio_pasaje + dispocion_pago + tiempo_viaje + nueva_empresa
'

# Ajustar el modelo CFA
ajuste_cfa <- cfa(modelo_cfa, data = variables)

# Resumen del ajuste del modelo CFA
summary(ajuste_cfa, fit.measures = TRUE, standardized = TRUE)

# Realizar el análisis factorial exploratorio (EFA)
efa_result <- fa(variables, nfactors = 2, rotate = "varimax")

# Resumen del resultado EFA
print(efa_result)