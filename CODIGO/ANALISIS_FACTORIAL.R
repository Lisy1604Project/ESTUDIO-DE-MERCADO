#CAREN LIZBETH QUISA MAMANI
#IX
#2024
####################
# ANALISIS FACTORIAL
####################
rm(list=ls())
par(mfrow=c(1,1))
# datos=read.delim("hatco-factorial.txt") # tipo txt
datos1 <- read.csv("encuesta.csv",head=T, sep=";", row.names=1)
head(datos1)
str(datos1)
head(datos1)
# Saber el numero de NAs por columna
colSums(is.na(datos1))
# Analisis descriptivo y Analisis de Correlacion
summary(datos1)
library(psych)
describe(datos1)
# Matriz de correlaciones
correlaciones <- round(cor(datos1),3)
correlaciones
library(psych)
corr.test(datos1)
#grafico de correlaciones
library(corrplot)
corrplot(cor(datos1), order = "hclust", tl.col='black', tl.cex=1)
library(psych)
cor.plot(cor(datos1))
#######################3
# Multicolinealidad
# Determinante de la matriz de correlaciones
# cercano a 0, indica alta multicolinealidad entre las variables.
# igual a 0 (matriz no singular)
det(correlaciones)
## [1] 0.002689087
# Supuesto de multicolinealidad test de esfericidad de Bartlett
# busca contrastar la hipotesis nula de que la matriz de correlaciones
# es igual a una matriz de identidad
# homogeneidad de varianzas
# Prueba de Esfericidad de Bartlett
library (psych)
cortest.bartlett(correlaciones,n=nrow(datos1))
# Indicador Kaiser-Meyer-Olkinn KMO y MSA
library(psych)
KMO(datos1)

# Autovalores y autovectores de la matriz de covarianzas de la muestra
aucor=eigen(correlaciones)
# Porcentajes de variacion explicada por cada componente
# nuestro ejemplo tioene 7 variables
Prop.Var=aucor$values/sum(aucor$values)*100
cumProp.var=cumsum(aucor$values/sum(aucor$values)*100)
porc=data.frame(Comp=1:60,Autovalor=round(aucor$values,3),
                Porc.Var=round(Prop.Var,3),Acum.Porc.Var=round(cumProp.var,3))
porc

# Calculos de Comunalidades y Matriz de Cargas Factoriales
mautov <- matrix(diag(aucor$values), ncol = ncol(datos1), nrow = ncol(datos1))
lamda <- aucor$vectors %*% sqrt(mautov)
hi <- lamda %*% t(lamda)

# Comunalidades
hi2 <- diag(hi)

# Especificidad de las primeras 2 dimensiones
him <- lamda[, 1:2] %*% t(lamda[, 1:2])

# Mostrar resultados de comunalidades y especificidad
resultados_comunalidades <- data.frame(
  Variable = names(datos1),
  Comunalidad = round(hi2, 3),
  Especificidad = round(diag(him), 3)
)
resultados_comunalidades
# Ajustar los márgenes internos y externos del área de trazado
par(mar = c(5, 4, 4, 2) + 0.1)  # Márgenes internos aumentados
par(oma = c(0, 0, 0, 0))  # Márgenes externos ajustados

# Gráfico de sedimentación (autovalores) con línea central visible
plot(1:length(aucor$values), aucor$values, type = "l", xlab = "Componentes", ylab = "Autovalores", ylim = c(0, max(aucor$values)))
abline(h = 1, col = "blue", lty = 2)  # Agregar línea en valor 1, color azul


# Matriz de Comunalidades
matriz_comunalidades <- data.frame(
  Variable = names(datos1),
  Inicial = round(hi2, 3),
  Extraccion = round(diag(him), 3)
)
matriz_comunalidades
# componentes con stats
factanal(datos1, factors = 3, rotation = "none")

facto1= factanal(datos1,factors=3,method="mle")
facto1

# puntuaciones factoriales tomando factanal
puntuaciones <- factanal(datos1, factors = 3, rotation = "none", scores =
                           "regression")$scores
puntuaciones_fact <- cbind(datos1, puntuaciones)
head(puntuaciones_fact)

library(ade4)
acp=dudi.pca(datos1,scannf=FALSE,nf=ncol(datos1))
acp$co

s.corcircle(datos1,grid=FALSE)
s.corcircle(acp$co)
# funcion principal
facto=principal(r=datos1, nfactors=3,rotate="none")
facto$values

# Ajustar los márgenes del área de trazado
par(mar = c(3, 3, 2, 2))  # Márgenes internos ajustados

# Crear una nueva figura con tamaño específico
png("valores_propios.png", width = 800, height = 600)  # Ajustar el tamaño según sea necesario
# Graficar valores propios
plot(facto$values, type = "h", main = "Gráfico de Valores Propios", xlab = "Componentes", ylab = "Autovalores")
# Añadir etiquetas a los valores propios
text(facto$values, labels = round(facto$values, 2), pos = 3, cex = 0.8)
# Añadir etiquetas a los ejes
mtext("Valores Propios", side = 2, line = 3)
mtext("Componentes", side = 1, line = 3)
# Guardar el gráfico
dev.off()  # Finaliza la salida gráfica



facto$communality # Comunalidades
facto$loadings # Cargas Factoriales, Correlaciones Factor,Componente

#rotacion
facto=principal(r=datos1,nfactors=3,rotate="varimax")
facto
facto$values
