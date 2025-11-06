###############################################
# AFE Motivación académica - EAFIT
# Curso: Análisis de Datos 2
###############################################

########## 0. PAQUETES ##########

# install.packages("psych")
# install.packages("readr")

library(psych)
library(readr)

########## 1. IMPORTACIÓN Y PREPARACIÓN DE DATOS ##########

# El working directory se definió desde RStudio (Session > Set WD).
# El archivo CSV contiene:
# - Columna 'Semestre' en formato numérico
# - Columnas P1 a P10 codificadas en escala Likert de 1 a 5

datos <- read_delim("Resultados_Encuestas_AFE_Limpios.csv", delim = ";")

# Revisión general de la estructura de la base
str(datos)
summary(datos)

# Defino el objeto con los 10 ítems de motivación para el AFE
items <- datos[, c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10")]
str(items)

########## 2. ADECUACIÓN PARA AFE (KMO Y BARTLETT) ##########

# Calculo la medida de adecuación muestral KMO
KMO(items)

# Aplico la prueba de esfericidad de Bartlett sobre la matriz de correlaciones
cortest.bartlett(cor(items), n = nrow(items))

########## 3. ANÁLISIS PARALELO (DECISIÓN DEL NÚMERO DE FACTORES) ##########

# Utilizo el análisis paralelo para decidir el número óptimo de factores
fa.parallel(items, fm = "minres", fa = "fa")
# El análisis paralelo sugiere extraer 2 factores

########## 4. ANÁLISIS FACTORIAL EXPLORATORIO (AFE) ##########

# Ajusto el modelo de AFE con:
# - nfactors = 2 (según el análisis paralelo)
# - método de mínimos residuales (minres)
# - rotación Varimax (ortogonal)

resultado_afe <- fa(items,
                    nfactors = 2,
                    fm       = "minres",
                    rotate   = "varimax")

# Muestro las cargas factoriales ordenadas
# cutoff = 0.30 para simplificar la visualización
print(resultado_afe, cutoff = 0.30, sort = TRUE)

# Interpretación de factores según cargas:
# MR1: P8, P5, P9, P4  -> Inercia_abandono
#       (continuar por compromiso previo, menor interés, riesgo de abandono)
# MR2: P2, P3, P6      -> Motivacion_sostenida
#       (interés, disfrute, sentido y utilidad percibida)

########## 5. PUNTAJES FACTORIALES Y NOMBRADO DE FACTORES ##########

# Extraigo los puntajes factoriales de cada estudiante
puntajes <- as.data.frame(resultado_afe$scores)

# Renombro los factores según la nueva interpretación teórica
# MR1 -> Inercia_abandono
# MR2 -> Motivacion_sostenida
colnames(puntajes)[1:2] <- c("Inercia_abandono",
                             "Motivacion_sostenida")

# Incorporo el semestre original a la tabla de puntajes
puntajes$Semestre <- datos$Semestre

# Revisión de la estructura final de la tabla de puntajes
str(puntajes)
head(puntajes)

########## 6. CORRELACIONES ENTRE SEMESTRE Y FACTORES ##########

# Correlación entre semestre e inercia / riesgo de abandono
cor.test(puntajes$Semestre, puntajes$Inercia_abandono)

# Correlación entre semestre y motivación sostenida
cor.test(puntajes$Semestre, puntajes$Motivacion_sostenida)

########## 7. ANÁLISIS POR GRUPOS DE SEMESTRE (ANOVA) ##########

# Defino grupos de semestre:
# 1–3: Primeros, 4–6: Intermedios, 7–10: Avanzados
puntajes$Grupo <- cut(puntajes$Semestre,
                      breaks = c(0, 3, 6, 10),
                      labels = c("Primeros", "Intermedios", "Avanzados"))

# Reviso el tamaño de cada grupo
table(puntajes$Grupo)

# ANOVA para Inercia_abandono por grupos de semestre
anova_IA <- aov(Inercia_abandono ~ Grupo, data = puntajes)
summary(anova_IA)

# ANOVA para Motivacion_sostenida por grupos de semestre
anova_MS <- aov(Motivacion_sostenida ~ Grupo, data = puntajes)
summary(anova_MS)

# Calculo los promedios de cada factor por grupo de semestre
medias_grupo <- aggregate(puntajes[, c("Inercia_abandono","Motivacion_sostenida")],
                          by = list(Grupo = puntajes$Grupo),
                          FUN = mean)
medias_grupo

########## 8. GRÁFICAS PARA EL INFORME ##########

## 8.1 Mapa simple de correlaciones entre ítems (antes del AFE)

mat_cor <- cor(items)

image(1:10, 1:10, mat_cor,
      main = "Matriz de correlaciones entre ítems de motivación",
      xlab = "Ítems", ylab = "Ítems",
      axes = FALSE)
axis(1, at = 1:10, labels = colnames(items))
axis(2, at = 1:10, labels = colnames(items))

## 8.2 Boxplots de factores por grupo de semestre

# Boxplot de Inercia_abandono por grupo de semestre
boxplot(Inercia_abandono ~ Grupo, data = puntajes,
        main = "Inercia y riesgo de abandono por grupo de semestre",
        xlab = "Grupo de semestre",
        ylab = "Puntaje factorial (Inercia_abandono)")

# Boxplot de Motivacion_sostenida por grupo de semestre
boxplot(Motivacion_sostenida ~ Grupo, data = puntajes,
        main = "Motivación sostenida por grupo de semestre",
        xlab = "Grupo de semestre",
        ylab = "Puntaje factorial (Motivacion_sostenida)")

## 8.3 Dispersión de factores frente a semestre

# Dispersión Semestre vs Inercia_abandono
plot(puntajes$Semestre, puntajes$Inercia_abandono,
     main = "Relación entre semestre e inercia/abandono",
     xlab = "Semestre en curso",
     ylab = "Inercia y riesgo de abandono (puntaje factorial)",
     pch = 19)
abline(lm(Inercia_abandono ~ Semestre, data = puntajes),
       col = "red", lwd = 2)

# Dispersión Semestre vs Motivacion_sostenida
plot(puntajes$Semestre, puntajes$Motivacion_sostenida,
     main = "Relación entre semestre y motivación sostenida",
     xlab = "Semestre en curso",
     ylab = "Motivación sostenida (puntaje factorial)",
     pch = 19)
abline(lm(Motivacion_sostenida ~ Semestre, data = puntajes),
       col = "red", lwd = 2)

## 8.4 Barplot de medias de factores por grupo de semestre

mat_medias <- t(as.matrix(medias_grupo[, c("Inercia_abandono",
                                           "Motivacion_sostenida")]))
colnames(mat_medias) <- medias_grupo$Grupo

barplot(mat_medias,
        beside = TRUE,
        col = c("#4C72B0", "#DD8452"),
        main   = "Medias de factores de motivación por grupo de semestre",
        xlab   = "Grupo de semestre",
        ylab   = "Media del puntaje factorial",
        ylim   = c(-0.4, 0.5))
legend("topleft",
       legend = c("Inercia_abandono", "Motivacion_sostenida"),
       fill   = c("#4C72B0", "#DD8452"),
       bty    = "n")

