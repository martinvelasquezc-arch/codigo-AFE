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
# MR1: P8, P5, P9, P4  -> Motivación intrínseca / compromiso personal
# MR2: P2, P3, P6      -> Motivación extrínseca / motivos externos

########## 5. PUNTAJES FACTORIALES Y NOMBRADO DE FACTORES ##########

# Extraigo los puntajes factoriales de cada estudiante
puntajes <- as.data.frame(resultado_afe$scores)

# Renombro los factores para facilitar la interpretación
# MR1 -> Motivacion_intrinseca
# MR2 -> Motivacion_extrinseca
colnames(puntajes)[1:2] <- c("Motivacion_intrinseca",
                             "Motivacion_extrinseca")

# Incorporo el semestre original a la tabla de puntajes
puntajes$Semestre <- datos$Semestre

# Revisión de la estructura final de la tabla de puntajes
str(puntajes)
head(puntajes)

########## 6. CORRELACIONES ENTRE SEMESTRE Y FACTORES ##########

# Correlación entre semestre y motivación intrínseca
cor.test(puntajes$Semestre, puntajes$Motivacion_intrinseca)

# Correlación entre semestre y motivación extrínseca
cor.test(puntajes$Semestre, puntajes$Motivacion_extrinseca)

########## 7. ANÁLISIS POR GRUPOS DE SEMESTRE (ANOVA) ##########

# Defino grupos de semestre:
# 1–3: Primeros, 4–6: Intermedios, 7–10: Avanzados
puntajes$Grupo <- cut(puntajes$Semestre,
                      breaks = c(0, 3, 6, 10),
                      labels = c("Primeros", "Intermedios", "Avanzados"))

# Reviso el tamaño de cada grupo
table(puntajes$Grupo)

# ANOVA para motivación intrínseca por grupos de semestre
anova_MI <- aov(Motivacion_intrinseca ~ Grupo, data = puntajes)
summary(anova_MI)

# ANOVA para motivación extrínseca por grupos de semestre
anova_ME <- aov(Motivacion_extrinseca ~ Grupo, data = puntajes)
summary(anova_ME)

# Calculo los promedios de cada factor por grupo de semestre
medias_grupo <- aggregate(puntajes[, c("Motivacion_intrinseca","Motivacion_extrinseca")],
                          by = list(Grupo = puntajes$Grupo),
                          FUN = mean)
medias_grupo

########## 8. GRÁFICAS PARA EL INFORME ##########

## 8.1 Mapa simple de correlaciones entre ítems (antes del AFE)

mat_cor <- cor(items)

# Mapa básico de la matriz de correlaciones
image(1:10, 1:10, mat_cor,
      main = "Matriz de correlaciones entre ítems de motivación",
      xlab = "Ítems", ylab = "Ítems",
      axes = FALSE)
axis(1, at = 1:10, labels = colnames(items))
axis(2, at = 1:10, labels = colnames(items))

## 8.2 Boxplots de factores por grupo de semestre

# Boxplot de motivación intrínseca por grupo de semestre
boxplot(Motivacion_intrinseca ~ Grupo, data = puntajes,
        main = "Motivación intrínseca por grupo de semestre",
        xlab = "Grupo de semestre",
        ylab = "Puntaje factorial (Motivación intrínseca)")

# Boxplot de motivación extrínseca por grupo de semestre
boxplot(Motivacion_extrinseca ~ Grupo, data = puntajes,
        main = "Motivación extrínseca por grupo de semestre",
        xlab = "Grupo de semestre",
        ylab = "Puntaje factorial (Motivación extrínseca)")

## 8.3 Dispersión de factores frente a semestre

# Dispersión de semestre frente a motivación intrínseca con recta de regresión
plot(puntajes$Semestre, puntajes$Motivacion_intrinseca,
     main = "Relación entre semestre y motivación intrínseca",
     xlab = "Semestre en curso",
     ylab = "Motivación intrínseca (puntaje factorial)",
     pch = 19)
abline(lm(Motivacion_intrinseca ~ Semestre, data = puntajes),
       col = "red", lwd = 2)

# Dispersión de semestre frente a motivación extrínseca con recta de regresión
plot(puntajes$Semestre, puntajes$Motivacion_extrinseca,
     main = "Relación entre semestre y motivación extrínseca",
     xlab = "Semestre en curso",
     ylab = "Motivación extrínseca (puntaje factorial)",
     pch = 19)
abline(lm(Motivacion_extrinseca ~ Semestre, data = puntajes),
       col = "red", lwd = 2)

## 8.4 Barplot de medias de factores por grupo de semestre

# Convierto las medias a matriz para el gráfico
mat_medias <- t(as.matrix(medias_grupo[, c("Motivacion_intrinseca",
                                           "Motivacion_extrinseca")]))
colnames(mat_medias) <- medias_grupo$Grupo

barplot(mat_medias,
        beside = TRUE,
        col = c("#4C72B0", "#DD8452"),   # Azul y naranja suave
        main = "Medias de motivación por grupo de semestre",
        xlab = "Grupo de semestre",
        ylab = "Media del puntaje factorial",
        ylim = c(-0.4, 0.5))
legend("topleft",
       legend = c("Motivación intrínseca", "Motivación extrínseca"),
       fill = c("#4C72B0", "#DD8452"),
       bty = "n")









