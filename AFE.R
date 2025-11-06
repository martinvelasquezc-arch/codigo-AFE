
getwd()

library(readr)

datos <- read_delim("Resultados_Encuestas_AFE_Limpios.csv", delim = ";")

# Revisar las primeras filas
head(datos)

# Verificar estructura de las variables
str(datos)

# Seleccionamos solo las columnas de las preguntas (P1–P10)
items <- datos[, c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10")]

# Verificamos que todo esté en orden
str(items)

#//////

library(psych)

# 1. Medida de adecuación muestral KMO
KMO(items)

# 2. Prueba de esfericidad de Bartlett
cortest.bartlett(cor(items), n = nrow(items))

# 3. Análisis paralelo para decidir el número óptimo de factores
fa.parallel(items, fm = "minres", fa = "fa")

# 4. Ajuste del modelo AFE con 2 factores
resultado_afe <- fa(items,
                    nfactors = 2,
                    rotate   = "varimax",
                    fm       = "minres")

# 5. Mostrar los resultados
print(resultado_afe, cutoff = 0.3, sort = TRUE)

# 6. Crear puntajes factoriales para cada estudiante
puntajes <- as.data.frame(resultado_afe$scores)

# 7. Agregar la columna de Semestre
puntajes$Semestre <- datos$Semestre

# 8. Revisar las primeras filas
head(puntajes)

# Correlación Semestre – Factores (MR1 y MR2)

# 9. Correlación entre Semestre y Factor 1 (MR1)
cor.test(puntajes$Semestre, puntajes$MR1)

# 10. Correlación entre Semestre y Factor 2 (MR2)
cor.test(puntajes$Semestre, puntajes$MR2)

# ANOVA por grupos de semestre

# Crear grupos de semestre (1–3 = primeros, 4–6 = intermedios, 7–10 = avanzados)
puntajes$Grupo <- cut(puntajes$Semestre,
                      breaks = c(0, 3, 6, 10),
                      labels = c("Primeros", "Intermedios", "Avanzados"))

# Verifica que se haya creado correctamente
table(puntajes$Grupo)

# ANOVA para motivación intrínseca (MR1)
anova_MR1 <- aov(MR1 ~ Grupo, data = puntajes)
summary(anova_MR1)

# ANOVA para motivación extrínseca (MR2)
anova_MR2 <- aov(MR2 ~ Grupo, data = puntajes)
summary(anova_MR2)

aggregate(puntajes[, c("MR1","MR2")],
          by = list(Grupo = puntajes$Grupo),
          FUN = mean)


