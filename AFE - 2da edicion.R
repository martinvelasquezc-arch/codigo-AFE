###############################################
# AFE Motivación académica - EAFIT
# Análisis de Datos 2
###############################################

########## 0. PAQUETES ##########

# install.packages("psych")
# install.packages("readr")

library(psych)
library(readr)

########## 1. IMPORTAR DATOS ##########

# El working directory ya lo definiste desde RStudio (Session > Set WD)
# Ajusta el nombre del archivo si es diferente:
datos <- read_delim("Resultados_Encuestas_AFE_Limpios.csv", delim = ";")

# Revisión rápida de la estructura
str(datos)
summary(datos)

# Nos quedamos solo con los ítems Likert para el AFE
items <- datos[, c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10")]
str(items)

########## 2. ADECUACIÓN PARA AFE (KMO y BARTLETT) ##########

# Medida de adecuación muestral KMO
KMO(items)

# Prueba de esfericidad de Bartlett
cortest.bartlett(cor(items), n = nrow(items))

########## 3. ANÁLISIS PARALELO (NÚMERO DE FACTORES) ##########

fa.parallel(items, fm = "minres", fa = "fa")

# Según el resultado de fa.parallel, se decidió usar 2 factores

########## 4. ANÁLISIS FACTORIAL EXPLORATORIO (AFE) ##########

resultado_afe <- fa(items,
                    nfactors = 2,
                    fm       = "minres",
                    rotate   = "varimax")

# Cargas factoriales ordenadas (con cutoff para simplificar la tabla)
print(resultado_afe, cutoff = 0.30, sort = TRUE)

########## 5. PUNTAJES FACTORIALES ##########

# Puntajes por estudiante en cada factor (MR1 y MR2)
puntajes <- as.data.frame(resultado_afe$scores)

# Agregar el semestre a la tabla de puntajes
puntajes$Semestre <- datos$Semestre

head(puntajes)
str(puntajes)

########## 6. CORRELACIONES CON SEMESTRE ##########

# Motivación intrínseca (MR1) vs semestre
cor.test(puntajes$Semestre, puntajes$MR1)

# Motivación extrínseca (MR2) vs semestre
cor.test(puntajes$Semestre, puntajes$MR2)

########## 7. ANÁLISIS POR GRUPOS DE SEMESTRE (ANOVA) ##########

# Grupos: 1–3 = Primeros, 4–6 = Intermedios, 7–10 = Avanzados
puntajes$Grupo <- cut(puntajes$Semestre,
                      breaks = c(0, 3, 6, 10),
                      labels = c("Primeros", "Intermedios", "Avanzados"))

table(puntajes$Grupo)

# ANOVA para motivación intrínseca
anova_MR1 <- aov(MR1 ~ Grupo, data = puntajes)
summary(anova_MR1)

# ANOVA para motivación extrínseca
anova_MR2 <- aov(MR2 ~ Grupo, data = puntajes)
summary(anova_MR2)

# Promedios por grupo (para interpretar)
aggregate(puntajes[, c("MR1","MR2")],
          by = list(Grupo = puntajes$Grupo),
          FUN = mean)

########## 8. GRÁFICAS (OPCIONALES PARA EL INFORME) ##########

# Boxplot de motivación intrínseca por grupo de semestre
boxplot(MR1 ~ Grupo, data = puntajes,
        main = "Motivación intrínseca por grupo de semestre",
        xlab = "Grupo de semestre",
        ylab = "Puntaje factorial (MR1)")

# Dispersión Semestre vs MR1 con recta de regresión
plot(puntajes$Semestre, puntajes$MR1,
     main = "Relación entre semestre y motivación intrínseca",
     xlab = "Semestre en curso",
     ylab = "Motivación intrínseca (MR1)",
     pch = 19)
abline(lm(MR1 ~ Semestre, data = puntajes), col = "red", lwd = 2)

