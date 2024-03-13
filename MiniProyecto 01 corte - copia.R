#########################################################################################
#Universidad Autónoma de Occidente
#MINIPROYECTO 1 CORTE
########################################################################################

install.packages("openxlsx")
install.packages("xtable")
install.packages("ggplot2")

#Cargar librerias
library(openxlsx)
library(xtable)
library(ggplot2)
library(tidyr)
library(dplyr)
library(psych)
library(e1071) # Funciones de skewness y kurtosis
library(readr)
library(tidyverse)
library(GGally)
library(patchwork)
library(VIM)# imputacion de datos knn
library(superheat) #graficar mapa valores faltantes
library(readxl)


#Fijar el directorio de trabajo
setwd("P:/Estadistica y probabilidad 2/Miniproyecto")

# Listar los archivos en el directorio de trabajo
dir()

#PUNTO 1
#Cargar los datos
df1 <- read_excel("Datos_MP1.xlsx", 
                   sheet = "Datos_1")
head(df1)
write.xlsx(df1, "Tabla datos 1.xlsx")

#Agregacion de nueva columna con total de ninos
df1$TotalNinos <- df1$Frec..Presente + df1$Frec..Ausente
head(df1)
write.xlsx(df1, "Tabla datos totalninos.xlsx")

#Tabla con estadictica descrptiva
Tabla_Descriptiva = df1 %>%
  select(Alcohol, Frec..Presente, Frec..Ausente, TotalNinos) %>%
  psych::describe(quant=c(.25,.75)) %>%
  as_tibble(rownames="rowname")  %>%
  print()
write.xlsx(Tabla_Descriptiva, "Tabla_Descriptiva.xlsx")

#Tabla de frecuencias
df <- data.frame(
  Alcohol = c(0.0, 0.5, 1.5, 4.0, 7.0),
  Frec..Presente = c(48, 38, 5, 1, 1),
  Frec..Ausente = c(17066, 14464, 788, 126, 37),
  TotalNinos = c(17114, 14502, 793, 127, 38))

df <- df %>%
  mutate(
    Frec_Rel_Presente = Frec..Presente / TotalNinos,
    Frec_Rel_Ausente = Frec..Ausente / TotalNinos)
print(df)
write.xlsx(df, "Tabla Frecuencias.xlsx")

#Generacion de un Dataset general de los datos
alcohol_presente <- rep(df1$Alcohol, df1$Frec..Presente)
estado_presente <- rep("Presente", times = sum(df1$Frec..Presente))
alcohol_ausente <- rep(df1$Alcohol, df1$Frec..Ausente)
estado_ausente <- rep("Ausente", times = sum(df1$Frec..Ausente))

datos_replicados <- data.frame(
  Alcohol = c(alcohol_presente, alcohol_ausente),
  Estado = c(estado_presente, estado_ausente))
head(datos_replicados, n = 10)
write.xlsx(datos_replicados, "Tabla general.xlsx")

#Estadistica Descriptiva
media <- mean(datos_replicados$Alcohol)
mediana <- median(datos_replicados$Alcohol)
moda <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]}
moda_alcohol <- moda(datos_replicados$Alcohol)

print(paste("Media: ", media))
print(paste("Mediana: ", mediana))
print(paste("Moda: ", moda_alcohol))

#Varianza
varianza_alcohol <- var(datos_replicados$Alcohol)
print(varianza_alcohol)

#Desviacion estandar y IQR
datos_replicados$Alcohol <- as.numeric(as.character(datos_replicados$Alcohol))
desviacion_estandar <- sd(datos_replicados$Alcohol)
rango_intercuartil <- IQR(datos_replicados$Alcohol)

print(desviacion_estandar)
print(rango_intercuartil)

comparacion <- data.frame(
  Medida = c("Media", "Mediana", "Moda","varianza", "Desviacion Estandar", "IQR"),
  Valor = c(media, mediana, moda_alcohol,varianza_alcohol, desviacion_estandar, rango_intercuartil))
print(comparacion)
write.xlsx(comparacion, "Tabla comparacion.xlsx")

#Graficar barras con los valores de Frec presente y Ausente para ver la diferencia de valores
conteos <- table(datos_replicados$Alcohol, datos_replicados$Estado)
conteos_df <- as.data.frame(conteos)
names(conteos_df) <- c("Alcohol", "Estado", "Frecuencia")
# Gráfico de barras
ggplot(conteos_df, aes(x = Alcohol, y = Frecuencia, fill = Estado)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Nivel de Alcohol", y = "Cantidad de Casos", 
       fill = "Estado", title = "Cantidad de Casos Presente y Ausente por Nivel de Alcohol") +
  theme_minimal()

# Gráfico de dispersión
ggplot(datos_replicados, aes(x = Alcohol, y = Estado)) + 
  geom_jitter(aes(color = Estado), width = 0.1) + 
  theme_minimal() + 
  labs(x = "Niveles de Alcohol", y = "Estado", title = "Relación entre Niveles de Alcohol y Estado") +
  theme(legend.title = element_blank())


# Graficar la proporcionalidad de tener malformaciones
df1$ProporcionMalf <- df1$Frec..Presente / df1$TotalNinos
imagen1 <- ggplot(df1, aes(x = Alcohol, y = ProporcionMalf)) +
  geom_point() +
  labs(title = "Proporcion de Malformaciones por Alcohol Ponderado",
       x = "Alcohol",
       y = "Proporcion de Malformacones") + 
  theme_minimal() 
ggsave("na_plot.png", imagen1, width = 6, height = 4, dpi = 300)
print(imagen1)

# porcentaje de casos con malformaciones frente al total de casos con y sin malformaciones por cada nivel de consumo de alcohol
df1$Porcentaje_Malformaciones <- (df1$Frec..Presente / df1$TotalNinos) * 100
tabla_porcentajes <- df1[, c('Alcohol', 'Frec..Presente', 'TotalNinos', 'Porcentaje_Malformaciones')]
print(tabla_porcentajes)
write.xlsx(tabla_porcentajes, "Tabla Porcentajes.xlsx")

print(df1)
write.xlsx(df1, "Tabla final df1.xlsx")
# 1.A. CONTRUCCION DE LOS MODELOS
#Construccion del modelo lineal
#vectores
alcohol <- c(0.0, 0.5, 1.5, 4.0, 7.0)
ProporcionMalf <- c(0.002804721, 0.002620328, 0.006305170, 0.007874016, 0.026315789)

modelo_lineal <- lm(ProporcionMalf ~ alcohol)
summary(modelo_lineal)
coeficientes <- summary(modelo_lineal)$coefficients
estimaciones <- as.data.frame(coeficientes)
write.xlsx(estimaciones, "Tabla summary lineal.xlsx")


#Construccion del modelo regresión logística generalizado
#vectores
y <- c(48, 38, 5, 1, 1) # Frec.Presente
n <- c(17114, 14502, 793, 127, 38) # TotalNiños
x <- c(0.0, 0.5, 1.5, 4.0, 7.0) 
n_y <- n - y # Frec.Ausente
#matriz de respuesta con 'y' y 'n - y'
ninos <- cbind(y, n_y)

#family=binomial, modelando datos de respuesta binaria
glm <- glm(ninos ~ x, family = binomial(link = "logit"))
summary(glm)
coeficientes <- summary(glm)$coefficients
estimaciones <- as.data.frame(coeficientes)
write.xlsx(estimaciones, "Tabla summary logisitco.xlsx")

odds_logit <- exp(coef(glm))
print(odds_logit)

#Construccion del modelo Probit
glm2=glm(ninos~x, family=binomial(link="probit"))
summary(glm2)
coeficientes <- summary(glm2)$coefficients
estimaciones <- as.data.frame(coeficientes)
write.xlsx(estimaciones, "Tabla summary probit.xlsx")


# Calcular AIC y BIC para cada modelo
aic_linear <- AIC(modelo_lineal)
bic_linear <- BIC(modelo_lineal)
aic_logit <- AIC(glm)
bic_logit <- BIC(glm)
aic_probit <- AIC(glm2)
bic_probit <- BIC(glm2)

# Calcular Pseudo R2 para modelos logit y probit
pseudo_r2_logit <- 1 - (glm$deviance / glm$null.deviance)
pseudo_r2_probit <- 1 - (glm2$deviance / glm2$null.deviance)

# Crear un dataframe para comparar los modelos
comparison <- data.frame(
  Modelo = c("Lineal", "Logit", "Probit"),
  AIC = c(aic_linear, aic_logit, aic_probit),
  BIC = c(bic_linear, bic_logit, bic_probit),
  Pseudo_R2 = c(NA, pseudo_r2_logit, pseudo_r2_probit))
print(comparison)
write.xlsx(comparison, "Comparacion_aic.xlsx")

#comparacion 3 modelos los valores predichos por cada modelo
pred_lineal <- predict(modelo_lineal)* n
pred_logistic <- predict(glm, type = "response") * n
pred_probit <- predict(glm2, type = "response") * n

comparacion <- data.frame(
  Alcohol = x,
  Observed = y,
  Lineal = pred_lineal,
  Logistic = pred_logistic,
  Probit = pred_probit)
print(comparacion)
write.xlsx(comparacion, "Comparacion_Modelos.xlsx")

# columnas Alcohol, Observed, Lineal, Logistic, Probit
datos <- data.frame(
  Alcohol = c(0, 0.5, 1.5, 4, 7),
  Observed = c(48, 38, 5, 1, 1),
  Lineal = c(34.3620178, 31.33086053, 25.26854599, 10.11275964, -8.074183976),
  Logistic = c(44.01874897, 43.67800161, 3.274173133, 1.15126482, 0.877811459),
  Probit = c(43.78077572, 43.91408659, 3.336758511, 1.159055292, 0.802701103))
datos_long <- reshape2::melt(datos, id.vars = 'Alcohol', variable.name = 'Modelo', value.name = 'Valor')

p <- ggplot(datos_long, aes(x = Alcohol, y = Valor, colour = Modelo)) +
  geom_point(data = subset(datos_long, Modelo == "Observed"), size = 3) +
  geom_line(aes(group = Modelo)) +
  scale_colour_manual(values = c("black", "red", "blue", "green")) +
  labs(title = "Comparación de Modelos de Regresión", x = "Niveles de Alcohol", y = "Frecuencia (Predicción vs Observada)") +
  theme_minimal()
print(p)
ggsave("comparacion_modelos.png", plot = p, width = 10, height = 6, dpi = 300)


# Crear un dataframe con los datos de proporción de malformaciones
df_proporcion <- data.frame(
  Alcohol = c(0.0, 0.5, 1.5, 4.0, 7.0),
  ProporcionMalf = c(0.002804721, 0.002620328, 0.006305170, 0.007874016, 0.026315789))

df_modelos <- data.frame(
  Alcohol = rep(c(0.0, 0.5, 1.5, 4.0, 7.0), 4),
  Modelo = rep(c("Lineal", "Logit", "Probit", "Real"), each = 5),
  Prediccion = c(
    predict(modelo_lineal, newdata = df_proporcion), 
    predict(glm, type = "response", newdata = data.frame(x = df_proporcion$Alcohol)),
    predict(glm2, type = "response", newdata = data.frame(x = df_proporcion$Alcohol)),
    df_proporcion$ProporcionMalf ) )

p1 <- ggplot(df_modelos, aes(x = Alcohol, y = Prediccion, colour = Modelo)) +
  geom_point(size = 3) +
  geom_line(aes(group = Modelo), data = df_modelos[df_modelos$Modelo != "Real", ]) +  # Excluir los datos reales para la línea
  scale_colour_manual(values = c("black", "red", "purple", "lightgreen")) +
  labs(title = "Comparación de Modelos por Alcohol y Proporción de Malformaciones",
       x = "Nivel de Alcohol",
       y = "Proporción de Malformaciones") +
  theme_minimal()
print(p1)

#########################################################################################
#PUNTO 2
datos <- read_excel("Datos_MP1.xlsx", 
                  sheet = "Datos_2")
head(datos)
# Codificar la variable 'programa' como categórica numérica
datos$programa_cod <- as.numeric(factor(datos$programa, levels = c('General', 'Academico', 'Vocacional')))

# Resumen estadístico básico para todas las variables
summary(datos)

# Análisis de valores faltantes
sum(is.na(datos))

#Calcular la asimetría para entender la distribución
skewness_puntaje_mat <- skewness(datos$puntaje_mat, na.rm = TRUE)

print(skewness_puntaje_mat)

# Calcular la mediana de puntaje_mat excluyendo NA
mediana_puntaje_mat <- median(datos$puntaje_mat, na.rm = TRUE)

# Imputar los NA en puntaje_mat con la mediana
datos$puntaje_mat[is.na(datos$puntaje_mat)] <- mediana_puntaje_mat

# Verificar la imputación
summary(datos$puntaje_mat)

# Verificar que no hayan NAs tras la imputación
n_na_post_imputacion <- sum(is.na(datos$puntaje_mat))
print(paste("Número de NA tras la imputación:", n_na_post_imputacion))

# Escribir los datos a un nuevo archivo Excel
write.xlsx(datos, "C:/Users/USUARIO/Desktop/Datos_Modificados.xlsx")

# Establecer la ruta del archivo
file_path <-("C:/Users/USUARIO/Desktop/Datos_Modificados.xlsx")

# Leer los datos del archivo de Excel
data <- read_excel(file_path)

# Obtener un resumen descriptivo solo de las columnas numéricas
summary <- data %>%
  summarise(across(where(is.numeric), list(
    mean = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE),
    min = ~min(.x, na.rm = TRUE),
    '25%' = ~quantile(.x, 0.25, na.rm = TRUE),
    median = ~median(.x, na.rm = TRUE),
    '75%' = ~quantile(.x, 0.75, na.rm = TRUE),
    max = ~max(.x, na.rm = TRUE)
  )))

# Imprimir el resumen
print(summary)


# Análisis de la variable num_premios (Distribución)
ggplot(data, aes(x=num_premios)) + 
  geom_histogram(binwidth = 1, fill="blue", color="black") +
  theme_minimal() +
  labs(title="Distribución de num_premios", x="Número de Premios", y="Frecuencia")

# Análisis de la variable puntaje_mat (Distribución)
ggplot(data, aes(x=puntaje_mat)) + 
  geom_histogram(fill="green", color="black", bins=30) +
  theme_minimal() +
  labs(title="Distribución de puntaje_mat", x="Puntaje de Matemáticas", y="Frecuencia")

# Análisis de la variable categórica programa
ggplot(data, aes(x=programa)) + 
  geom_bar(fill="orange", color="black") +
  theme_minimal() +
  labs(title="Distribución de programa", x="Programa", y="Conteo")

# Boxplot para puntaje_mat por programa para explorar distribuciones y posibles outliers
ggplot(data, aes(x=programa, y=puntaje_mat, fill=programa)) + 
  geom_boxplot() +
  theme_minimal() +
  labs(title="Puntaje de Matemáticas por Programa", x="Programa", y="Puntaje Mat")

# Relación entre puntaje_mat y num_premios
ggplot(data, aes(x=puntaje_mat, y=num_premios)) + 
  geom_point(alpha=0.5, color="red") + 
  theme_minimal() +
  labs(title="Relación entre Puntaje Matemáticas y Número de Premios", x="Puntaje de Matemáticas", y="Número de Premios")

# Gráfico de dispersión con color por programa
ggplot(data, aes(x=puntaje_mat, y=num_premios, color=programa)) + 
  geom_jitter(alpha=0.5) + 
  theme_minimal() +
  labs(title="Relación entre Puntaje Matemáticas y Número de Premios por Programa",
       x="Puntaje de Matemáticas", y="Número de Premios") +
  scale_color_brewer(type="qual", palette="Dark2") +
  theme(legend.title = element_text(face = "bold"))

# Histograma de Puntaje en Matemáticas
ggplot(data, aes(x = puntaje_mat)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 5, fill = "blue", color = "black") +
  geom_density(color = "red") +
  labs(title = "Distribución del Puntaje en Matemáticas",
       x = "Puntaje en Matemáticas",
       y = "Densidad") +
  theme_minimal()


# Boxplot de Número de Premios
ggplot(data, aes(y = num_premios)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Distribución del Número de Premios",
       y = "Número de Premios") +
  theme_minimal()

### Inicio del modelo de poisson

library(ggplot2)
library(dplyr)

# Establecer la ruta del archivo
file_path <-("C:/Users/USUARIO/Desktop/Datos_Modificados.xlsx")

# Leer los datos del archivo de Excel
datos <- read_excel(file_path)


# Establecer una semilla para reproducibilidad
set.seed(42)

# Seleccionar una muestra aleatoria del 50% de los datos sin reposición
indices <- sample(1:nrow(datos), size = 0.5 * nrow(datos))
datos_muestra <- datos[indices, ]

# No es necesario añadir una constante explícitamente en R para modelos lineales generalizados

# Ajustar el modelo de regresión de Poisson a la muestra
modelo_poisson_muestra <- glm(num_premios ~ puntaje_mat + factor(programa_cod), 
                              family = poisson(link = "log"), data = datos_muestra)


# Mostrar el resumen del modelo de Poisson ajustado a la muestra
summary(modelo_poisson_muestra)

# 1. Crear un nuevo data frame para las predicciones
pred_data <- expand.grid(puntaje_mat = seq(min(datos$puntaje_mat), max(datos$puntaje_mat), length.out = 100),
                         programa_cod = factor(1:3))

# Calcular las predicciones del modelo
pred_data$pred_premios <- predict(modelo_poisson_muestra, newdata = pred_data, type = "response")

# Para visualizar correctamente, asignamos etiquetas aquí sin alterar el modelo
pred_data$programa_etiqueta <- factor(pred_data$programa_cod, levels = 1:3, labels = c("General", "Académico", "Vocacional"))

# Gráfico de los efectos de las variables predictoras
ggplot(pred_data, aes(x = puntaje_mat, y = pred_premios, color = programa_etiqueta)) +
  geom_line() +
  labs(x = "Puntaje de Matemáticas", y = "Número Esperado de Premios", color = "Programa") +
  theme_minimal() +
  ggtitle("Efecto del Puntaje de Matemáticas y el Programa en el Número Esperado de Premios")

# 2. Gráfico de dispersión con líneas de predicción
# Para usar geom_smooth con glm, necesitamos asegurarnos de que programa_cod en datos_muestra sea factor
datos_muestra$programa_cod <- factor(datos_muestra$programa_cod)

ggplot(datos_muestra, aes(x = puntaje_mat, y = num_premios, color = factor(programa_cod))) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = poisson(link = "log")), se = FALSE) +
  labs(x = "Puntaje de Matemáticas", y = "Número de Premios", color = "Programa") +
  theme_minimal() +
  ggtitle("Dispersión de Premios y Líneas de Predicción por Puntaje de Matemáticas y Programa")

# 3. Diagnóstico del modelo - Residuos vs. valores ajustados
datos_muestra$residuos <- residuals(modelo_poisson_muestra, type = "response")
datos_muestra$valores_ajustados <- predict(modelo_poisson_muestra, type = "response")

ggplot(data = datos_muestra, aes(x = valores_ajustados, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Valores Ajustados", y = "Residuos") +
  theme_minimal() +
  ggtitle("Residuos vs. Valores Ajustados")

# 4. Q-Q plot de residuos
qqnorm(residuals(modelo_poisson_muestra, type = "pearson"))
qqline(residuals(modelo_poisson_muestra, type = "pearson"), col = "red")

######################################################################################
# PUNTO 3
data3 <- read_excel("Datos_MP1.xlsx", 
                   sheet = "Datos_3")

summary(data3) 
write.xlsx(summary(data3), "summary_data3.xlsx")

resultados_descriptivos <- sapply(data3[, sapply(data3, is.numeric)], function(x) {
  c(
    media = mean(x, na.rm = TRUE),
    mediana = median(x, na.rm = TRUE),
    varianza = var(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    rango_intercuartilico = IQR(x, na.rm = TRUE),
    coef_variacion = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
  )
})

resultados_descriptivos_df <- as.data.frame(t(resultados_descriptivos))

rownames(resultados_descriptivos_df) <- colnames(data3)[sapply(data3, is.numeric)]

colnames(resultados_descriptivos_df) <- c("media", "mediana", "varianza", "desviación estándar", "rango intercuartílico", "coeficiente de variación")
write.xlsx(resultados_descriptivos_df, "Resultados_Descriptivos.xlsx")


# Analizando missing values
#bar plot por porcentajes
data3$porcentaje_asistencia <- data3$asistencia_miles / sum(data3$asistencia_miles, na.rm = TRUE) * 100
data3$porcentaje_arrestos <- data3$num_arrestos / sum(data3$num_arrestos, na.rm = TRUE) * 100
data3$porcentaje_inv_social <- data3$inv_social_millones / sum(data3$inv_social_millones, na.rm = TRUE) * 100


bar_plot_percent <- function(data, columna, name) {
  graf <- ggplot(data3, aes(x = !!sym(columna), y = reorder(Equipo, !!sym(columna)), fill = Equipo)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = paste0(round(!!sym(columna), 2), "%")), hjust = 1.1, size = 3) +
    labs(title = paste("\nPorcentaje de ", name, " por Equipo \n"), x = paste("\nPorcentaje de ", name, "\n"), y = "\n Equipo \n") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face="bold", hjust = 0.5),
          axis.title.x = element_text(face="bold", size = 12),
          axis.title.y = element_text(face="bold", size = 12),
          legend.position = 'none')
  return(graf)}
bar_plot_percent(data, 'porcentaje_asistencia', 'Asistencia (miles)')
bar_plot_percent(data, 'porcentaje_arrestos', 'Número de arrestos')
bar_plot_percent(data, 'porcentaje_inv_social', 'Inversión Social (Millones)')


# Distribución de las variables
box1 <- ggplot(data3, aes(y = asistencia_miles)) +
  geom_boxplot(position = position_dodge(1)) +
  labs(title = 'Asistencia en miles', y = 'Asistencia')

box2 <- ggplot(data3, aes(y = num_arrestos)) +
  geom_boxplot(position = position_dodge(1)) +
  labs(title = 'Número de arrestos', y = 'Arrestos')

box3 <- ggplot(data3, aes(y = inv_social_millones)) +
  geom_boxplot(position = position_dodge(1)) +
  labs(title = 'Inversión social en millones', y = 'Inversión')

box1 + box2 + box3

summary(data3)
write.xlsx(summary(data3), "summary data3.xlsx")

# Dispersión de los datos
ggpairs(data3[,2:4], title = 'Correlaciones')

#Observar datos faltantes
superheat(data3[,2:4], heat.na.col = "white", title = 'Valores faltantes')


#Na con metodo con interpolado
install.packages("tidyverse")
library(tidyverse)
install.packages("zoo")
library(zoo)
df_ordenado <- data3 %>% arrange(desc(num_arrestos))
print(df_ordenado)
df_ordenado <- df_ordenado %>% fill(asistencia_miles, .direction = "up")
df_ordenado <- df_ordenado %>% fill(inv_social_millones, .direction = "down")
print(df_ordenado)
df_ordenado$num_arrestos <- na.locf(df_ordenado$num_arrestos, na.rm = FALSE)
print(df_ordenado)

#MODELOS PROPUESTOS
# Modelo de regresión lineal

modelo_knn1 <- lm(num_arrestos ~ asistencia_miles + inv_social_millones, data = df_ordenado )
summary(modelo_knn1)
coeficientesmodknn <- summary(modelo_knn1)$coefficients
estimaciones <- as.data.frame(coeficientesmodknn)
write.xlsx(estimaciones, "Tabla modelo regresion lineal 3.xlsx")

desv_1 <- deviance(modelo_knn1)
gl_1 <- modelo_knn1$df.residual
aic_1 <- AIC(modelo_knn1)
bic_1 <- BIC(modelo_knn1)
tabla_resultadosk <- data.frame(
  Modelo = "knn1",
  Variable = c("Grados de libertad", "Desviación", "AIC", "BIC"),
  Valor = c(gl_1, desv_1, aic_1, bic_1)
)
write.xlsx(tabla_resultadosk, "Tabla metricas knn1.xlsx")

#Modelo poisson
modelo_poisson <- glm(num_arrestos ~ asistencia_miles + inv_social_millones, 
                      family = poisson, data = df_ordenado)
summary(modelo_poisson)
coeficientesmodp <- summary(modelo_poisson)$coefficients
estimaciones <- as.data.frame(coeficientesmodp)
write.xlsx(estimaciones, "Tabla modelo regresion poisson.xlsx")

desv_2 <- deviance(modelo_poisson)
gl_2 <- modelo_poisson$df.residual
aic_2 <- AIC(modelo_poisson)
bic_2 <- BIC(modelo_poisson)
tabla_resultadosp <- data.frame(
  Modelo = "Poisson",
  Variable = c("Grados de libertad", "Desviación", "AIC", "BIC"),
  Valor = c(gl_2, desv_2, aic_2, bic_2))
write.xlsx(tabla_resultadosp, "Tabla metricas poisson.xlsx")

#Modelo Binomial negativo
library(MASS)
modelo_binom_neg <- glm.nb(num_arrestos ~ asistencia_miles + inv_social_millones, 
                           data = df_ordenado)
summary(modelo_binom_neg)
coeficientesmodb <- summary(modelo_binom_neg)$coefficients
estimaciones <- as.data.frame(coeficientesmodb)
write.xlsx(estimaciones, "Tabla modelo BN.xlsx")

desv_3 <- deviance(modelo_binom_neg)
gl_3 <- modelo_binom_neg$df.residual
aic_3 <- AIC(modelo_binom_neg)
bic_3 <- BIC(modelo_binom_neg)
tabla_resultadosb <- data.frame(
  Modelo = "Binomial Negativo",
  Variable = c("Grados de libertad", "Desviación", "AIC", "BIC"),
  Valor = c(gl_3, desv_3, aic_3, bic_3))
write.xlsx(tabla_resultadosb, "Tabla metricas BN.xlsx")

#tabla de modelos 
tabla_resultados_modelos <- data.frame(
  Modelo = c("knn1", "knn1", "knn1", "knn1", "Poisson", "Poisson", "Poisson", "Poisson", "Binomial Negativo", "Binomial Negativo", "Binomial Negativo", "Binomial Negativo"),
  Variable = c("Grados de libertad", "Desviación", "AIC", "BIC", "Grados de libertad", "Desviación", "AIC", "BIC", "Grados de libertad", "Desviación", "AIC", "BIC"),
  Valor = c(gl_1, desv_1, aic_1, bic_1, gl_2, desv_2, aic_2, bic_2, gl_3, desv_3, aic_3, bic_3))

write.xlsx(tabla_resultados_modelos, "Tabla_metricas.xlsx", rowNames = FALSE)

