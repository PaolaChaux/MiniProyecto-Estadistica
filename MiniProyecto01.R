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

#Fijar el directorio de trabajo
setwd("P:/Estadistica y probabilidad 2/Miniproyecto")

# Listar los archivos en el directorio de trabajo
dir()

#PUNTO 1
#Cargar los datos
df1 <- read.xlsx("Datos_MP12.xlsx")
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
alcohol_presente <- rep(df$Alcohol, df$Frec..Presente)
estado_presente <- rep("Presente", times = sum(df$Frec..Presente))
alcohol_ausente <- rep(df$Alcohol, df$Frec..Ausente)
estado_ausente <- rep("Ausente", times = sum(df$Frec..Ausente))

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

#Correlacion 
correlacion_pearson <- cor(df1$Alcohol, df1$Frec..Presente, method = "pearson")
# Calcular la correlación de Spearman
correlacion_spearman <- cor(df1$Alcohol, df1$Frec..Presente, method = "spearman")
print(paste("Correlación de Pearson:", correlacion_pearson))
print(paste("Correlación de Spearman:", correlacion_spearman))

# Correlación de Pearson
cor_pearson <- cor(df1$Alcohol, df1$Frec..Presente, method = "pearson")
# Correlación de Spearman
cor_spearman <- cor(df1$Alcohol, df1$Frec..Presente, method = "spearman")
print(paste("Correlación de Pearson:", cor_pearson))
print(paste("Correlación de Spearman:", cor_spearman))

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
df$Porcentaje_Malformaciones <- (df$Frec..Presente / df$TotalNinos) * 100
tabla_porcentajes <- df[, c('Alcohol', 'Frec..Presente', 'TotalNinos', 'Porcentaje_Malformaciones')]
print(tabla_porcentajes)
write.xlsx(tabla_porcentajes, "Tabla Porcentajes.xlsx")

# 1.A. CONTRUCCION DE LOS MODELOS
#Construccion del modelo lineal
#vectores
alcohol <- c(0.0, 0.5, 1.5, 4.0, 7.0)
frec_presente <- c(48, 38, 5, 1, 1)

modelo_lineal <- lm(frec_presente ~ alcohol)
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
  Pseudo_R2 = c(NA, pseudo_r2_logit, pseudo_r2_probit)  # NA para el modelo lineal
  )
print(comparison)
write.xlsx(comparison, "Comparacion_aic.xlsx")

#comparacion 3 modelos los valores predichos por cada modelo
pred_lineal <- predict(modelo_lineal)
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

# Asumiendo que tienes un data frame llamado 'datos' con las columnas Alcohol, Observed, Lineal, Logistic, Probit
datos <- data.frame(
  Alcohol = c(0, 0.5, 1.5, 4, 7),
  Observed = c(48, 38, 5, 1, 1),
  Lineal = c(34.3620178, 31.33086053, 25.26854599, 10.11275964, -8.074183976),
  Logistic = c(44.01874897, 43.67800161, 3.274173133, 1.15126482, 0.877811459),
  Probit = c(43.78077572, 43.91408659, 3.336758511, 1.159055292, 0.802701103)
)
datos_long <- reshape2::melt(datos, id.vars = 'Alcohol', variable.name = 'Modelo', value.name = 'Valor')

p <- ggplot(datos_long, aes(x = Alcohol, y = Valor, colour = Modelo)) +
  geom_point(data = subset(datos_long, Modelo == "Observed"), size = 3) +
  geom_line(aes(group = Modelo)) +
  scale_colour_manual(values = c("black", "red", "blue", "green")) +
  labs(title = "Comparación de Modelos de Regresión", x = "Niveles de Alcohol", y = "Frecuencia (Predicción vs Observada)") +
  theme_minimal()
print(p)
ggsave("comparacion_modelos.png", plot = p, width = 10, height = 6, dpi = 300)



#ESTO NO ME DIO, NO LO HICE
# Comparar los valores predichos con los observados podría implicar, por ejemplo, 
# calcular la raíz del error cuadrático medio para cada modelo
rmse_linear <- sqrt(mean((datos_replicados$def_cat - pred_lineal)^2))
rmse_logit <- sqrt(mean((datos_replicados$def_cat - pred_logistic)^2))
rmse_probit <- sqrt(mean((datos_replicados$def_cat - pred_probit)^2))

# Agregar RMSE al dataframe de comparación si es necesario
comparison$RMSE <- c(rmse_linear, rmse_logit, rmse_probit)
print(comparison)
write.xlsx(comparison, "Comparacion_nuevo.xlsx")
  