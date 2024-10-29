
#Ejercicio 2.1
library(haven)
data <- read_dta("C:/Users/sofia/OneDrive/Documentos/jtpa.dta")

#Modelo en niveles
modelo_niveles <- lm(earnings ~ jtpa_training, data = data)
summary(modelo_niveles)

#Limpio los datos de la variable earnings
data_clean <- data[!is.na(data$earnings) & data$earnings > 0, ]

#Modelo en logaritmos
modelo_logaritmos <- lm(log(earnings) ~ jtpa_training, data = data_clean)
summary(modelo_logaritmos)

#Ejercicio 2.2

#Modelo en logaritmos: diferencia entre hombres y mujeres
modelo_log_vi <- lm(log(earnings) ~ jtpa_training * sex, data = data_clean)
summary(modelo_log_vi)

#Ejercicio 2.4

#Modelo en logaritmos: variable instrumental para resolver endogeneidad
modelo_logaritmos_vi <- ivreg(log(earnings) ~ jtpa_training | jtpa_offer, data = data_clean)
summary(modelo_logaritmos_vi)
