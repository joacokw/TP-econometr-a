rm(list = ls())

#Librerías

library(eph)
library(ggplot2)
library(tidyverse)
library(scales)
library(grid)
library(gridExtra)

#Descarga de las Bases

eph1t23 <- get_microdata(year = 2023, period = 1, type = "individual") #%>% 
  #organize_labels(type = "individual")

eph1t24 <- get_microdata(year = 2024, period = 1, type = "individual") #%>% 
  #organize_labels(type = "individual")


unique(eph1t24$CH03)

#Primero obtenemos la muestra para habitantes de CABA, jefes de hogar, entre 25 y 65 años de edad.

eph1t23_clean <- eph1t23 %>% 
  filter(CH06 >= 25 & CH06 <= 65) %>%
  filter(AGLOMERADO == 32) %>% 
  filter(CH03 == 1) %>% 
  rename(sexo = CH04, edad = CH06, salario = P21) %>% 
  mutate(sexo = case_when(sexo == 1 ~ 'Hombre', 
                          sexo == 2  ~ 'Mujer')) %>% 
  mutate(rango_etario = case_when(
                          edad < 25 ~ "Menor a 25 años",
                          edad %in% 25:34 ~ "25 a 34 años", 
                          edad %in% 35:44 ~ "35 a 44 años", 
                          edad %in% 45:54 ~ "45 a 54 años",
                          edad %in% 55:65 ~ "55 a 65 años",
                          edad > 65 ~ "Mayor a 65 años")) %>% 
  mutate(salario = as.numeric(salario))


eph1t24_clean <- eph1t24 %>% 
  filter(CH06 >= 25 & CH06 <= 65) %>%
  filter(AGLOMERADO == 32) %>% 
  filter(CH03 == 1) %>%  
  rename(edad = CH06, salario = P21, nivel_ed = NIVEL_ED, cat_ocup = CAT_OCUP, sector = PP04D_COD) %>% 
  mutate(informal = ifelse(PP07H == 2, 1, 0)) %>% 
  mutate(sexo = case_when(CH04 == 1 ~ 'Hombre', 
                          CH04 == 2  ~ 'Mujer')) %>% 
  mutate(mujer = case_when(sexo == "Hombre" ~ 0,
                           sexo == "Mujer" ~ 1)) %>% 
  mutate(rango_etario = case_when(
                          edad < 25 ~ "Menor a 25 años",
                          edad %in% 25:34 ~ "25 a 34 años", 
                          edad %in% 35:44 ~ "35 a 44 años", 
                          edad %in% 45:54 ~ "45 a 54 años",
                          edad %in% 55:65 ~ "55 a 65 años",
                          edad > 65 ~ "Mayor a 65 años")) %>% 
  mutate(salario = as.numeric(salario))

eph1t24_clean$nivel_ed <- factor(eph1t24_clean$nivel_ed, levels = c(1, 2, 3, 4, 5, 6, 7, 9), 
                         labels = c("Primaria Incompleta (incluye educación especial)", 
                                    "Primaria Completa", 
                                    "Secundaria Incompleta", 
                                    "Secundaria Completa", 
                                    "Superior Universitaria Incompleta", 
                                    "Superior Universitaria Completa", 
                                    "Sin instrucción", 
                                    "Ns./Nr."))

eph1t24_clean$cat_ocup <- factor(eph1t24_clean$cat_ocup, 
                                             levels = c(1, 2, 3, 4, 0), 
                                             labels = c("Patrón", "Cuenta propia", "Obrero o empleado", 
                                                        "Trabajador familiar sin remuneración", "Ns./Nr."))

#Punto 1.

#Usando el ponderador PONDIIO, calculamos:

#Salarios prom. tot.

salario23tot <- eph1t23_clean %>%
  summarise(salario_promedio23 = weighted.mean(salario, PONDIIO))

salario24tot <- eph1t24_clean %>%
  summarise(salario_promedio24 = weighted.mean(salario, PONDIIO))

#Salarios prom. por sexo.

salario23sex <- eph1t23_clean %>% 
  group_by(sexo) %>% 
  summarise(salario_promedio23 = weighted.mean(salario, PONDIIO))

salario24sex <- eph1t24_clean %>% 
  group_by(sexo) %>% 
  summarise(salario_promedio24 = weighted.mean(salario, PONDIIO))

#Salarios prom. por rango etario

salario23age <- eph1t23_clean %>% 
  group_by(rango_etario) %>% 
  summarise(salario_promedio23 = weighted.mean(salario, PONDIIO))

salario24age <- eph1t24_clean %>% 
  group_by(rango_etario) %>% 
  summarise(salario_promedio24 = weighted.mean(salario, PONDIIO))


salario_total <- cbind(salario23tot, salario24tot) %>% pivot_longer(cols = 1:2, names_to = "names", values_to = "values") %>% 
  ggplot(aes(x = names, y = values, fill = names)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "darkred")) +
  guides(fill = "none") +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  labs(x = "", y = "Pesos corrientes") +
  ggtitle("Salario promedio")

salario_sex <- salario24sex %>% 
  ggplot(aes(x = sexo, y = salario_promedio24, fill = sexo)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "darkred")) +
  guides(fill = "none") +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  labs(x = "", y = "Pesos corrientes") +
  ggtitle("Salario promedio por sexo para el año 2024")

salario_age <- salario24age %>%
  ggplot(aes(x = rango_etario, y = salario_promedio24, fill = rango_etario)) +
  geom_col() +
  theme_minimal() +
  scale_fill_viridis_d() +
  guides(fill = "none") +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  labs(x = "", y = "Pesos corrientes") +
  ggtitle("Salario promedio por rango etario para el año 2024")


#Para realizar la comparación salarial utilizamos el IPC (mar23 = 100), descargado de la página de INDEC.
#Ajustamos el valor obtenido de la EPH con un promedio del índice de los tres primeros meses de cada año.

serieipc <- read.csv("./ipcmar23-mar24.csv", sep = ";", header = T, dec = ",")
serieipc23 <- serieipc %>% 
  head(n=3) %>% 
  select(2)

serieipc24 <- serieipc %>% 
  tail(n=3) %>% 
  select(2)

indice <- c(mean(serieipc23), mean(serieipc24))

comparacion_sal <- cbind(salario23tot, salario24tot) %>% 
  pivot_longer(cols = 1:2, values_to = "salario_nominal", names_to = "cat") %>% 
  cbind(indice) %>% 
  mutate(salario_ctesmar23 = salario_nominal*100/indice)

salario_comp <- comparacion_sal %>% 
  ggplot(aes(x = cat, y = salario_ctesmar23, fill = cat)) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "darkred")) +
  guides(fill = "none") +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
  labs(x = "", y = "Pesos constantes de mar-23") +
  ggtitle("Comparación en términos reales")


# Es decir que la variación interanual en el salario entre el 1T23 y el 1T24 es:

varsalarial <- ((comparacion_sal[2,4]/comparacion_sal[1,4])-1)*100

varsalarial <- str_c(round(varsalarial,2), "%")

print(varsalarial)


#Armamos el grid con los graficos

punto1a <- grid.arrange(salario_total, salario_comp, ncol=2)

punto1b <- grid.arrange(salario_age, salario_sex, ncol = 2)

ggsave(plot = punto1a, filename = "punto1a.jpg", device = "jpg", path = "./Gráficos", dpi = 1000, scale = 2, width = 5)

ggsave(plot = punto1b, filename = "punto1b.jpg", device = "jpg", path = "./Gráficos", dpi = 1000, scale = 2, width = 5)


#Punto 3.

#Utilizamos el mismo dataframe del punto 1. En particular, vamos a correr la regresión para los datos del 1T24. 
#Estimamos el logaritmo del ingreso controlando por edad, las dummies "mujer" e "informal" (se marca 1 si la persona dice no recibir descuentos jubilatorios), 
#las variables categóricas categoría ocupacional y nivel educativo, y fundamentalmente la interacción entre mujer y nivel educativo.


eph1t24_regp3 <- eph1t24_clean %>%
  select(salario, edad, sexo, nivel_ed, cat_ocup, mujer, sector, informal) %>% 
  filter(salario > 0)

#Por la reducida cantidad de casos -sólo 1- con primaria incompleta en la muestra (CABA, 26-65 años, jefes de hogar), vamos a tomar como categoría base 
#al nivel educativo Superior Universitaria Completa. Esto dificulta la interpretación pero le brinda una mayor robustez a la estimación.

prueba <- eph1t24_regp3 %>% filter(nivel_ed == "Primaria Incompleta (incluye educación especial)")

nrow(prueba)

eph1t24_regp3$nivel_ed <- relevel(eph1t24_regp3$nivel_ed, ref = "Superior Universitaria Completa") 

eph1t24_regp3$cat_ocup <- relevel(eph1t24_regp3$cat_ocup, ref = "Cuenta propia")

regp3 <- lm(log(salario) ~ edad + mujer + cat_ocup + informal + nivel_ed * mujer, data = eph1t24_regp3)

summary(regp3)

