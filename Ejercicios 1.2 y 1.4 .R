install.packages(c('eph'))
library(eph)
library(ggplot2)
options(scipen = 100)
eph_data_1_2024 <- get_microdata(year = 2024, trimester = 1, type = "individual")
str(eph_data_1_2024 )

eph_data_1_2023 <- get_microdata(year = 2023, trimester = 1, type = "individual")
str(eph_data_1_2023)


##EJERCICIO 1.2

##Crear variables##
ingreso<-eph_data_1_2024$P21
Ninicial<-length(ingreso)
hombre<-cbind(rep(1,Ninicial),eph_data_1_2024$CH04)#1 si es hombre y 0 si es mujer
hombre[,1]<-ifelse(hombre[,2]==2,0,1)
hombre<-hombre[,1]
jefe<-cbind(rep(1,Ninicial),eph_data_1_2024$CH03)
jefe[,1]<-ifelse(jefe[,2]==1,1,0)
jefe<-jefe[,1]
asalariado<-cbind(rep(1,Ninicial),eph_data_1_2024$ESTADO,eph_data_1_2024$CAT_OCUP)
asalariado[,1]<-ifelse((asalariado[,2]==1 & asalariado[,3]==3),1,0)
asalariado<-asalariado[,1]
edad<-eph_data_1_2024$CH06
edad2<-edad^2
casado<-cbind(rep(1,Ninicial),eph_data_1_2024$CH07)
casado[,1]<-ifelse((casado[,2]==1 | casado[,2]==2),1,0)
casado<-casado[,1]

##Variables educativas##
pi<-cbind(rep(1,Ninicial),eph_data_1_2024$NIVEL_ED)
pi[,1]<-ifelse((pi[,2]==1 | pi[,2]==7 |  pi[,2]==9),1,0)
pi<-pi[,1]
pc<-cbind(rep(1,Ninicial),eph_data_1_2024$NIVEL_ED)
pc[,1]<-ifelse(pc[,2]==2,1,0)
pc<-pc[,1]
si<-cbind(rep(1,Ninicial),eph_data_1_2024$NIVEL_ED)
si[,1]<-ifelse(si[,2]==3,1,0)
si<-si[,1]
sc<-cbind(rep(1,Ninicial),eph_data_1_2024$NIVEL_ED)
sc[,1]<-ifelse(sc[,2]==4,1,0)
sc<-sc[,1]
ui<-cbind(rep(1,Ninicial),eph_data_1_2024$NIVEL_ED)
ui[,1]<-ifelse(ui[,2]==5,1,0)
ui<-ui[,1]
uc<-cbind(rep(1,Ninicial),eph_data_1_2024$NIVEL_ED)
uc[,1]<-ifelse(uc[,2]==6,1,0)
uc<-uc[,1]
educacion <- eph_data_1_2024$NIVEL_ED

ingreso2<-eph_data_1_2023$P21
Ninicial2<-length(ingreso2)
hombre2<-cbind(rep(1,Ninicial2),eph_data_1_2023$CH04)#1 si es hombre y 0 si es mujer
hombre2[,1]<-ifelse(hombre2[,2]==2,0,1)
hombre2<-hombre2[,1]
jefe2<-cbind(rep(1,Ninicial),eph_data_1_2023$CH03)
jefe2[,1]<-ifelse(jefe2[,2]==1,1,0)
jefe2<-jefe2[,1]
asalariado2<-cbind(rep(1,Ninicial2),eph_data_1_2023$ESTADO,eph_data_1_2023$CAT_OCUP)
asalariado2[,1]<-ifelse((asalariado2[,2]==1 & asalariado2[,3]==3),1,0)
asalariado2<-asalariado2[,1]
eedad<-eph_data_1_2023$CH06
eedad2<-eedad^2
casado2<-cbind(rep(1,Ninicial2),eph_data_1_2023$CH07)
casado2[,1]<-ifelse((casado2[,2]==1 | casado2[,2]==2),1,0)
casado2<-casado2[,1]

##Variables educativas##
pi2<-cbind(rep(1,Ninicial2),eph_data_1_2023$NIVEL_ED)
pi2[,1]<-ifelse((pi2[,2]==1 | pi2[,2]==7 |  pi2[,2]==9),1,0)
pi2<-pi2[,1]
pc2<-cbind(rep(1,Ninicial2),eph_data_1_2023$NIVEL_ED)
pc2[,1]<-ifelse(pc2[,2]==2,1,0)
pc2<-pc2[,1]
si2<-cbind(rep(1,Ninicial2),eph_data_1_2023$NIVEL_ED)
si2[,1]<-ifelse(si2[,2]==3,1,0)
si2<-si2[,1]
sc2<-cbind(rep(1,Ninicial2),eph_data_1_2023$NIVEL_ED)
sc2[,1]<-ifelse(sc2[,2]==4,1,0)
sc2<-sc2[,1]
ui2<-cbind(rep(1,Ninicial2),eph_data_1_2023$NIVEL_ED)
ui2[,1]<-ifelse(ui2[,2]==5,1,0)
ui2<-ui2[,1]
uc2<-cbind(rep(1,Ninicial2),eph_data_1_2023$NIVEL_ED)
uc2[,1]<-ifelse(uc2[,2]==6,1,0)
uc2<-uc2[,1]
educacion2 <- eph_data_1_2023$NIVEL_ED


#regresión
y1<-data.frame(ingreso,hombre,jefe,asalariado,edad,edad2,casado,pi,pc,si,sc,ui,uc)
suby1<-subset(y1,ingreso>0 & edad>0 & jefe==1 & asalariado==1)
summary(suby1)

reg1<-lm(formula=log(ingreso)~hombre+edad+casado+pc+si+sc+ui+uc,data=suby1)
summary(reg1)



y2<-data.frame(ingreso2,hombre2,jefe2,asalariado2,eedad,eedad2,casado2,pi2,pc2,si2,sc2,ui2,uc2)
suby2<-subset(y2,ingreso2>0 & eedad>0 & jefe2==1 & asalariado2==1)
summary(suby2)

reg2<-lm(formula=log(ingreso2)~hombre2+eedad+casado2+pc2+si2+sc2+ui2+uc2,data=suby2)
summary(reg2)

#variable continua

y11 <- data.frame(ingreso, hombre, jefe, asalariado, edad, casado, educacion)
suby11<-subset(y11,ingreso>0 & edad>0 & jefe==1 & asalariado==1 & !is.na(educacion))
summary(suby11)
reg11<-lm(formula=log(ingreso)~hombre+edad+casado+educacion,data=suby11)
summary(reg11)


y12 <- data.frame(ingreso2, hombre2, jefe2, asalariado2, eedad, casado2, educacion2)
suby12<-subset(y12,ingreso2>0 & eedad>0 & jefe2==1 & asalariado2==1 & !is.na(educacion2))
summary(suby12)
reg12<-lm(formula=log(ingreso2)~hombre2+eedad+casado2+educacion2,data=suby12)
summary(reg12)



#EJERCICIO 1.4


# Coefficientes del modelo
coeficientes <- summary(reg1)$coefficients

# Secuencia de edades de 25 a 65
edades <- seq(25, 65, by=1)

# Definir las características de las personas a estudiar
# Casado = 1, Universitario completo (educacion = 5)
# Hombre = 1, Mujer = 0

hombres <- data.frame(edad = edades,  
                      hombre = 1,     
                      casado = 1,     
                      pc = 0,         
                      si = 0,         
                      sc = 0,       
                      ui = 0,    
                      uc = 1)        

mujeres <- data.frame(edad = edades,  
                      hombre = 0,     
                      casado = 1,     
                      pc = 0,        
                      si = 0,         
                      sc = 0,         
                      ui = 0,         
                      uc = 1)         



# Ingresos para hombres y mujeres
pred_hombres <- predict(reg1, newdata=hombres, interval="confidence", level=0.95)
pred_mujeres <- predict(reg1, newdata=mujeres, interval="confidence", level=0.95)


# Transformar las predicciones de log(ingreso) a ingreso real
salarios_hombres <- exp(pred_hombres)
salarios_mujeres <- exp(pred_mujeres)



# Resultados
datos_hombres <- data.frame(edad = edades, 
                            salario = salarios_hombres[,1], 
                            inferior = salarios_hombres[,2], 
                            superior = salarios_hombres[,3])

datos_mujeres <- data.frame(edad = edades, 
                            salario = salarios_mujeres[,1], 
                            inferior = salarios_mujeres[,2], 
                            superior = salarios_mujeres[,3])

# Grráfico
ggplot() +
  geom_line(data=datos_hombres, aes(x=edad, y=salario, color="Hombres")) +
  geom_ribbon(data=datos_hombres, aes(x=edad, ymin=inferior, ymax=superior), alpha=0.2, fill="blue") +
  geom_line(data=datos_mujeres, aes(x=edad, y=salario, color="Mujeres")) +
  geom_ribbon(data=datos_mujeres, aes(x=edad, ymin=inferior, ymax=superior), alpha=0.2, fill="red") +
  labs(title="Salarios promedio para hombres y mujeres con educación universitaria completa",
       x="Edad",
       y="Salario estimado",
       color="Género") +
  scale_color_manual(values=c("blue", "red"), labels=c("Hombres", "Mujeres")) +
  theme(legend.position = "right")


# Defino un data frame con mis características al momento de recibirme.
joaco <- data.frame(
  hombre = 1,                       
  edad = 24,                        
  casado = 0,                       
  pc = 0, 
  si = 0, 
  sc = 0, 
  ui = 0, 
  uc = 1)

# Al igual que en el caso anterior estimo mi ingreso con la función predict.
log_salario_joaco <- predict(reg1, newdata = joaco)

salario_joaco <- exp(log_salario_joaco)

print(salario_joaco)
