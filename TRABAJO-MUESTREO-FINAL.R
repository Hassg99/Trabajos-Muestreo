library(readr)
library(TeachingSampling)

Datos <- read_csv("C:/Users/hassg/OneDrive/Desktop/MUESTREO/archive (1)/healthcare-dataset-stroke-data.csv")
Datos$gender<-replace(Datos$gender, Datos$gender == "Other", "Male")

################################################################
##### Tamaño de la Muestra #####################################
################################################################

alpha = 0.05
N = nrow(Datos); N <- 5110
e = 0.02
p = 0.5

v = p*(1-p); v
z = qnorm(1-alpha/2); z = 1.957

no = (z^2)*v/(e^2)
n = no/(1+(no/N))
n = ceiling(n); n

#Asignación proporcional: Variable de estratificacion tipo de trabajo

T1 <- table(Datos$smoking_status)
n1 <- ceiling(n*T1[1]/N); n1
n2 <- ceiling(n*T1[2]/N)-1; n2
n3 <- ceiling(n*T1[3]/N); n3
n4 <- ceiling(n*T1[4]/N); n4

nh <- c(n1,n2,n3,n4); nh; sum(nh)
Nh <- c(T1[1],T1[2],T1[3],T1[4]); sum(Nh)

#################################
#####Selección de la muestra#####
#################################

set.seed(3456)
muestra <- S.STpiPS(Datos$smoking_status,Datos$avg_glucose_level,nh); muestra
muesam<-Datos[muestra[,1],];muesam #Extraer la muestra seleccionada

#################################
##### ESTIMACIONES ##############
#################################


#Estimacion del Total con la variable auxiliar 

pk<-muestra[,2]
est<-round(E.STpiPS(muesam$stroke,pk,muesam$smoking_status),3);est
est/N

round(est^2,2)

#IC POR ESTRATO:

t = ceiling(870.047)
z = 1.957
ee = 16.021
Li = t - z*ee; Li/N
Ls = t + z*ee; Ls/N

t = ceiling(1904.406)
z = 1.957
ee = 23.137
Li = t - z*ee; Li/N
Ls = t + z*ee; Ls/N

t = ceiling(784.963)
z = 1.957
ee = 15.235
Li = t - z*ee; Li/N
Ls = t + z*ee; Ls/N

t = ceiling(1555.494)
z = 1.957
ee = 17.259
Li = t - z*ee; Li/N
Ls = t + z*ee; Ls/N

t = ceiling(5114.910)
z = 1.957
ee = 36.359
Li = t - z*ee; Li/N
Ls = t + z*ee; Ls/N

#Est Media

table(Datos$smoking_status)/N #real
est/N
round((est/N)^2,6)

table(Datos$smoking_status, Datos$stroke)
est
est^2

t = ceiling(267.988)
ee = 24.046
z = 1.957
Li = t - z*ee; Li
Ls = t + z*ee; Ls

#Estimacion de proporcion
table(Datos$smoking_status, Datos$stroke)/N #real


t = ceiling(0.092661 )
ee = 0.000011
z = 1.957
Li = t - z*ee; Li
Ls = t + z*ee; Ls



################## Estimacion con el dominio mujer y hombre  ###########################
table(Datos$smoking_status, Datos$gender)
table(Datos$gender)
table(Datos$smoking_status,Datos$stroke, Datos$gender)
table(Datos$smoking_status, Datos$gender)/N
table(Datos$gender)/N

muesam$fem <- ifelse(muesam$gender == "Female", 1, 0)
muesam$yfem <- muesam$stroke * muesam$fem

E.STpiPS(muesam$yfem,pk,muesam$stroke)


table(Datos$smoking_status, Datos$gender)
estima <- data.frame(stroke)
Dominios <- Domains(muesam$gender)
dm <- Dominios[,1]*estima
dh <- Dominios[,2]*estima

set.seed(345)
estimacion <- E.STSI(muesam$smoking_status,Nh,nh,Dominios)
estimacion

########### ESTIMACION DEL TOTAL DE PERSONAS CON ACCIDENTES 
########## CEREBROVASCUÑARES POR DOMINIO MUJER Y HOMBRE.

E.STSI(muesam$smoking_status,Nh,nh,dm)
E.STSI(muesam$smoking_status,Nh,nh,dh)

########## IC Mujeres Total

#Estrato 1
z = 1.957
ee = 21.683747
t = ceiling(412.791519)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Estrato 2

z = 1.957
ee = 29.928758
t = ceiling(1267.608624)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Estrato 3

z = 1.957
ee = 20.134340
t = ceiling(472.773810)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Estrato 4

z = 1.957
ee = 28.595368
t = ceiling(842.466531)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Poblacion

z = 1.957
ee = 28.595368
t = ceiling(2995.6404839)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

######### IC Hombres: Total

#Estrato 1
z = 1.957
ee = 21.683747
t = ceiling(472.208481)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Estrato 2

z = 1.957
ee = 29.928758
t = ceiling(624.391376)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Estrato 3

z = 1.957
ee = 20.134340
t = ceiling(316.226190)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Estrato 4

z = 1.957
ee = 28.595368
t = ceiling(701.533469)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Poblacion

z = 1.957
ee = 28.595368
t = ceiling(2114.3595161)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

##################################

########## IC Mujeres Total por enfermedad cerebrovascular

E.STSI(muesam$smoking_status,Nh,nh,dm)
E.STSI(muesam$smoking_status,Nh,nh,dh)

#Estrato 1
z = 1.957
ee = 7.62703
t = ceiling(28.14488)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Estrato 2

z = 1.957
ee = 11.39779
t = ceiling(62.75290)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Estrato 3

z = 1.957
ee = 5.135043
t = ceiling(12.523810)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Estrato 4

z = 1.957
ee = 8.482335
t = ceiling(34.450304)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Poblacion

z = 1.957
ee = 16.923354
t = ceiling(137.871892)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

######### IC Hombres: Total
E.STSI(muesam$smoking_status,Nh,nh,dh)

#Estrato 1
z = 1.957
ee = 7.62703
t = ceiling(28.14488)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Estrato 2

z = 1.957
ee = 7.282407
t = ceiling(25.101161)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Estrato 3

z = 1.957
ee = 9.411108
t = ceiling(43.833333)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Estrato 4

z = 1.957
ee = 6.297028
t = ceiling(18.791075)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N

#Poblacion

z = 1.957
ee = 15.4734140
t = ceiling(115.8704456)

Li = t - z*ee; Li
Ls = t + z*ee; Ls

Li/N
Ls/N
###########################################################

estimacion
Nest <- 2115

estimacion/Nest
est/Nest
