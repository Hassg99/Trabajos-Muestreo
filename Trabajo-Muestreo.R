library(readr)
library(TeachingSampling)
Datos <- read_csv("C:/Users/hassg/OneDrive/Desktop/MUESTREO/archive (1)/healthcare-dataset-stroke-data.csv")

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


################################################################
##### Diseño de Muestreo Bernoulli #############################
################################################################

pik <- 0.5

#### Seleccion de la muestra bajo diseño Bernoulli ###

set.seed(655)
sam = S.BE(N, prob = pik)
mueSam <- Datos[sam,]  #Muestra seleccionada,
length(mueSam$gender)

### Estimaciones:

## Estimacion total de personas con accidentes cerebrovasculares
round(E.BE(mueSam$stroke, prob = pik),3)

#Interpretacion: 

# De acuerdo al  diseño de  muestreo   Bernoulli  el  total  de 
# personas  con  accidentes  cerebrovasculares  es  de  228. El 
# Coeficiente de Variacion Estimado es de 6.62%  lo cual indica
# que el diseño es aceptable puesto que  está  por  debajo  del
# nivel de tolerancia que es 17%. Por otro lado,la varianza del
# diseño de muestreo Bernoulli dio 1.045 por encima de  la  var
# del diseño aleatorio simple, lo que es un indicio de que este
# ultimo diseño es más eficiente.

## Estimaciones al tiempo:

round(E.BE(mueSam[,c(4:5,12)], prob = pik),3)

#Interpretción:

# Se observa  que al estimar el total de la poblacion  de las 
# demás variables que se encuentran relacionadas directamente   
# con problemas de salud como lo son la hipertension o alguna 
# enfermedad en el corazón. El DEFF de todas ellas nos indica 
# que el diseñoaleatorio simple es el más eficiente.


################################################################  
##### DISEÑO SISTEMATICO #######################################
################################################################

a <- N/n ; a

# Muestra sistematica
set.seed(655)
sam <- S.SY(N,ceiling(a))

#Muestra de la poblacion
mueSam <- Datos[sam,]
nrow(mueSam)

### Estimaciones:

## Estimacion total de personas con accidentes cerebrovasculares
typi_SY <- round(E.SY(N,ceiling(a), mueSam$stroke),3); typi_SY

# Interpretación:

# De acuerdo al  diseño Sistematico el  total  de personas con 
# accidentes  cerebrovasculares  es  de 248. El Coeficiente de
# Variacion Estimado es de 10.73% lo cual indica que el diseño 
# es aceptable  puesto  que   está  por  debajo  del  nivel de 
# tolerancia que es 17%. Por otro lado, el DEFF indica que el
# diseño de muestreo sistematico es tan eficiente como el MAS.


################################################################
###### DISEÑO ALEATORIO SIMPLE #################################   
################################################################  
    
#### Variables de interés: #############################
    
    Datos$fem <- ifelse(Datos$gender == "Female", 1, 0)
    Datos$yfem <- Datos$stroke * Datos$fem
    
    Datos$male <- ifelse(Datos$gender == "Male", 1, 0)
    Datos$ymale <- Datos$stroke * Datos$male
    
    Datos$agep <- ifelse(Datos$age > 21, 1, 0)
    Datos$yage <- Datos$stroke * Datos$agep
    
    Datos$yhiper <- Datos$stroke * Datos$hypertension
    Datos$yheart <- Datos$stroke * Datos$heart_disease
    
    Datos$rural <- ifelse(Datos$Residence_type == "Rural", 1, 0)
    Datos$yrural <- Datos$stroke * Datos$rural
    
    Datos$urban <- ifelse(Datos$Residence_type == "Urban", 1, 0)
    Datos$yurban <- Datos$stroke * Datos$urban   

    Datos$imc <- ifelse(Datos$bmi > 25, 1, 0)
    Datos$yimc <- Datos$stroke * Datos$imc

#######################################################
   
#Estimacion poblacional
    
   set.seed(655)
   sam <- S.SI(N = N, n=n)
   mueSam <- Datos[sam,]
   #pik = n/N
   
   ### Estimaciones:
    ## Estimacion total de personas con accidentes cerebrovasculares
    ty <- E.SI(N = N, n = n, y = mueSam$stroke); ty
    table(Datos$stroke)
    
    #I.C Mujeres
    z = 1.957
    ee = ty[2,2]
    t = ceiling(ty[1,2])
    
    Li = t - z*ee; Li
    Ls = t + z*ee; Ls
   
    # Interpretación:
   
    # De acuerdo al Diseño de Muestreo Simple, en total hay 245 
    # personas con accidentes cerebrovasculares El Coeficiente 
    # de Variacion Estimado es de 9.11% lo cual indica que el 
    # diseño es aceptable puesto  que   está  por  debajo  del   
    # nivel de tolerancia que es 17%.
    
    table(Datos$stroke)/N
    (E.SI(N = N, n = n, y = mueSam$stroke)/N)^2
    
    sum(Datos$stroke)/245
 
    Li/N
    Ls/N
    
# ESTIMACIONES:   
    
## 1) Estimacion por genero ####
     y <- mueSam[,colnames(Datos)%in%c("fem", "male", "yfem", "ymale")]
  
 #1.1) Total de mujeres y hombres
    typg <- E.SI(N = N, n = n, y = y[,c("fem","male")]); typg
    
    #I.C Mujeres
    z = 1.957
    ee = typg[2,2]
    t = ceiling(typg[1,2])
    
    Li = t - z*ee; Li
    Ls = t + z*ee; Ls
    
    #I.C Hombres
    z = 1.957
    ee = typg[2,3]
    t = ceiling(typg[1,3])
    
    Li = t - z*ee; Li
    Ls = t + z*ee; Ls
    
 #1.2) Proporción de mujeres y hombres que han tenido enfermedades cerebrovasculares
    E.SI(N = N, n = n, y = y)/N
    
    #I.C Mujeres
    z = 1.957
    ee = typg[2,2]
    t = ceiling(typg[1,2])
    
    Li = t - z*ee; Li/N
    Ls = t + z*ee; Ls/N
    
    #I.C Hombres
    z = 1.957
    ee = typg[2,3]
    t = ceiling(typg[1,3])
    
    Li = t - z*ee; Li/N
    Ls = t + z*ee; Ls/N
    
 #1.3) Total de mujeres y hombres que han tenido enfermedades cerebrovasculares
    tydg <- E.SI(N = N, n = n, y = y[,c("yfem", "ymale")]); tydg
    
    #I.C Mujeres
    z = 1.957
    ee = tydg[2,2]
    t = ceiling(tydg[1,2])
    
    Li = t - z*ee; Li
    Ls = t + z*ee; Ls
    
    #I.C Hombres
    z = 1.957
    ee = tydg[2,3]
    t = ceiling(tydg[1,3])
    
    Li = t - z*ee; Li
    Ls = t + z*ee; Ls
  
 #1.4) Media
  
    sum(Datos$gender)/sum(Datos$yfem)
    sum(Datos$gender)/sum(Datos$ymale)

   tym <-  tydg/typg; tym
      
      #I.C Mujeres
      z = 1.957
      ee = tym[2,2]
      t = ceiling(tym[1,2])
      
      Li = t - z*ee; Li
      Ls = t + z*ee; Ls
      
      #I.C Hombres
      z = 1.957
      ee = tym[2,3]
      t = ceiling(tym[1,3])
      
      Li = t - z*ee; Li
      Ls = t + z*ee; Ls
    
## 2) Estimacion por edad ####
     
      y <- mueSam[,colnames(Datos)%in%c("agep", "yage")]
      
 #2.1) Tamaño
      type <- E.SI(N = N, n = n, y = y[,c("agep")]); type
      
      #I.C 
      z = 1.957
      ee = type[2,2]
      t = ceiling(type[1,2])
      
      Li = t - z*ee; Li
      Ls = t + z*ee; Ls
      
 #2.2) Proporción personas mayores de edad que han tenido enfermedades cerebrovasculares
      
      E.SI(N = N, n = n, y = y)/N
      
      #I.C 
      Li/N
      Ls/N      
      
 #2.3) Total de personas mayores de edad que han tenido enfermedades cerebrovasculares
      
      tyde <- E.SI(N = N, n = n, y = y[,c("yage")]); tyde
      
      #I.C 
      z = 1.957
      ee = tyde[2,2]
      t = ceiling(tyde[1,2])
      
      Li = t - z*ee; Li
      Ls = t + z*ee; Ls
      
 #2.4) Media 
      sum(Datos$yage)/sum(Datos$agep)
      tym <- tyde/type
      
      #I.C 
      z = 1.957
      ee = tym[2,2]
      t = ceiling(tym[1,2])
      
      Li = t - z*ee; Li
      Ls = t + z*ee; Ls
        
## 3) Estimacion por hipertensión ####
      y <- mueSam[,colnames(Datos)%in%c("hypertension", "yhiper")]
 #3.1) Tamaño
      
      typh <- E.SI(N = N, n = n, y = y[,c("hypertension")]); typh
      
      #I.C 
      z = 1.957
      ee = typh[2,2]
      t = ceiling(typh[1,2])
      
      Li = t - z*ee; Li
      Ls = t + z*ee; Ls
      
 #3.2) Proporción
      E.SI(N = N, n = n, y = y)/N
      
      #I.C 
      Li/N
      Ls/N 
      
 #3.3) Total
      tydh <- E.SI(N = N, n = n, y = y[,c("yhiper")]); tydh
      
      #I.C 
      z = 1.957
      ee = tydh[2,2]
      t = ceiling(tydh[1,2])
      
      Li = t - z*ee; Li
      Ls = t + z*ee; Ls
      
 #3.4) Media 
      sum(Datos$yhiper)/sum(Datos$hypertension)
      tym = tydh/typh 
      
      #I.C 
      z = 1.957
      ee = tym[2,2]
      t = ceiling(tym[1,2])
      
      Li = t - z*ee; Li
      Ls = t + z*ee; Ls
        
## 4) Estimacion por tipo de residencia ####
      y <- mueSam[,colnames(Datos)%in%c("rural","yrural","urban","yurban")]
 #5.1) Tamaño
     table(Datos$Residence_type)
     typr <- E.SI(N = N, n = n, y = y[,c("rural","urban")]); typr
     
     #I.C Rural
     z = 1.957
     ee = typr[2,2]
     t = ceiling(typr[1,2])
     
     Li = t - z*ee; Li
     Ls = t + z*ee; Ls
     
     #I.C Urbano
     z = 1.957
     ee = typr[2,3]
     t = ceiling(typr[1,3])
     
     Li = t - z*ee; Li
     Ls = t + z*ee; Ls
     
 #5.2) Proporción
     table(Datos$Residence_type)/N
     E.SI(N = N, n = n, y = y)/N
     
     #I.C Rural
     z = 1.957
     ee = typr[2,2]
     t = ceiling(typr[1,2])
     
     Li = t - z*ee; Li/N
     Ls = t + z*ee; Ls/N
     
     #I.C Urbano
     z = 1.957
     ee = typr[2,3]
     t = ceiling(typr[1,3])
     
     Li = t - z*ee; Li/N
     Ls = t + z*ee; Ls/N
     
     
 #5.3) Total
     table(Datos$yurban)
     tydr <- E.SI(N = N, n = n, y = y[,c("yrural","yurban")]); tydr
     
     #I.C Rural
     z = 1.957
     ee = tydr[2,2]
     t = ceiling(tydr[1,2])
     
     Li = t - z*ee; Li
     Ls = t + z*ee; Ls
     
     #I.C Urbano
     z = 1.957
     ee = tydr[2,3]
     t = ceiling(tydr[1,3])
     
     Li = t - z*ee; Li
     Ls = t + z*ee; Ls
     
 #5.4) Media
     
     # Rural:
     sum(Datos$yrural)/sum(Datos$rural)
     sum(Datos$yurban)/sum(Datos$urban)
     tym <- tydr/typr
     
     #I.C Rural
     z = 1.957
     ee = tym[2,2]
     t = ceiling(tym[1,2])
     
     Li = t - z*ee; Li
     Ls = t + z*ee; Ls
     
     #I.C Urbano
     z = 1.957
     ee = tym[2,3]
     t = ceiling(tym[1,3])
     
     Li = t - z*ee; Li
     Ls = t + z*ee; Ls
  
    
## 5) Estimacion por IMC ####
      y <- mueSam[,colnames(Datos)%in%c("imc", "yimc")]
 
 #5.1) Tamaño
      table(Datos$imc)
      typi <- E.SI(N = N, n = n, y = y[,c("imc")]); typi
      
      #I.C 
      z = 1.957
      ee = typi[2,2]
      t = ceiling(typi[1,2])
      
      Li = t - z*ee; Li
      Ls = t + z*ee; Ls
      
 #5.2) Proporción
      table(Datos$imc)/N
      E.SI(N = N, n = n, y = y)/N
      
      #I.C 
      Li/N
      Ls/N
      
 #5.3) Total
      table(Datos$yimc)
      tydi <- E.SI(N = N, n = n, y = y[,c("yimc")]); tydi
      
      #I.C 
      z = 1.957
      ee = tydi[2,2]
      t = ceiling(tydi[1,2])
      
      Li = t - z*ee; Li
      Ls = t + z*ee; Ls
 
 #5.4) Media 
      sum(Datos$yimc)/sum(Datos$imc)
      tym <- tydi/typi
      
      #I.C 
      z = 1.957
      ee = tym[2,2]
      t = ceiling(tym[1,2])
      
      Li = t - z*ee; Li
      Ls = t + z*ee; Ls
