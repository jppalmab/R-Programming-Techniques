#### Frequecy tables

corredores<- read.csv("data/rociochavez/categorias de corredores.csv", header= T, sep=",", dec=",", row.names = 1)

head(corredores)

summary(corredore$velocidades)

barplot (corredore$velocidades)

#Barplots

estudiantes<- read.csv("data/rociochavez/Ejemplo Estudiantes.csv", header= T, sep=",", dec=".", row.names = 1)

barplot(estudiantes$Matematicas, main="Students Score", 
        names.arg = row.names(estudiantes), ylim = c(0,10), col = "yellow")

latinos<- read.csv("data/rociochavez/Latinos.csv", header= T, sep=",", dec=".", row.names = 1)

library(dplyr)

por_pais<- group_by(latinos, Pais)

por_pais

count_por_pais<- summarise(por_pais, total_por_pais= length(Pais))

count_por_pais

barplot(count_por_pais$total_por_pais, main="Students Score", 
        names.arg = count_por_pais$Pais, ylim = c(0,16), col = rainbow(12))


#Boxplots

latinos<- read.csv("data/rociochavez/Latinos.csv", header= T, sep=",", dec=".", row.names = 1)

boxplot(latinos$Tiempo,xlab="Time", col = "blue")

summary(latinos)

boxplot(latinos$Edad,xlab="Age", col = "blue")

boxplot(latinos$Tiempo ~ latinos$Pais,xlab="Time by country", 
        col = c("red", "sienna", "palevioletred1", "royalblue2"))

# Pareto Diagram
ventas<- read.csv("data/rociochavez/Datos Compras.csv", sep=",", dec=".", header = T)

# table
productos_vendidos = table(ventas$Producto)

productos_vendidos #notice that are arranged by alphabet

# Install QCC
##install.packages("qcc", dependencies = T)
library(qcc) 

pareto.chart(productos_vendidos, col=rainbow(length(productos_vendidos)), 
             main="Pareto Diagram")

# se ve que el 80% de las ventas corresponden a la Leche, Mantequilla, Pan y Galletas

#we can save the object
table_freq_ventas <- pareto.chart(productos_vendidos, col=rainbow(length(productos_vendidos)), 
                               main="Pareto Diagram")

table_freq_ventas #tabla de frecuencias y grafico asociado

#Histograms
## it is used with numeric variables.

latinos<- read.csv("data/rociochavez/Latinos.csv", header= T, sep=",", dec=".", row.names = 1)

class((latinos$Tiempo))

hist(latinos$Tiempo)
# it calculated 6 intervlas by default, if you wanna 10 interval you use breaks

hist(latinos$Tiempo, breaks = 10)

# for put color you use col
hist(latinos$Tiempo, breaks = 10, col="blue")

# if you want different colors
hist(latinos$Tiempo, breaks = 10, col= rainbow(12))

# put name on x and y lab and title
hist(latinos$Tiempo, breaks = 10, col= rainbow(12), xlab = "Time", 
     ylab = "Frequency", main = "Histogram of Time")



# Graph dispersion
latinos<- read.csv("data/rociochavez/Latinos.csv", header= T, sep=",", dec=".", row.names = 1)

plot(latinos$Tiempo ~ latinos$Edad, xlab="Age", ylab = "Time", main="Time vs Ages")

# for more than 2 variables

estudiantes<- read.csv("data/rociochavez/Ejemplo Estudiantes.csv", header= T, sep=",", dec=".", row.names = 1)

pairs(data=estudiantes,
      ~Matematicas + Ciencias + Espanol + Historia + Deportes,
      pch=19, #shape of points 19= points 17=triangle 15=square  in scale 0 to 25
      main="Students Score")


# Correlation Matrix

mtcars <- read.csv("data/rociochavez/mtcars.csv", header = T, sep = ",", dec = ".")

mtcars$X= NULL

mtcars_cor<- cor(mtcars, method = "pearson")

round_cor<- round(mtcars_cor, digits = 1)

install.packages("corrplot", dependencies = T)

library(corrplot)

corrplot(round_cor)

#Lineal Correlation
contaminacion <- read.csv("data/rociochavez/Contaminacion Atmosferica.csv", header = T, sep = ",", dec = ".")

plot(contaminacion$Velocidad_viento, contaminacion$Dias_Lluvia,
     pch=19,
     col="blue",
     xlab = "Wind speed",
     ylab = "Raining Days",
     main="Raining days and wind speed")

plot(contaminacion$Habitantes, contaminacion$Fabricas,
     pch=19,
     col="blue",
     xlab = "Habitants",
     ylab = "Fabrics",
     main="Fabrics and habitants")

#prove normal distribution
## Option 1: histogram
hist(contaminacion$Velocidad_viento, col="blue")

# Option 2: Normal Q-Q Plot
qqnorm(contaminacion$Velocidad_viento, pch=19, col="blue")
qqline(contaminacion$Velocidad_viento, col="red", lwd=2)

# Option 3: Shapiro-wilks 
velocidad_viento_test<- shapiro.test(contaminacion$Velocidad_viento)
print(velocidad_viento_test) #values above 0.05 means it have a normal distribution

#now other varible
hist(contaminacion$Contaminacion_SO2, col="blue")
qqnorm(contaminacion$Contaminacion_SO2, pch=19, col="blue")
qqline(contaminacion$Contaminacion_SO2, col="red", lwd=2)
contaminacion_SO2_test<- shapiro.test(contaminacion$Contaminacion_SO2)
print(contaminacion_SO2_test) #values above 0.05 means it have a normal distribution

# Correlation when one variable has not normal distribution
#install.packages("psych", dependencies = T)

library(psych)

pairs.panels(contaminacion,
             method = "spearman", #por defecto usa Pearson si queremos usar Spearman pues una variable no es normal
             density = F, #para no deplegar graficos de densidad
             ellipses = F, #o las elipses de correlacion
             smooth = F) #o la linea de regresion local

# ahora calculamos la correlacion que existe entre las variables
contaminacion_corr<- cor(contaminacion, method = "spearman")

# redondeamos
contaminacion_corr<- round(contaminacion_corr, digits = 2)
contaminacion_corr

# install.packages("ggcorrplot")
library(ggcorrplot)

ggcorrplot(contaminacion_corr, 
           method = "circle", # le decimos que queremos circulos para graficar el tamanho de la correlacion
           type = "lower", # le decimos que nos den los valores bajo la diagonal 
           lab = T) + #valores de las correlacions en cada uno de los circulos
  ggtitle("Correlation Matrix") +
  theme_minimal() #elimina el fondo gris con rayas blancas de la funcion por default

# para obtener el p-value de la correlacion ocupamos corr.test

corr.test(contaminacion, 
          method = "spearman", 
          adjust="none") #adjusted none es para que no realice ningun tio de ajuste extra al momento de calcular la significancia

#valores menores a 0.05 se entiende que son correlaciones estadisticamente significativas.
