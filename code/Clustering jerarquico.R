

datos<- read.csv("data/rociochavez/Ejemplo Estudiantes.csv", header = T, 
                 sep = ",", dec = ".", row.names = 1)


modelo<- hclust(dist(datos), method = "ward.D2")

plot(modelo)

rect.hclust(modelo, k=3, border = "magenta")

?hclust()


# Explicacion matematica

#url: https://www.youtube.com/watch?v=d_7pU9zqkfM&ab_channel=RocioChavezCienciadeDatos