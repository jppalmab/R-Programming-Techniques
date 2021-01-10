# WORD CLOUDS


#install.packages("tm", dependencies = T)
## paquete muy utilizado en mineria, 
## sirve para remocon de puntuacion, 
## poner letras mayusculas o minusculas, 
## o remocion de palabras muuuy comunes

library(tm)

comentarios<- read.csv("data/rociochavez/opiniones acerca de vinos.csv",
                       header = T, 
                       sep = ",", 
                       dec=".", 
                       row.names = 1)

# Para cargar el texto en el formato requerido para el analisis

palabras<- VCorpus(VectorSource(comentarios$description))

# Preparamos los textos para que puedan ser utilizados

palabras<- tm_map(palabras, content_transformer(tolower)) #todo a minuscula
palabras<- tm_map(palabras, removePunctuation) #elimina la puntuacion, pues no aporta info
palabras<- tm_map(palabras, removeWords, stopwords("english")) #remueve articulos, se supone sirve para el Spanish

# Para poner colores atractivos utilizamos el paquete R color Brewer
## install.packages("RColorBrewer", dependencies = T)

library(RColorBrewer)

#este paquete contiene conjunto de colores
colores<- brewer.pal(8, "Dark2") # los parametros son n de colores, y del conjunto de colores

#esta escala de colores, no tiene colores claros porque serian dificl de identificar

display.brewer.all(colorblindFriendly = T) #para desplegar la paleta de colores

# install.packages("wordcloud", dependencies = T)

library(wordcloud)

wordcloud(palabras, # el objeto donde esta almacenadas las alabras
          scale = c(2.6,0.3), # el tamanho de la palabra mas grande y el mas pequenho
          random.order = F, # para que las palabras mas repetidas esten al centro
          max.words = 75, # el maximo de palabras incluidos en el grafico
          rot.per = 0.25, # % de palabras que quiero que esten verticales
          colors = colores) #finalmente la escala de colores almacenados en el objeto

title(main = "Wine wordcloud", 
      cex.main=1.5) #el tamanho
