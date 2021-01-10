## THEMATIC MAPPING

### lo primero s descomprimir los shape files
### tienen 3 archivos
### shape (.shp) inforacion geometrica
### shape index (.shx) indice de las entidades geometricas
### device (.dbf) tabla de datos en que se registran los atributos de los elementos


#install.packages('rgeos', type='source')
#install.packages('rgdal', type='source')

library(sp)
library(rgdal)
library(ggplot2)
library(rgeos)
library(rgdal)

mapa_mundial<- readOGR(dsn="data/rociochavez/Shape",
                       layer = "TM_WORLD_BORDERS-0.3") #dsn es el directorio

# veamos que contiene el slot data
head(mapa_mundial@data)

write.csv(mapa_mundial@data$NAME, file="data/rociochavez/nombres de paises.csv")

#graficamos el mapa 

plot(mapa_mundial, axes=T)

# Transformar la informacion de los poligonos
library(maptools)
#install.packages("plyr")
library(plyr)
#install.packages("ggplot2")
library(ggplot2)

??fortify
#Tranformar la info de los poligonos a un dataframe
mapa_mundial_fortified<- fortify(mapa_mundial, region = "NAME")

head(mapa_mundial_fortified)

# Ahora se busca que el archivo con la poblacion
# o info que queramos cargar haga match con la data
# obtenida del shapefile

#en el ejemplo se agruo la poblacion en una variable categorica de 3 niveles

poblacion<- read.csv("data/rociochavez/Poblacion Mundial Corregido Categorias.csv", sep = ",", dec = ".", row.names = 1, fileEncoding = "latin1")

#cambiamos los nombres de las columnas para que coincidan
names(poblacion)<- c("id", "Poblacion", "Categorias")

# hacemos que este todo en minuscula
mapa_mundial_fortified$id<- tolower(mapa_mundial_fortified$id)

poblacion$id<- as.character(poblacion$id)
poblacion$id<- tolower(poblacion$id)

mapa_a_graficar<- merge(mapa_mundial_fortified, poblacion, by="id", all=T)

#ahora vemos como queda el mapa a graficar en el data set
head(mapa_a_graficar)

#ahora creamos el mapa tematico
#pero antes es necesario ordenar el shapefile segun la variable order
#que entrega el orden de los poligonos a graficar

mapa_a_graficar<- mapa_a_graficar[order(mapa_a_graficar$order), ]

ggplot(mapa_a_graficar, aes(x=long, y=lat, group=group))+
  geom_polygon(aes(fill=Categorias)) + #le indicamos que los poligonos los coloree en base a la categoria que pertenece el pais
  theme_classic() + #pedimos que el backgroun sea blanco
  scale_fill_brewer(palette = "Set1") + #le indicamos la paleta de colores
  labs(x="", y="", fill="Poblacion", title = "Poblacion Mundial") + #colocamos las etiquetas
  theme(plot.title= element_text(hjust = 0.5)) #con esta linea centramos el titulo
