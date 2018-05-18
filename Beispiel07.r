##########################################################################################

# Instalar el paquete para manejar la librería rplos, para datos de la BD de PLOS ONE
install.packages("aRxiv")

# Uso de la librería
library(aRxiv)

# Funcionar para sacar el conteo de artículo con el nombre del autor "Peter Hall"
arxiv_count('au:"Peter Hall"')

# Hacer la búsqueda por autor limitando el número de artículos encontrados a 50
# por defecto este límite es 10
rec <- arxiv_search('au:"Peter Hall"', limit=50)

# Se pueden hacer búsquedas más específicas, por ejemplo considerando:
# autor: Peter Hall y en el Titulo la palabra deconvolution
deconv <- arxiv_search('au:"Peter Hall" AND ti:deconvolution')

# Se puede utilizar para crear el query
##				termino								Descripción
## 1			ti									Titulo
## 2			au									Autor
## 3			abs									Abstract
## 4			co									Comentarios
## 5			jr									Journal de Referencia
## 6			cat                                 Catgoría del tema
## 7			rn									Reporte Número
## 8			all                                 Todo lo anterior
## 9			submittedDate 						Fecha/Tiempo de presentación inicial, como YYYYMMDDHHMM
## 10			lastUpdatedDate						Fecha/Tiempo de última actualización, como YYYYMMDDHHMM

rec <- arxiv_search('all:"hydrogel optical phantom"', limit=100)
