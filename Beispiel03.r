# Instalar el paquete para manejar la librería rscopus, para datos de la BD de SCOPUS
# install.packages("rscopus")


##########################################################################################

# Instalar el paquete para manejar la librería rplos, para datos de la BD de PLOS ONE
install.packages("rplos")

# Otros páquetes que pueden ser instalados
install.packages("readr")	# Para la lectura de datos
install.packages("plyr")	# Para 'wrangling' (exprimir) datos
install.packages("dplyr")	# Para 'wrangling' (exprimir) datos
install.packages("tidyr")	# Para ordenar datos
install.packages("stringr")	# Para manipular cadenas (muy importante)
install.packages("tm")		# Para hacer mineria de textos
install.packages("XML")		# Para tratar con texto en XML

# Ahora cargar las librerias
library('ggplot2')
# Utilizar la libreria de PLOS
library(rplos)
library(readr)
library(plyr)  				# Cargar esta librería antes que dplry para evitar errores
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(XML)


# Obtener los campos que pueden ser buscados mediante la librería RPLOS
head(plosfields)
# o mediante
plosfields

# Hacer la búsqueda pidiendo todos los datos a guardar
res <- searchplos('optical phantom hydrogel', 'id,author, title, alternate_title,  title_display, publication_date,cross_published_journal_key,journal', limit = 50)
# puede pedirse ayuda de la función mediante '?'(searchplos)
# pedir de que clase es el resultado
class(res)
# o la estructura
str(res)

# Revisar los metadatos
head(res$meta)			# = res[[1]]
# Número de elementos hallados
res$meta$numFound		# = res[[1]][[1]]

# Revisar datos
head(res$data)

# Buscar varios datos y guardarlos en un vector de Metadatos
q <- c('optical','phantom','hydrogel')
# Aplicar a la función de searchplos
lapply(q, function(x) searchplos(x, limit=10))


# Mas específico está para buscar 
# Por Abstract (o dentro del abstract)
res1 <- plosabstract(q = 'optical phantom', 'id,author, title, alternate_title,  title_display, publication_date,cross_published_journal_key,journal', limit = 5)

# Por título
res2 <- plostitle(q = 'optical phantom', 'id,author, title, alternate_title,  title_display, publication_date,cross_published_journal_key,journal', limit = 5)


# Buscar por términos y visualizar los resultados
# PLOSWORD permite buscar palabras y visualizarlas en un histograma por el número de elementos encontrados
res3 <- plosword(list('optical phantom', 'phototherapy', 'photodynamic therapy', 'hydrogel'), vis = 'TRUE')

# Se obtiene una tabla
res3$table

# O la gráfica
res3$plot

# Graficar a través del tiempo como se han desarrollado los artículos con algún término
# (solo pueden buscarse hasta 2 términos)
plot_throughtime(terms = "optical phantom", limit = 200) + geom_line(size=2, color='black')

# Hacer una búsqueda por ejemplo, de cuántos artículos hay dependiendo de los artículos de PLOS
# PLOS One, PLOS Genetics, etc...
facetplos(q='*:*', facet.field='journal')

# Para incuir un query con conteos   ???
facetplos(q='*:*', facet.field='journal', facet.query='optical,phantom')


# Mas info
# Obtener el directorio actual
getwd()

# pasar la información a una tabla
dat <- res$data

# Escribir la información en un archivo
write.csv(dat, "dat.csv", row.names = FALSE)

# Pude usarse este modificador fq = 'cross_published_journal_key:PLoSONE'
# dentro de searchplos para limitar en que journal de los 7 disponibles en PLOS habrá que hacer la búsqueda
