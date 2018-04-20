# Instalar el paquete para manejar la librería rscopus, para datos de la BD de SCOPUS
# install.packages("rscopus")


##########################################################################################

# Instalar el paquete para manejar la librería rplos, para datos de la BD de PLOS ONE
install.packages("rplos")

library('ggplot2')

# Utilizar la libreria de PLOS
library(rplos)
# Obtener los campos que pueden ser buscados mediante la librería RPLOS
head(plosfields)
# o mediante
plosfields

# Hacer la búsqueda pidiendo todos los datos a guardar
res <- searchplos('optical phantom hydrogel', 'id,author, title, alternate_title,  title_display, publication_date,cross_published_journal_key,journal', limit = 50)

# Revisar datos
head(res$data)

# Metadatos obtenidos
res$meta


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


