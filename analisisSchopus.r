####################################################################################################
# ANÁLISIS BIBLIOMÉTRICO MEDIANTE BASES DE DATOS OBTENIDAS DE SCOPUS, ISI WEB OF SCIENCE Y PUBMED
####################################################################################################
# Cargar la librería bibliometrix
# La librería se descarga mediante la instrucción:
# install.packages(“bibliometrix”, dependencies=TRUE) ### instala bibliometrix y todas sus dependencias
library(bibliometrix)
library(ggplot2)

# Puede iniciarse con biblioshiny() para una appweb interactiva
####################################################################################################
# SECCIÓN 1

# Nombre del archivo BIB que se va a analizar
dbfilen = "XXX.bib"

# Obtener la información del archivo, se pueden leer varios archivos a la vez
# por eso de que las bases de datos guardan por archivos múltiples para poder 
# obtener toda la información
D <- readFiles(dbfilen)

# readFiles obtiene datos en forma de texto, por lo que se debe de
# convertir a tipo tabla
# convert2df(archivoleido, dbsource = "", format = "")
# dbsource:	"isi", "scopus", "pubmed"
# format:	"bibtext", "plaintext"
M <- convert2df(D, dbsource = "scopus", format = "bibtext")

# Ver que es lo que contiene la tabla
summary(M)

# Realizar el análisis bibliometrico, por lo que se genera 
# un tipo de variable con toda la información de los 
# artículos
results <- biblioAnalysis(M, sep = ";")

# Obtener el resumen de lo que contiene el archivo
# de forma desglosada
# Número de documentos, Autores, Coautores, tipo de documentos
# índice de colaboración = total autores en artículos multiautor / total de artículos multiautor
# Artículos por año, crecimiento anual en el número de publicaciones
# Citas, Países de los autores, citas por país, 
# revistas más relevantes, palabras clave más relevantes
summary(results, k = 20)

# Puedese crear un objeto summary para guardar los datos
# y modificando el número de elementos a mostrar
# S <- summary(object = results, k = 50, pause = FALSE, width = 130)

# Graficar algunas cuantas cosas
plot(x = results, k = 10, pause = TRUE)

# Obtener los artículos más citados
# AQUÍ SUELE HABER PROBLEMAS EN EL ANÁLISIS DE LA INFORMACIÓN
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])

# Obtener los primeros autores más citados
CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])

# Obtener los autores más citados dentro de este mismo grupo de artículos
# fast.search, TRUE solo analiza el 25% de los primeros resultados, o los top
CR <- localCitations(M, fast.search = FALSE, sep = ";")
CR$Authors[1:10,]
CR$Papers[1:10,]

# Dominancia de los autores
DF <- dominance(results, k = 10)

# Calcular el índice-H y otros de los 10 autores más productivos
hors=gsub(","," ",names(results$Authors)[1:10])
indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)
indices$H

# Productividad a lo largo del tiempo (con gráfica)
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)

# Obtener una tabla con la productividad por año (ORDENAR)
head(topAU$dfAU)

##########################################################################################
# ANÁLISIS DE LOTKA PARA PRODUCTIVIDAD CIENTÍFICA
# El análisis de Lotka, o ley de Lotka, describe la frecuencia de publicación
# de los autores en un campo del conocimiento determinado como una ley de cuadrado inverso 
# dónde el número de autores que publican un cierto número de artículos es una proporción
# fija al número de autores publicando un artículo. Esta suposición implica que el 
# coeficiente beta de la ley de Lotka es 2
L <- lotka(results)

# Productividad de los autores, distribución empírica
L$AuthorProd

# Estimado del coeficiente beta
L$Beta

# constante
L$C

# Bondad del ajuste
L$R2

# Valor p del test K-S (Kolmoorov-Smirnoff) de dos muestras
L$p.value

# Distribución observada
Observed=L$AuthorProd[,3]

# Distribución teórica con Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

# Graficar ambas
plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")


##########################################################################################
# MATRICES DE REDES BIBLIOGRÁFICAS

# Crear una red Manuscrito - Revista se usa:
A <- cocMatrix(M, Field = "SO", sep = ";")

# Ordenando se pueden observar las publicaciones más relevantes (en cuanto a número de artículos)
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

# Red con número de citas
A <- cocMatrix(M, Field = "CR", sep = ".  ")
# Autores
A <- cocMatrix(M, Field = "AU", sep = ";")

# Países - Autores
# Al no ser un atributo estándar se debe de extraer la información 
M2 <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
A <- cocMatrix(M2, Field = "AU_CO", sep = ";")
# También se pued extraer:
# AU_CO		País autor (principal)/es
# CR_AU		Primer autor en cada referencia citada
# CR_SO		Revista de cada referencia citada
# AU_UN		Afiliación de autores

# Red Autor - Palabras claves
A <- cocMatrix(M, Field = "DE", sep = ";")

# Artículos (referencias) análisis de co-citas
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Red de Co-citas", type = "fruchterman", size.cex = TRUE, size = 5,
				remove.multiple = FALSE, labelsize = 0.7, label.n=10, edgesize = 10, edges.min = 5)

#NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")
#NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")

# Calcular la red de colaboración de autores  *****
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")
#net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 150, Title = "Colaboración de Autores", type = "fruchterman", size=10, size.cex=T, remove.multiple=TRUE, labelsize=0.8, label.n=20, label.cex=F, edgesize = 5)
# EL SIGUIENTE USAR SOLO CUANDO SEA ABOSLIUTAMENTE NECESARIO Y POCOS AUTORES
#net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = dim(NetMatrix)[1], Title = "Colaboración de Autores", type = "fruchterman", size=10, size.cex=T, remove.multiple=TRUE, labelsize=0.8, label.n=50, label.cex=F, edgesize = 5)


# Colaboración por países *****
M3 <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M3, analysis = "collaboration",  network = "countries", sep = ";")
#net=networkPlot(NetMatrix,  n = dim(NetMatrix)[1], Title = "Colaboración entre países", type = "sphere", size = 10, size.cex = T, edgesize = 10, edges.min = 2, labelsize = 1.2, label.n = 20, cluster="none")
net=networkPlot(NetMatrix, normalize = "salton", weighted=NULL, n = dim(NetMatrix)[1], Title = "Colaboración entre países", type = "fruchterman", size=10, size.cex=T, remove.multiple=TRUE, labelsize=0.8, label.n=20, label.cex=F, edgesize = 5)
net=networkPlot(NetMatrix, normalize = "salton", weighted=NULL, n = dim(NetMatrix)[1], Title = "Colaboración entre países", type = "fruchterman", size=10, size.cex=T, remove.multiple=TRUE, labelsize=0.8, label.cex=F, edgesize = 3)


# Colaboración por países 2*****
M2 <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M2, analysis = "collaboration", network = "countries", sep = ";")
#net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = dim(NetMatrix)[1], Title = "Colaboración Autores-País", type = "fruchterman", size=10, size.cex=T, remove.multiple=TRUE, labelsize=0.8, label.n=20, label.cex=F, edgesize = 5, edges.min = 2)


# Colaboración por afiliación
M6 <- metaTagExtraction(M, Field = "AU_UN", sep = ";")
NetMatrix <- biblioNetwork(M6, analysis = "collaboration",  network = "authors", sep = ";")
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 40, Title = "Colaboración de Autores", type = "fruchterman", size=10, size.cex=T, remove.multiple=TRUE, labelsize=0.8, label.n=20, label.cex=F, edgesize = 5)


# Colaboración por co-citas en revistas
M4 <- metaTagExtraction(M, Field = "CR_SO", sep = ";")
NetMatrix <- biblioNetwork(M4, analysis = "co-citation",  network = "sources", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Red de Co-citas", type = "auto", size.cex = TRUE, size = 15,
				remove.multiple = FALSE, labelsize = 0.7, label.n=10, edgesize = 10, edges.min = 5)


# Análisis descriptivo de las características de las gráficas de redes
# Red de co-ocurrencia de palabras clave
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
netstat <- networkStat(NetMatrix)

# Nombres
names(netstat$network)
# Estadísticas principales de la red
summary(netstat, k=10)


# Crear una red de colaboración de países (tipo VOS Viewer) ****
M3 <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M3, analysis = "collaboration", network = "countries", sep = ";")
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")

# Crear una red de colaboración por universidades (tipo VOS Viewer) ****
M4 <- metaTagExtraction(M, Field = "AU_UN", sep = ";") # AU_UN
NetMatrix <- biblioNetwork(M4, analysis = "collaboration", network = "authors", sep = ";")
net=networkPlot(NetMatrix, n = 30, Title = "Author Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")


# Crear una red de co-citas (MODIFICAR M)
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)

# Crear una red de co-ocurrencias de palabras clave
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)



# Crear un historial de la red de citas
options(width=130)
histResults <- histNetwork(M, min.citations = 5, sep = ";")
net <- histPlot(histResults, n=15, size = 20, labelsize=10, size.cex=TRUE, arrowsize = 0.5, color = TRUE)

histResults <- histNetwork(M, min.citations = quantile(M$TC, 0.75), sep = ";")
options(width=130)
net <- histPlot(histResults, n=13, size = 5, labelsize=3, size.cex=TRUE, arrowsize = 0.5)