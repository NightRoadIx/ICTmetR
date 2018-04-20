library(RISmed)
res <- EUtilsSummary("pinkeye", type="esearch", db="pubmed", datetype='pdat', mindate=2000, maxdate=2015, retmax=500)

t<-ArticleTitle(EUtilsGet(res))
typeof(t)
head(t,1)
t[2]

# Usar de preferencia YearPubmed, pues no contiene tantos NA
y <- YearPubmed(EUtilsGet(res))
r <- YearReceived(EUtilsGet(res))
y
r

# Cargar la libreria ggplot2
library(ggplot2)
date()
# Contar ocurrencias de los datos en el vector y
count<-table(y)
# Crear una tabla con datos y frecuencias
count<-as.data.frame(count)
# Asignar nombres a los datos y las fecuencias
names(count)<-c("Year", "Counts")
# Crear una suma acumulada de los datos de frecuencia (FDA)
num <- data.frame(Year=count$Year, Counts=cumsum(count$Counts))
# Añadir una "g"
num$g <- "g"
# Asegurar los nombres o encabezados de la tabla
names(num) <- c("Year", "Counts", "g")

q <- qplot(x=Year, y=Counts, data=count, geom="bar", stat="identity")
q <- q + ggtitle(paste("PubMed articles containing \'", g,            "\' ", "= ", max(num$Counts), sep="")) +
     ylab("Number of articles") +
     xlab(paste("Year \n Query date: ", Sys.time(), sep="")) +
     labs(colour="") +
     theme_bw()
q



library(qdap)
myFunc<-function(argument){
articles1<-data.frame('Abstract'=AbstractText(fetch), 'Year'=YearPubmed(fetch))
abstracts1<-articles1[which(articles1$Year==argument),]
abstracts1<-data.frame(abstracts1)
abstractsOnly<-as.character(abstracts1$Abstract)
abstractsOnly<-paste(abstractsOnly, sep="", collapse="")
abstractsOnly<-as.vector(abstractsOnly)
abstractsOnly<-strip(abstractsOnly)
stsp<-rm_stopwords(abstractsOnly, stopwords = qdapDictionaries::Top100Words)
ord<-as.data.frame(table(stsp))
ord<-ord[order(ord$Freq, decreasing=TRUE),]
head(ord,20)
}

oSix<-myFunc(2006)
oSeven<-myFunc(2007)
all<-cbind(oSix, oSeven)
names(all)<-c("2006","freq","2007","freq")

all
