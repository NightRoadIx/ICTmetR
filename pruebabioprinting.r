# Instalar el paquete para manejar la librería RISmed, para datos de la BD de PUBMed
install.packages("RISmed")
install.packages("ggplot2")

library(RISmed)
library(ggplot2)

# (printing AND(optical AND phantom))OR photonic
# (printing AND(optical AND phantom))
# query <- "(printing AND(optical AND phantom))"
# query <- "printing AND phantom"
query <- "hydrogel AND (optical AND phantom)"
res<-EUtilsSummary(query, type="esearch", db="pubmed", datetype="pdat", mindate=1980, maxdate=2018, retmax=5000)

QueryCount(res)
res_records <- EUtilsGet(res)
t <- ArticleTitle(res_records)

# # # # # # # # # # # # # # # # # # # # # # # # # #
# Información (importante) que se puede obtener:
# Afiliación de los investigadores
#af <- Affiliation(res_records)
# País de afiliación del autor principal
#cou <- Country(res_records)
# Texto del abstract
#abstr <- AbstractText(res_records)
# Texto del títuloi
#ArticleTitle(res_records)
# Año de la publicación
#YearPubmed(res_records)
# Contiene las cabeceras mesh  con descriptores y calificadores del artículo
#mesh(res_records)
# Número de citas que tiene el artículo en PubMed
#Cited(res_records)
# Tipo de publicación
#PublicationType(res_records)
# Revista de publicación (nombre corto)
#journal <- MedlineTA(res_records)
# # # # # # # # # # # # # # # # # # # # # # # # # #

# Por año
y <- YearPubmed(res_records)
res_pubs_count<-as.data.frame(table(y))

total <- NULL
for(i in 1900:2018) {
	peryear <- EUtilsSummary("", type="esearch", db="pubmed", mindate=i, maxdate=i)
	total[i] <- QueryCount(peryear)
}
year <- 1900:2018
total_pubs_count <- as.data.frame(cbind(year,total[year]))
names(total_pubs_count) <- c("year","Total_publications")
names(res_pubs_count) <-  c("year","NGS_publications")
pubs_year <- merge(res_pubs_count,total_pubs_count,by="year")
pubs_year$NGS_publications_normalized <-  pubs_year$NGS_publications * 100000 / pubs_year$Total_publications

write.table(pubs_year,"NGS_publications_per_year.txt",quote=F,sep="\t",row.names=F)

# Por journal
journal <- MedlineTA(res_records)
ngs_journal_count <- as.data.frame(table(journal))
ngs_journal_count_top10 <- ngs_journal_count[order(-ngs_journal_count[,2]),][1:10,]

journal_names <- paste(ngs_journal_count_top10$journal,"[jo]",sep="")

total_journal <- NULL
for (i in journal_names){
	perjournal <- EUtilsSummary(i, type='esearch', db='pubmed',mindate=1980, maxdate=2018)
	total_journal[i] <- QueryCount(perjournal)
}

journal_ngs_total <- cbind(ngs_journal_count_top10,total_journal)
names(journal_ngs_total) <- c("journal","NGS_publications","Total_publications")
journal_ngs_total$NGS_publications_normalized <- journal_ngs_total$NGS_publications / journal_ngs_total$Total_publications

write.table(journal_ngs_total,"NGS_publications_per_journal.txt",quote=F,sep="\t",row.names=F)

pubs_per_year <- read.table("NGS_publications_per_year.txt",header = T,sep="\t")
pubs_per_journal <- read.table("NGS_publications_per_journal.txt",header = T,sep="\t")

ggplot(pubs_per_year,aes(year, NGS_publications_normalized)) + geom_line (colour="blue",size=2) +
xlab("Year") +
ylab("NGS/100000 articles")+
ggtitle("NGS PubMed articles")
 
ggplot(pubs_per_journal,aes(journal, NGS_publications,fill=journal)) + geom_bar(stat="identity")+
coord_flip()+
theme(legend.position="none")
 
ggplot(pubs_per_journal ,aes(journal, NGS_publications_normalized,fill=journal)) + geom_bar(stat="identity")+
coord_flip()+
theme(legend.position="none")



# Usar data.frame() para juntar todos los valores






names(count)<-c("Year", "Counts")
num <- data.frame(Year=count$Year, Counts=cumsum(count$Counts))


res<-EUtilsSummary("bioprinting", type="esearch", db="pubmed", datetype="pdat", mindate=2000, maxdate=2015, retmax=500)
res<-EUtilsSummary("optical phantom", type="esearch", db="pubmed", datetype="pdat", mindate=2000, maxdate=2015, retmax=500)

QueryCount(res)

t<-ArticleTitle(EUtilsGet(res))

y <- YearPubmed(EUtilsGet(res))
r <- YearReceived(EUtilsGet(res))


library(ggplot2)
date()
count<-table(y)
count<-as.data.frame(count)


names(count)<-c("Year", "Counts")
num <- data.frame(Year=count$Year, Counts=cumsum(count$Counts))
num$g <- "g"


names(num) <- c("Year", "Counts", "g")
q <- qplot(x=Year, y=Counts, data=count, geom="bar", stat="identity")
q <- q + ggtitle(paste("Artículos en PubMed que contienen \'", g,            "\' ", "= ", max(num$Counts), sep="")) +
     ylab("Número de artículos") +
     xlab(paste("Año \n Fecha de consulta: ", Sys.time(), sep="")) +
     labs(colour="") +
     theme_bw()
q