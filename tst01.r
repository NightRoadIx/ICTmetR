# Cargar las librerías RISmed y ggplot2
library(RISmed)
library(ggplot2)

# Query a buscar
query <- "((three dimensional)OR(3d)OR(3-d)OR (three d))AND(printing)"
res <- EUtilsSummary(query, type="esearch", db="pubmed", datetype="pdat", mindate=1900, maxdate=2018, retmax=5000)

# Hacer un conteo de los artículos obtenidos
QueryCount(res)
# Obtener los artículos
res_records <- EUtilsGet(res)

# Hallar los títulos de los artículos
t <- ArticleTitle(res_records)

# Hallar los artículos por año
y <- YearPubmed(res_records)
res_pubs_count <- as.data.frame(table(y))

# Hallar los artículos por país de afiliación del autor principal
cntry <- Country(res_records)
res_pubs_count_cntry <- as.data.frame(table(cntry))

# Hallar los artículos por afiliación de los autores
# ENCONTRAR COMO SOLO TOMAR LA AFILIACIÓN DEL AUTOR PRINCIPAL
#af <- Affiliation(res_records)
#res_pubs_count_af <- as.data.frame(table(af))

# Hallar los artículos por journal de publicación
journal <- MedlineTA(res_records)
ngs_journal_count <- as.data.frame(table(journal))


# # # # # 
# Hacer el análisis por año de publicación
# Iniciar creando un vector en NULL
total <- NULL
# Recorrer sobre todos los años
for(i in 1900:2018) {
	# Hacer un query de todos los elementos en cada año
	peryear <- EUtilsSummary("", type="esearch", db="pubmed", mindate=i, maxdate=i)
	# Se hace el conteo de los elementos hallados por año
	total[i] <- QueryCount(peryear)
}
# Se crea un vector con todos los años buscados
year <- 1900:2018
total_pubs_count <- as.data.frame(cbind(year,total[year]))
names(total_pubs_count) <- c("year","Total_publications")
names(res_pubs_count) <-  c("year","NGS_publications")
pubs_year <- merge(res_pubs_count,total_pubs_count,by="year")
pubs_year$NGS_publications_normalized <-  pubs_year$NGS_publications * 100000 / pubs_year$Total_publications

write.table(pubs_year,"NGS_publications_per_year.txt",quote=F,sep="\t",row.names=F)

# # # # # 
# Hacer el análisis por journal
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
