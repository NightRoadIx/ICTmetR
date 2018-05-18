# Cargar las librerías RISmed y ggplot2
library(RISmed)
library(ggplot2)

# Query a buscar
#query <- "((three dimensional)OR(3d)OR(3-d)OR (three d))AND(printing)"
#query <- "hydrogel AND (optical AND phantom)"
query <- "(tissue)AND(phantom)AND(optical)"
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

# Hallar los artículos por journal de publicación
journal <- MedlineTA(res_records)
ngs_journal_count <- as.data.frame(table(journal))

# Hallar los artículos por afiliación de los autores
# ENCONTRAR COMO SOLO TOMAR LA AFILIACIÓN DEL AUTOR PRINCIPAL
af <- Affiliation(res_records)
# El vector trae consigo todas las afiliaciones de los autores
# solo se considerará la afiliación del autor principal
afiliaciones <- NULL
for(i in 1:length(af)) {
	afiliaciones[i] <- af[[i]][1]
}
# FALTA TRATAR LAS CADENAS PARA OBTENER SOLO LA {UNIVERSIDAD}/EMPRESA Y DEPARTAMENTO
#res_pubs_count_af <- as.data.frame(table(af))

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
# Crear un conteo del número de publicaciones en una tabla con los años y el total de publicaciones por año
total_pubs_count <- as.data.frame(cbind(year,total[year]))
# Cambiar los nombres de las cabeceras de las tablas por año y total de publicaciones
names(total_pubs_count) <- c("year","Total_publications")
# Cambiar los nombres de las cabeceras de las tablas por año y publicaciones buscadas
names(res_pubs_count) <-  c("year","NGS_publications")
# Mezclar ambas tablas para crear una sola por años
pubs_year <- merge(res_pubs_count,total_pubs_count,by="year")
# Normalizar la tabla creada por las publicaciones buscadas
pubs_year$NGS_publications_normalized <- pubs_year$NGS_publications * 100000 / pubs_year$Total_publications

# Crear un archivo a partir de esta tabla
write.table(pubs_year,"NGS_publications_per_year.txt",quote=F,sep="\t",row.names=F)

# # # # # 
# Hacer el análisis por journal
# Ordenar y luego obtener los 10 journals con una mayor cantidad de publicaciones de la búsqueda
ngs_journal_count_top10 <- ngs_journal_count[order(-ngs_journal_count[,2]),][1:10,]
#ngs_journal_count_top10 <- ngs_journal_count[order(-ngs_journal_count[,2]),][,]	# Ordenar todos

# Obtener los nombres de los journals y añadirles "[jo]" y separarlos con un ""
journal_names <- paste(ngs_journal_count_top10$journal,"[jo]",sep="")

# Crear un vector total_journal
total_journal <- NULL
# Recorrer todo el vector de journal_names
for (i in journal_names){
	# Hacer la búsqueda de todos los artículos de los journals obtenidos como los 10 primeros en el tema
	perjournal <- EUtilsSummary(i, type='esearch', db='pubmed',mindate=1900, maxdate=2018)
	# Hacer el conteo
	total_journal[i] <- QueryCount(perjournal)
}
# Unir las tablas con los nombres de los journals, el total de artículos hallados con el query y el total publicados en dichas revistas
journal_ngs_total <- cbind(ngs_journal_count_top10,total_journal)
# Cambiar los encabezados de la tabla
names(journal_ngs_total) <- c("journal","NGS_publications","Total_publications")
# Añadir una columna con la proporción de artículos hallados entre el total publicados por la revista
journal_ngs_total$NGS_publications_normalized <- journal_ngs_total$NGS_publications / journal_ngs_total$Total_publications

write.table(journal_ngs_total,"NGS_publications_per_journal.txt",quote=F,sep="\t",row.names=F)


# # # # # 
# Gráficas
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