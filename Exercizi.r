# Ingresar datos en una variable
height = c(176, 154, 138, 196, 132, 176, 181, 169, 150, 175)
weight = c(82, 49, 53, 112, 47, 69, 77, 71, 62, 78)

# Información estadística de la variable añadida
summary(height)
# Rango de la variable
range(height)
# Promedio
mean(height)
# Desviación estándar
sd(height)
# Valor máximo
max(height)
# Valor mínimo
min(height)
# Longitud
length(height)

# Obtener valores individuales
height[1]
height[3]
height[10]

# Generar una gráfica con los datos
# plot(x,y, tipo de punto, número de veces tamaño, color, título, xlabel, yabel)
plot(weight, height, pch = 2, cex = 1.3, col = "red", main = "MY FIRST PLOT USING R", xlab = "WEIGHT (kg)", ylab = "HEIGHT (cm)")

# Realizar una regresión lineal
lm(height~weight)
# Colocar la gráfica de la línea que representa la regresión lineal
abline(98.0054, 0.9528)

# Guardar las regresión lineal en una variable
mod <- lm(height~weight)
# Información de la variable
summary(mod)

# Ahora crear un vector de valores ajustados al modelo
regmodel <- predict(mod)
plot(weight, height, pch = 2, cex = 1.3, col = "blue", main = "MY FIRST PLOT USING R", xlab = "WEIGHT (kg)", ylab = "HEIGHT (cm)")
abline(mod)

# Obtener el tamaño del vector height
npoints <- length(height)

# Hacer un ciclo sobre el tamaño del vector encontrado en la instrucción kengt
for (k in 1:npoints)  
	lines(c(weight[k], weight[k]), c(height[k], regmodel[k]))
	

#################################
# Regresión cuadrática
# Crea una estructura como tabla con 2 columnas, tiempo y Conteo
A <- structure(list(Time = c(0, 1, 2, 4, 6, 8, 9, 10, 11, 12, 13, 
14, 15, 16, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30), 
Counts = c(126.6, 101.8, 71.6, 101.6, 68.1, 62.9, 45.5, 41.9, 
46.3, 34.1, 38.2, 41.7, 24.7, 41.5, 36.6, 19.6, 
22.8, 29.6, 23.5, 15.3, 13.4, 26.8, 9.8, 18.8, 25.9, 19.3)), .Names = c("Time", "Counts"),
row.names = c(1L, 2L, 3L, 5L, 7L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 19L, 20L, 21L, 22L, 23L, 25L, 26L, 27L, 28L, 29L, 30L, 31L),
class = "data.frame")

# Se hace un attach de la estructura de datos para referirse a las variables directamente por el nombre
# Sino no es fácil hacer referencia a esos datos
attach(A)
# Ver los nombres
names(A)

# Crear un modelo lineal de los datos
linear.model <- lm(Counts ~ Time)
# Ver sus detalles
summary(linear.model)

# Graficar los datos
# cex.lab produce ejes de un tamaño adecuado
plot(Time, Counts, pch=16, ylab = "Counts ", cex.lab = 1.3, col = "red" )
# Graficar el modelo de regresión lineal
abline(linear.model, col = "blue")

# tiempo al cuadrado
Time2 <- Time^2
# Generar una regresión cuadrática a partir de un modelo lineal
quadratic.model <-lm(Counts ~ Time + Time2)
summary(quadratic.model)

# Obtener una secuencia de datos de 0 a 30 con separación de 0.1
timevalues <- seq(0, 30, 0.1)
predictedcounts <- predict(quadratic.model,list(Time=timevalues, Time2=timevalues^2))

# Graficar
plot(Time, Counts, pch=16, xlab = "Time (s)", ylab = "Counts", cex.lab = 1.3, col = "blue")
lines(timevalues, predictedcounts, col = "darkgreen", lwd = 3)


#########
# Ajuste a una exponencial
A <- structure(list(Time = c(0, 1, 2, 4, 6, 8, 9, 10, 11, 12, 13, 
14, 15, 16, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30), 
Counts = c(126.6, 101.8, 71.6, 101.6, 68.1, 62.9, 45.5, 41.9, 
46.3, 34.1, 38.2, 41.7, 24.7, 41.5, 36.6, 19.6, 
22.8, 29.6, 23.5, 15.3, 13.4, 26.8, 9.8, 18.8, 25.9, 19.3)), .Names = c("Time", "Counts"), row.names = c(1L, 2L,
3L, 5L, 7L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 19L, 20L, 21L, 22L, 23L, 25L, 26L, 27L, 28L, 29L, 30L,
31L), class = "data.frame")

attach(A)
names(A)

# Hacer la regresión lineal con los datos
exponential.model <- lm(log(Counts)~ Time)
summary(exponential.model)

timevalues <- seq(0, 30, 0.1)
Counts.exponential2 <- exp(predict(exponential.model,list(Time=timevalues)))
plot(Time, Counts,pch=16)
lines(timevalues, Counts.exponential2,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")

##############################
# Graficación
x <- seq(-4, 4, 0.2)
y <- 2*x^2 + 4*x - 7

plot(x,y)
# Gráfica con círculos sólidos y en color rojo
plot(x, y, pch = 16, col = "red")
# Gráfica con círculos sólidos rojos, límites en x & y definidos, título, etiquetas en los ejes
plot(x, y, pch = 16, col = "red", xlim = c(-8, 8), ylim = c(-20, 50),
main = "MY PLOT", xlab = "X VARIABLE" , ylab = "Y VARIABLE")
# La línea que une los puntos
lines(x, y)

# Generar líneas horizontales
abline(h = 0)
# verticales
abline(v = 0)
# Dibujar una línea con intercepto en -10 y pendiente 2
abline(-10, 2)
# Texto localizado en el punto (4, 20)
text(4, 20, "TEXT")
# Leyenda centrada en (-8, 36)
legend(-8, 36, "Texto")
# Indica los puntos en el eje x, en donde la gráfica tiene un varlor
rug(x)

# Grafica avanzada
X <- c(3, 4, 6, 6, 7, 8, 9, 12)
B1 <- c(4, 5, 6, 7, 17, 18, 19, 22)
B2 <- c(3, 5, 8, 10, 19, 21, 22, 26)
# Superponer otra
plot(X, B1, type="o", pch = 17, cex=1.2, col="darkgreen", ylim=c(0, 25))
lines(B2, type="o", pch=16, lty=2, col="blue")
title(main="MY FIRST R PLOT", col.main="blue", font.main=2)

B1 <- c(1, 2, 4, 5, 7, 12, 14, 16, 19)
B2 <- c(2, 3, 6, 7, 8, 9, 11, 12, 18)
B3 <- c(0, 1, 7, 3, 2, 2, 7, 9, 13)
yaxismax<- max(B1, B2, B3)
yaxismax
plot(B1, pch = 15, type="o", col="blue", ylim=c(0, yaxismax),axes=FALSE, ann=FALSE) 
axis(1, at=1:9, lab=c("A","B","C","D","E", "F", "G","H", "I"))
axis(2, las=1, at=2*0: yaxismax)
box()
lines(B2, pch = 16, type="o", lty=2, col="red")
lines(B3, pch = 17, type="o", lty=3, col="darkgreen")
title(main="MY LINE PLOTS IN R", col.main="red", font.main=2)
title(xlab=toupper("ALPHABET"), col.lab="purple")
title(ylab="NUMERALS", col.lab="brown")
legend(1, yaxismax, c("B1","B2", "B3"), cex=0.7, col=c("blue", "red", "darkgreen"),
pch=c(15, 16, 17), lty=1:3)


##########################
# Comandos
a <- c(3,-7,-3,-9,3,-1,2,-12, -14)
b <- c(3,7,-5, 1, 5,-6,-9,16, -8)
d <- c(1,2,3,4,5,6,7,8,9)

a*a 
a + b 
a - d 
a / d 
a*d
a^2
# Un elemento
a[7] 
# Varios elementos del arreglo
a[c(1,4,7)]
# Sin los elementos declarados
a[-c(5,6)] 
length(a)
# Ultimo elemento
a[length(a)] 
# sin el último elemento
a[-length(a)] 
length(a[c(1, 2, 3)])
a[8] 
a[c(7, 5, 3)] 
# Obtiene el resultado de cada uno de los elementos
a>2
# Obtiene un vector con todos los elementos que cumplan la condición
a[a >2]
a[-c(3, 5, 6)] 
a^2 
# Kgual que el anterior
a**2
a + 99
min(b)
# valor mínimo de los 3 vectores
min(a,b,d)
max(a,b,d)
max(b)
mean(a)
median(d)
range(a) 
unique(a,b, d)
length(unique(a,b, d))

# quitar el último elemento de manera permanente
a <- a[-length(a)]
a

########################################
# Tablas de datos

A <- structure(list(NATION = structure(c(3L, 3L, 3L, 1L, 3L, 2L, 3L, 1L, 3L, 3L, 1L, 2L, 2L, 3L, 3L, 3L, 2L), 
.Label = c("CHINA", "GERMANY", "FRANCE"), class = "factor"), 
GENDER = structure(c(1L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 1L), 
.Label = c("F", "M"), class = "factor"), 
CHILDREN = c(1L, 3L, 2L, 2L, 3L, 1L, 0L, 1L, 0L, 1L, 2L, 2L, 1L, 1L, 1L, 0L, 2L)), 
.Names = c("NATION", "GENDER", "CHILDREN"), row.names = 2:18, class = "data.frame")

# La forma genérica para los subsets es:
# Z <- A[ A[ , colnum ] == val, ]
# Esto determina u obtiene el subset cuando en la segunda columna el valor es 'F'
FE <- A[ A[, 2] == "F", ]

# pero esto es un poco más sencillo
FE <- subset(A, GENDER == "F")

# Cuando el valor de la tercera columna sea menor a 2
C1<- A[ A[, 3] <2, ] 
# esto es similae
C1<- subset(A, CHILDREN <2)

# Y si lo qu quiere es encontrarse la intersección entre ambos
F1<- A[ A[, 2] == "F"&A[, 3] < 2, ]
# con el subset
F1<- subset(A, GENDER == "F"&CHILDREN <2) 


# Crear gráficas de barras o histogramas

H <- c(2,3,3,3,4,5,5,5,5,6)
# Cuenta los elementos disponibles en una tabla
counts <- table(H)
counts
H
# Hacer una gráfica de barras
barplot(counts)

# Si estos datos ya representan el número de elementos en un conjunto de datos (o su frecuencia)
B <- c(3, 2, 25, 37, 22, 34, 19)
# Se puede graficar de manera directa
barplot(B, col="darkgreen")

# Una gráfica más profesional
barplot(B, main="MY NEW BARPLOT", xlab="LETTERS", ylab="MY Y VALUES", names.arg=c("A","B","C","D","E","F","G"),
border="red", density=c(90, 70, 50, 40, 30, 20, 10))

# Otro ejemplo con una tabla de 4 elementos (columnas) de 5 elementos cada una (filas)
data <- structure(list(W= c(1L, 3L, 6L, 4L, 9L), X = c(2L, 5L, 4L, 5L, 12L), Y = c(4L, 4L, 6L, 6L, 16L), Z = c(3L, 5L, 6L, 7L, 6L)), 
.Names = c("W", "X", "Y", "Z"), class = "data.frame", row.names = c(NA, -5L))
attach(data)
print(data)
colours <- c("red", "orange", "blue", "yellow", "green")
# Graficar el histograma, serán entonces 4 grupos de 5 barras
barplot(as.matrix(data), main="My Barchart", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)
# Añadir una leyenda
legend("topleft", c("First","Second","Third","Fourth","Fifth"), cex=1.3, bty="n", fill=colours)


# Segunda sección de la creación de histogramas
B <- c(2, 4, 5, 7, 12, 14, 16)
hist(B)

# Estos son los datos de una tabla
A <- structure(list(James = c(1L, 3L, 6L, 4L, 9L), Robert = c(2L, 5L, 4L, 5L, 12L), David = c(4L, 4L, 6L, 6L, 16L), Anne = c(3L, 5L, 6L, 7L, 6L)), 
.Names = c("James", "Robert", "David", "Anne"), class = "data.frame", row.names = c(NA, -5L))
attach(A)
A

# El truco se trata de transformar las 4 variables en un vector simple y hacer el histograma de todos los elementosB <- c(A$James, A$Robert, A$David, A$Anne)
B <- c(A$James, A$Robert, A$David, A$Anne)
hist(B, col="darkgreen", ylim=c(0,10), ylab ="MY HISTOGRAM", xlab="FREQUENCY")

# 
bins<- c(0, 4, 8, 12, 16)
# Generar de nuevo el histograma
hist(B, col = "blue", breaks=bins, main="My Histogram", las=2, xlab = "Values", cex.lab = 1.3)


####################################
# Crear gráficas de cajas
A <- c(3, 2, 5, 6, 4, 8, 1, 2, 3, 2, 4)
boxplot(A)

# Abrir una lista de automóviles que contiene el R
print(mtcars)

# COMO SE OBSERVA ESTO EN UNA TABLA?  var ~ var
boxplot(wt~cyl, data=mtcars, main=toupper("Vehicle Weight"), font.main=3, cex.main=1.2, xlab="Number of Cylinders", ylab="Weight", font.lab=3, col="darkgreen")
boxplot(mpg~cyl, data=mtcars, main= toupper("Fuel Consumption"), font.main=3, cex.main=1.2, col=c("red","blue", "yellow"), xlab="Number of Cylinders", ylab="Miles per Gallon", font.lab=3, notch=TRUE, range = 0)

####################################
# Crear gráficas de pastel

B <- c(2, 4, 5, 7, 12, 14, 16)
pie(B)
# Controlar el color de cada uno de los "pedazos" con rainbow y la longitud de B
pie(B, main="My Piechart", col=rainbow(length(B)), labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

# Supongamos los valores para cada uno de los días de la semana
B <- c(5, 3, 1, 8, 9, 4, 6)
# Establecer los colores negro, gris y blanco para una impresión clara
cols <- c("grey90","grey50","black","grey30","white","grey70","grey50")
# Calcular el procentaje para cada día, usando 1 dígito de precisión
percentlabels <- round(100*B/sum(B), 1)
# Añadir el caracter '%' al valor de los porcentajes
pielabels<- paste(percentlabels, "%", sep="")
# Imprimir la gráfica
pie(B, main="My Best Piechart", col=cols, labels=pielabels, cex=0.8)
# Añadir una leyenda
legend("topright", c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), cex=0.8, fill=cols)

# Ahora de la misma tabla obtener número de cilindros de los mtcars
cyltable<- table(mtcars$cyl)
cyltable
# Se generan las etiquetas
labs<- paste("(",names(cyltable),")", "\n", cyltable, sep="")

pie(cyltable, labels = labs, col = c("red", "yellow", "blue"), main="PIE CHART OF CYLINDER NUMBERS\n with sample sizes")

####################################
# Gráficas múltiples

# Se generan 4 vectores de la misma longitud
X <- c(1, 2, 3, 4, 5, 6, 7)
Y1 <- c(2, 4, 5, 7, 12, 14, 16)
Y2 <- c(3, 6, 7, 8, 9, 11, 12)
Y3 <- c(1, 7, 3, 2, 2, 7, 9)

# Se genera un ambiente de graficación con 2 filas y 3 columnas
par(mfrow=c(2,3))

# y se mandan a graficar
plot(X,Y1, pch = 1)
plot(X,Y2, pch = 2)
plot(X,Y3, pch = 3)
plot(X,Y1, pch = 4)
plot(X,Y2, pch = 15)
plot(X,Y3, pch = 16)

####################################
# Crear un vector
b <- c(7, 2, 4, 3, -1, -2, 3, 3, 6, 8, 12, 7, 3)
# Contar las ocurrencias del número "3"
count3 <- length(which(b == 3)) 
# Verificar los valores que sean menores a 7
length(which(b < 7))

# Esta puede ser una alternativa, crear "subsets"
length(b[ b < 7 ])

# Otra alternativa es la siguiente
sum(b <- 7)

# Tener cuidado con esto
mean(b < 7)
# Ya que se obtiene la propoción de elementos que cubren dicho criterio, no realmente el promedio

# Encontrar el porcentaje de "7" que se encuentran en el vector
P7 <- 100 * length(which(b == 7)) / length(b)

# Crear las funciones
# Para conteo de elementos en un vector
count <- function(x, n){ length((which(x == n))) }
# Obtener el porcentaje de ocurrencias de un elemento en un vector
perc <- function(x, n){ 100*length((which(x == n))) / length(x) }

####################################
# Puede generarse esta función
countcases1 <- function(x, n) { apply(x, 1, function(r) sum(r == n)) } 
# y esta otra:
countcases2 <- function(x, n) { rowSums(x == n) }

# Se crrea un arreglo rectangular
M <- structure(c(1, 4, 5, 4, 5, 5, 3, 2, 5, 5, 5, 4, 5, 2, 5), .Dim = c(3L, 5L), .Dimnames = list(c("David", "Mary", "Anne"), NULL))

colnames(M) <- c("Item1", "Item2", "Item3", "Item4", "Item5")

# Contamos los 5 en las filas de la matriz MY
countcases1(M, 5)

# Usamos la funcion 2 para contar los "2"
countcases2(M, 2)

# Tomamos los datos de la primera fila con
countcases1(M, 5)[1]
# O mediante la expresión
countcases1(M, 5)["David"]

####################################
b <- c(7, 2, 4, 3, -1, -2, 3, 3, 6, 8, 12, 7, 3)
# Verifica si hay agún elemento en el vector que sea igual al valor
any(b == -4)		# Regresa true o false, dependiendo del caso
any(b < 5)

#Verificar si hay datos que faltan
d <- c(3, 2, NA, 5, 6, NA)
any(is.na(d))
any(!is.na(d))		# O también saber si no existen datos faltantes
# El comando any() es muy útil para verificar valores particulaes en grandes grupos de datos

# Verificar si TODOS los valores que no esten perdidos (los no NA) son  menores a 5
all(d[!is.na(d)] < 5)
all(d[!is.na(d)] < 7)	# o si son menores a 7

# Recordar que la siguiente instrucción
!is.na(d)
# Arroja un vector lógico que analiza dato por dato
# Mientras que :
d[!is.na(d)]
# Arroja un vector con todos los valores que no están perdidos

####################################
# Rellenar los datos

# Si se tuvieran datos con valores NA
A <- c(3, 2, NA, 5, 3, 7, NA, NA, 5, 2, 6)

# Y se quisieran sustituir con un valor que fuera "operable", digamos 0
A[ is.na(A) ] <- 0
# O todos los valores menores a 5 los cambio a 99
A[ A < 5 ] <- 99

gender <- c("MALE","FEMALE","FEMALE","UNKNOWN","MALE")
# Si se quiere pasar a números, puede usarse el operador ifelse
ifelse(gender == "MALE", 1, ifelse(gender == "FEMALE", 2, 3))

# Ahora para el caso de una tabla
A <- data.frame(Gender = c("F", "F", "M", "F", "B", "M", "M"), Height = c(154, 167, 178, 145, 169, 183, 176))
# Recodificar para decir que M es 1, F es 2, y si no, es 99
A[,1] <- ifelse(A[,1] == "M", 1, ifelse(A[,1] == "F", 2, 99	))		# Matlab code A(:,1)

S <- data.frame(SPECIES = c("ORC", "HOBBIT", "ELF", "TROLL", "ORC", "ORC", "ELF", "HOBBIT"), HEIGHT = c(194, 127, 178, 195, 149, 183, 176, 134))
# Recodificar
S[,1] <- ifelse(S[,1] == "ORC", 1, ifelse(S[,1] == "ELF", 2, ifelse(S[,1] == "HOBBIT", 3, ifelse(S[,1] == "TROLL", 4, 99))))
# Re-recodificar
S[,1] <- ifelse(S[,1] == 1, "ORC", ifelse(S[,1] == 2, "ELF", ifelse(S[,1] == 3, "HOBBIT", ifelse(S[,1] == 4, "TROLL", 99))))


####################################
# Comandos básicos para mover datos

A <- structure(list(NATION = structure(c(3L, 3L, 3L, 1L, 3L, 2L, 3L,
1L, 3L, 3L, 1L, 2L, 2L, 3L, 3L, 3L, 2L), .Label = c("CHINA",
"GERMANY", "FRANCE"), class = "factor"), GENDER = structure(c(1L,
2L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 1L
), .Label = c("F", "M"), class = "factor"), CHILDREN = c(1L,
3L, 2L, 2L, 3L, 1L, 0L, 1L, 0L, 1L, 2L, 2L, 1L, 1L, 1L, 0L, 2L
)), .Names = c("NATION", "GENDER", "CHILDREN"), row.names = 2:18, class = "data.frame")

# Ver los 3 primeros datos de la tabla
head(A,3)

# o los últimos 4 datos
tail(A,4)

# Numero de filas
nrow(A)
# Numero de columnas
ncol(A)

# Al hacer attach(), hace posible que las variables de la tabla puedan ser reeferidas sin problemas en el entorno
# sin necesidad de especificar que pertenecen a dicha variable
attach(A)

# Revisar si algún valor de NATION es "USA"
any(NATION == "USA")

# los valores de la variable NATION entonces
levels(NATION)

# Cantidad de datos en la variable NATION
length(NATION)
# Pero, ¿cuántos de ellos son diferentes?
length(levels(NATION))

# Existe algún valor de CHILDREN mayor a 3
any(CHILDREN > 3)

# Hay algún dato faltante o NA
any(is.na(A))

# Cuales observaciones involucran el término "FRANCE"
which(A == "FRANCE")
# y cuantas
length(which(A == "FRANCE"))

####################################
# Remover datos
rm(A)

# Crear una tabla más grande
T <- structure(list(COUNTRY = structure(c(3L, 3L, 3L, 3L, 1L, 3L, 2L, 3L, 1L, 3L, 3L, 1L, 2L, 2L, 3L, 3L, 3L, 2L, 3L, 1L, 1L, 3L, 1L, 2L), .Label = c("AUS", "JAPAN", "USA"), class = "factor"),GENDER = structure(c(2L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L), .Label = c("F", "M"), class = "factor"), CHILDREN = c(2L, 1L, 3L, 2L, 2L, 3L, 1L, 0L, 1L, 0L, 1L, 2L, 2L, 1L, 1L, 1L, 0L, 2L, 1L, 2L, 4L, 2L, 5L, 1L), SPEND = c(8500L, 23000L, 4000L, 9800L, 2200L, 4800L, 12300L, 8000L, 7100L, 10000L, 7800L, 7100L, 7900L, 7000L, 14200L, 11000L, 7900L, 2300L, 7000L, 8800L, 7500L, 15300L, 8000L, 7900L)), .Names = c("COUNTRY", "GENDER", "CHILDREN", "SPEND"), class = "data.frame", row.names = c(NA, -24L))
attach(T)

# Calcular la correlación (nos indicaría si los turistas con mayor número de niños, gastan más)
R <- cor(CHILDREN, SPEND)
# Redondeo
round(R, 2)
# Porcentaje de varianza compartida
100 * (R**2)

# Para probar que el coeficiente de correlación difiera de 0, se usando
cor.test(CHILDREN, SPEND)

# R no reconoce CHILDREN como factor
levels(CHILDREN)
# Entonces se transforma
levels(as.factor(CHILDREN))

# entonces hay ser una variable discreta con pocos valores, entonces se utiliza mejor la correlación de Spearman
cor(CHILDREN, SPEND, method ="spearman")

# Grafica
plot(CHILDREN, SPEND)
plot(CHILDREN, SPEND, pch = 16, col = "red", main = "TOURIST SPEND VS. NUMBERS OF CHILDREN")
plot(CHILDREN, SPEND, pch = 17, cex = 1.3, col = "blue", main = "MONEY SPENT ON VACATION ($) \nVS. NUMBERS OF CHILDREN", xlim = c(0, 6), ylim = c(0, 25000), xlab = "Numbers of Children" , ylab = "Money Spent ($)" )


####################################

# Crear un vector en un intervalo de -6 a 6, con longitud total de 71
x <- seq(-6, 6, len = 71)
# Graficar un coseno con una gráfica continua (type="l") y quitando el eje x (xaxt="n")
plot(x, cos(x),type="l",xaxt="n", xlab=expression(paste("Angle ",theta)), ylab=expression("sin "*beta))
# y se añade al eje x ciertas expresiones
axis(1, at = c(-2*pi, -1.5*pi, -pi, -pi/2, 0, pi/2, pi, 1.5*pi, 2*pi), lab = expression(-2*phi, -1.5*phi, -phi, -phi/2, 0, phi/2, phi, 2*phi, 1.5*phi))
# Colocar una etiqueta de texto en el punto 
text(-0.7*pi,0.5,substitute(chi^2=="23.5"))
text(0.1*pi, -0.5, expression(paste(frac(alpha*omega, sigma*phi*sqrt(2*pi)), " ", e^{frac(-(5*x+2*mu)^3, 5*sigma^3)})))
text(0.3*pi,0,expression(hat(z) %+-% frac(se, alpha)))

####################################

library(ggplot2)
# El formato de qplot es el siguiente:
# qplot(x = X, y = X, data = X, color = X, shape = X, geom = X, main = "Title")


