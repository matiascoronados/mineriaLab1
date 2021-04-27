#https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Prognostic)
url.data.set <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/00329/messidor_features.arff'
data.raw <- read.csv(url.data.set, header=FALSE, comment.char = "@")

#Los datos representan los resultados obtenidos a partir del analisis de diferentes imagenes de retinas, entre las cuales
#se encuentran casos activos de Retinopatía diabética (RN).

df <- data.frame(data.raw)
colnames(df) <- c(
    "q",      #  0 Numero binario, relacionado con la calidad de la imagen. 0 = Mala calidad; 1 = Buena calidad.
    "ps",     #  1 Numero binario resultante de un pre-escaneo, que indica si la imagen presenta animalias graves en la retina.
    "nma.a",  #  2 Numero de MAs (microneurismas) encontrados en un nivel de confianza con alpha = 0.5
    "nma.b",  #  3 Numero de MAs (microneurismas) encontrados en un nivel de confianza con alpha = 0.6
    "nma.c",  #  4 Numero de MAs (microneurismas) encontrados en un nivel de confianza con alpha = 0.7
    "nma.d",  #  5 Numero de MAs (microneurismas) encontrados en un nivel de confianza con alpha = 0.8
    "nma.e",  #  6 Numero de MAs (microneurismas) encontrados en un nivel de confianza con alpha = 0.9
    "nma.f",  #  7 Numero de MAs (microneurismas) encontrados en un nivel de confianza con alpha = 1.0
    "nex.a",  #  8 Numero de Exudates encontrados en un nivel de confianza con alpha = 0.5
    "nex.b",  #  9 Numero de Exudates encontrados en un nivel de confianza con alpha = 0.6
    "nex.c",  # 10 Numero de Exudates encontrados en un nivel de confianza con alpha = 0.7
    "nex.d",  # 11 Numero de Exudates encontrados en un nivel de confianza con alpha = 0.8
    "nex.e",  # 12 Numero de Exudates encontrados en un nivel de confianza con alpha = 0.9
    "nex.f",  # 13 Numero de Exudates encontrados en un nivel de confianza con alpha = 1.0
    "nex.g",  # 14 Numero de Exudates encontrados en un nivel de confianza con alpha = 1.0
    "nex.h",  # 15 Numero de Exudates encontrados en un nivel de confianza con alpha = 1.0
    "dd",     # 16 Distancia eucladiana entre el centro de la macula y el centro del disco optico
    "dm",     # 17 Diametro del disco optico
    "amfm",   # 18 Numeri binario, relacionado con la clasificacion AM/FM-based
    "class"   # 19 Clase del dato, en donde 1 = contiene signos de DR, 0 = no contiene signos de DR.
)

#Se realiza una estanadarizacion de las observacion respecto a las varialbes. Esto se hace por separado en base a las 
# dimenciones de las variables, para ser consecuente on los valores contemplados en las columnas.

# Se relizo un escalado o normaliado de los datos para mejorar su representacion grafica, con tal facilitar su entendimiento.
# Uno representa cantidad de exucdados y microneurismos encontrados.
valoresCantidad = c(3:16)
# El otro alude a la distancia eucladiana y el diametro del disco.
valoresDistancia = c(17,18)

#Este escalado se relaizao en separado dada la naturaleza de las variables

df.normalizado <- df
df.normalizado[, c(valoresCantidad, valoresDistancia)] = scale(df[, c(valoresCantidad, valoresDistancia)])
df.normalizado$class = as.factor(df.normalizado$class)

########### Determinar datos con baja calidad.
#sapply(df, function(x) sum(is.na(x)))
sum(df$q==0)

########### Se convierten en factor el valor binario de class.
df$class <- factor(df$class)

########### Se pasan a numericos los valores q; ps; amfm
df$q <- as.numeric(df$q)
df$ps <- as.numeric(df$ps)
df$amfm <- as.numeric(df$amfm)
df$class <- as.numeric(df$class)


#Se realizara un analisis tomando en consideracion las duplas que contengan el mismo valor de alpha.
datos.05 <- df[,-c(4,5,6,7,8,10,11,12,13,14,15,16)]

datos.06 <- df[,-c(3,5,6,7,8,9,11,12,13,14,15,16)]

datos.07 <- df[,-c(3,4,6,7,8,9,10,12,13,14,15,16)]

datos.08 <- df[,-c(3,4,5,7,8,9,10,11,13,14,15,16)]

datos.09 <- df[,-c(3,4,5,6,8,9,10,11,12,14,15,16)]

datos.10 <- df[,-c(3,4,5,6,7,9,10,11,12,13,15,16)]

############ Medidas de centralizacion 
# Media ; Mediana; 1Q ; 3Q; Max; Min
summary(df)


#Comportamientos de todos los nma son similares con direntes rangos de confianza
#a medida que aumenta el alpha se ve una reduccion en el valor de los datos
#La distribucion de los datos se mostrara mas adelante.


############ Grafico de torta
## Create a frequency table
df.table <- table(df$class)
colors <- terrain.colors(2)
class.prop.table <- prop.table(df.table)*100
class.prop.df <- as.data.frame(class.prop.table)
pielabels <- sprintf("%s - %3.1f%s", class.prop.df[,1], class.prop.table, "%")

pie(class.prop.table,
  labels=pielabels,  
  clockwise=TRUE,
  col=colors,
  border="gainsboro",
  radius=0.8,
  cex=0.8, 
  main="Frecuencia de la clase")

#Se puede apreciar que el 53.1% de los datos presentan la enfermedad RN,
#mientras que el restante 46,9% no presenta dicha enefermedad.
#Ademas, se puede ver que se tienen una proporcion relativamente equitativa
#con una diferencia del 6,2% entre las clasificaciones.


############ Grafico de cajas
#install.packages('reshape')
#install.packages('ggplot2')
library(reshape)
library(ggplot2)
long = melt(df.normalizado[,c(1:ncol(df.normalizado)-1)])

ggplot(long) + 
    geom_boxplot(aes(variable, value)) + 
    coord_flip() +
    labs(title="Unimodal feature distribution", x='Feature', y='Scaled value')

#Se puede ver que todos los valores nex tienen a tener una gran cantidad de datos sesgados y 
#una considerable cantidad de valores atipicos en la parte derecha de la caja
#Se puede apreciar que los exudados presentan los valores mas sesgados, sobretodo desde nex.d.
#Esto nos da un indicio sobre el margen del nivel de confianza que es recomendable utilziar.
# Se va reduciendo la caja a medida que aumenta el intervalo de confianza.



############ Grafico de correlacion
#Se explica que hay una gran correlacion.

#install.packages('corrplot')
library(corrplot)

p.mat <- cor.mtest(df, conf.level = .95)
M <- cor(df)

corrplot(M, type = "upper", order = "hclust", 
         p.mat = p.mat$p, sig.level = 0.01)

#se puede ver una gran correlacion entre las variables que representan los distintos nivel de confianza de nma y nex.
#se se pueden apreciar las coreelaciones que se desctan por no alcanzar un nivel de significancia minimo.


#GRAFICOS DE DISTRIBUCIOB

p1 <- ggplot(df.normalizado, aes(x=nma.a, y=nex.a)) +
  geom_point()+
  stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="blue", high="red")

p2 <- ggplot(df.normalizado, aes(x=nma.b, y=nex.b)) +
  geom_point()+
  stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="blue", high="red")

p3 <- ggplot(df.normalizado, aes(x=nma.c, y=nex.c)) +
  geom_point()+
  stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="blue", high="red")

p4 <- ggplot(df.normalizado, aes(x=nma.d, y=nex.d)) +
  geom_point()+
  stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="blue", high="red")

p5 <- ggplot(df.normalizado, aes(x=nma.e, y=nex.e)) +
  geom_point()+
  stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="blue", high="red")

p6 <- ggplot(df.normalizado, aes(x=nma.f, y=nex.f)) +
  geom_point()+
  stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="blue", high="red")

#La diferencia con la distribuiones de varialbes en distinos grados de condianza.

#Como se verian la matriz de correacion unicamente con el alpha 0.7 presente.

#install.packages("gridExtra")
library("gridExtra")

#Multigraficos de distribucion de los datos diferenciando a los alphas.
grid.arrange(p1, p2,p3,p4,p5,p6)


---



#Estos graficos nos permiten analizar como se encuentra la distribucion de las variables nex y nma
#para sus correspondientes niveles de confianza
#nex f; tiene los datos muy condensarods para los diferentes valores de nma
#tiene valores muchos mas acotados
#el otro tiene mapas de calor mas disperso, entonces el rango en que se encuentra la correlacion de las variables es mayor.
#Podemos ver que el grado de distribucion de los valores es muy disperso.

#Dadas las distribuciones mostradas anteriormente y las correlaciones existentes entre los distintos niveles de ocnfianza de las
#variables nma y nex, se decide utilizar ambas variables con un nivel de confianza de 0.7
#Ahora vamos a mostrar las correlaciones resultantes, producto de la eliminacion de las variables rebundantes.

#Matriz de correlacion para los datos con alpha = 0.7
p.mat <- cor.mtest(datos.07, conf.level = .95)
M <- cor(datos.07)

corrplot(M, type = "upper", order = "hclust", 
         p.mat = p.mat$p, sig.level = 0.01)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = p.mat$p, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)

# De forma preliminar podemos decir que existe una correlacion entre las varialbes que indican distancias, tal como dd y dm
# Tambien se puede ver una fuerte correlacion negativa entre el valor de amfm y nma.c
# cabe destacar que las correlaciones de la clase no la consideramos, ya que no tienene informacion relevante para el modelo.


#Graficos de distribuones de los par de valores con correlaciones mas signficativas.
p1 <- ggplot(datos.07, aes(x=dd, y=dm)) +
  geom_point() + geom_density_2d()

p2 <- ggplot(datos.07, aes(x=dd, y=nex.c)) +
  geom_point() + geom_density_2d()

p4 <- ggplot(datos.07, aes(x=nma.c, y=nex.c)) +
  geom_point() + geom_density_2d()

p5 <- ggplot(datos.07, aes(x=nma.c, y=amfm)) +
  geom_point() + geom_density_2d()

p6 <- ggplot(datos.07, aes(x=dm, y=nex.c)) +
  geom_point() + geom_density_2d()

p7 <- ggplot(datos.07, aes(x=dm, y=amfm)) +
  geom_point() + geom_density_2d()

p8 <- ggplot(datos.07, aes(x=nex.c, y=amfm)) +
  geom_point() + geom_density_2d()


grid.arrange(p1, p2,p4,p5,p6,p7,p8)


#Entre las variables binarias, se puede ver donde esta mas concentrado los datos (idnciado por el nivel de ondas3)
#por parte de las distancias
#y las variables nex y nma, nos indica que rangos estan concentrados los datos.


#No existe una correlacion lineal entre los datos, por lo mismo, se estima que conveniente utilizar el agrupamiento por modelos
class <- df$class

#Ya que previamente se noto que para los niveles de confianza grandes, existen muchos datos atipicos, y con muy
#pequeños el gradod de distribucion es muy amplio.
#Se procede a analizar los BIC obtenidos con difernetes grados de alpha, con el fin de analizar
#cual prevee de un mejor grado de agrupamiento.
library(mclust)

#BIC con alpha 0.5
data.sinclass.05 <- datos.05[,-20]
BIC.05 <- mclustBIC(data.sinclass.05)
summary(BIC.05)
plot(BIC.05)
# BIC = 2504.353

#BIC  con alpha 0.6
data.sinclass.06 <- datos.06[,-20]
BIC.06 <- mclustBIC(data.sinclass.06)
summary(BIC.06)
plot(BIC.06)
# BIC = 4926.973

#BIC  con alpha 0.7
data.sinclass.07 <- datos.07[,-20]
BIC.07 <- mclustBIC(data.sinclass.07)
summary(BIC.07)
plot(BIC.07)
# BIC = 6630.694 

#BIC  con alpha 0.8
data.sinclass.08 <- datos.08[,-20]
BIC.08 <- mclustBIC(data.sinclass.08)
summary(BIC.08)
plot(BIC.08)
# BIC = -5858.015

#BIC  con alpha 0.9
data.sinclass.09 <- datos.09[,-20]
BIC.09 <- mclustBIC(data.sinclass.09)
summary(BIC.09)
plot(BIC.09)
# BIC = -4651.728

#BIC  con alpha 1.0
data.sinclass.10 <- datos.10[,-20]
BIC.10 <- mclustBIC(data.sinclass.10)
summary(BIC.10)
plot(BIC.10)
# BIC = -2108.609

#A juzgar por los valores anteriores, se aprecia que el valor que maximiza al BIC
#corresponde a los datos que presentan un alpha = 0.7.
#Por esto mismo, se decidio utilizar este set de datos para conformar el modelo propuesto.

casos0 = sum(class == "0")
casos1 = sum(class == "1")

distribucion.clase = c(casos0,casos1)

#Se porceden a probar las difernetes formas que puede tomar el modelo, con tal de encontrar el que mas se adecue
#al contexto de nuestro problema.
modelo1 = Mclust(data.sinclass.07, G=2 ,prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="VEV")
summary(modelo1)
distribucion.clase

modelo2 = Mclust(data.sinclass.07, G=2 ,prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="EEV")
summary(modelo2)
distribucion.clase

modelo3 = Mclust(data.sinclass.07, G=2 ,prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="VEI")
summary(modelo3)
distribucion.clase

modelo4 = Mclust(data.sinclass.07 ,prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="EEI")
summary(modelo4)
distribucion.clase

modelo5 = Mclust(data.sinclass.07, G=2 ,prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="EEE")
summary(modelo5)
distribucion.clase


#Se procede a generar el modelo utilizando el set de tatos que maximiza el BIC.
modelo.07 = Mclust(data.sinclass.07, x = BIC.07)
summary(modelo.07)

#install.packages("factoextra")
library(factoextra)

#Grafico de la distribucion de los datos dentro de los clusters
#fviz_mclust(modelo.07, what = "classification", geom = "point",
#            pallete = "jco")

#Mientrasn mas grande el punto; Mas le costo decidir la asignacion de grupo.
fviz_mclust(modelo.07, what = "uncertainty", pallete = "jco")

#Con esta tabla se conforman los valores de Sensibilidad y especificidad.
table(class, modelo.07$classification)




