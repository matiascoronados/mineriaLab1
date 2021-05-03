url.data.set <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/00329/messidor_features.arff'
data.raw <- read.csv(url.data.set, header=FALSE, comment.char = "@")

# Los datos representan los resultados obtenidos a partir del analisis de diferentes imagenes de 
# retinas con la enfermedad Retinopatía diabética (RN) presente.
# - 40% de la poblacion diabetica tiende a sufrir esta enfermedad, con un 5% de probabilidad de que
# el paciente sufra cegera permantente
# - Se hace uso de herramientas  y tecnicas de clasficiacion.
# - La base de datos presenta 1150 observaciones

# Origen de los datos: Fondo de retina/ Foto.

df <- data.frame(data.raw)
colnames(df) <- c(
    "q",      #  0 Numero binario, relacionado con la calidad de la imagen. 0 = Mala calidad; 1 = Buena calidad.(remover)
    "ps",     #  1 Numero binario resultante de un pre-escaneo, que indica si la imagen presenta animalias graves en la retina.(remover)
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
    "amfm",   # 18 Numeri binario, relacionado con la clasificacion AM/FM-based.(remover) (indirectamente es la clase)
    "class"   # 19 Clase del dato, en donde 1 = contiene signos de DR, 0 = no contiene signos de DR.(remover)
)



df_test <- 


########### Se convierten en factor el valor binario de class.
df$class <- factor(df$class)

########### Se pasan a numericos los valores q; ps; amfm
df$q <- as.numeric(df$q)
df$ps <- as.numeric(df$ps)
df$amfm <- as.numeric(df$amfm)
df$class <- as.numeric(df$class)


#Se realizara un analisis tomando en consideracion las duplas que contengan el mismo valor de alpha.
datos.05 <- df[,-c(1,2,4,5,6,7,8,10,11,12,13,14,15,16,19,20)]

datos.06 <- df[,-c(1,2,3,5,6,7,8,9,11,12,13,14,15,16,19,20)]

datos.07 <- df[,-c(1,2,3,4,6,7,8,9,10,12,13,14,15,16,19,20)]

datos.08 <- df[,-c(1,2,3,4,5,7,8,9,10,11,13,14,15,16,19,20)]

datos.09 <- df[,-c(1,2,3,4,5,6,8,9,10,11,12,14,15,16,19,20)]

datos.10 <- df[,-c(1,2,3,4,5,6,7,9,10,11,12,13,15,16,19,20)]

df.final <- df[,-c(1,19,20)]



# Se relizo un escalado o normaliado de los datos para mejorar su representacion grafica, con tal facilitar su entendimiento.
# Uno representa cantidad de exucdados y microneurismos encontrados.
valoresCantidad = c(2:15)
# El otro alude a la distancia eucladiana y el diametro del disco.
valoresDistancia = c(16,17)

#Este escalado se relaizao en separado dada la naturaleza de las variables
df.normalizado <- df.final
df.normalizado[, c(valoresCantidad, valoresDistancia)] = scale(df.final[, c(valoresCantidad, valoresDistancia)])



############ Medidas de centralizacion 
# Media ; Mediana; 1Q ; 3Q; Max; Min
summary(df)


#Comportamientos de todos los nma son similares con direntes rangos de confianza
#a medida que aumenta el alpha se ve una reduccion en el valor de los datos
#La distribucion de los datos se mostrara mas adelante.
#No hay mucha informacion de los valores binarios.

############ Grafico de torta
# Solo usarlo para contexcualizar el problemna
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

#Se puede apreciar que el 46,9% de los datos presentan la enfermedad RN,
#mientras que el restante 53.1% no presenta dicha enefermedad.
#Ademas, se puede ver que se tienen una proporcion relativamente equitativa
#con una diferencia del 6,2% entre las clasificaciones.


############ Grafico de cajas
#install.packages('reshape')
#install.packages('ggplot2')
library(reshape)
library(ggplot2)

df.normalizado.b <- df.normalizado[,-1]

long = melt(df.normalizado.b[,c(1:ncol(df.normalizado.b))])

ggplot(long) + 
    geom_boxplot(aes(variable, value)) + 
    coord_flip() +
    labs(title="Unimodal feature distribution", x='Feature', y='Scaled value')

#Se puede ver que todos los valores nex (cantidad de exudados) tienen a tener una gran cantidad de 
# datos sesgados y una considerable cantidad de valores atipicos en la parte derecha de la caja.
#Esto nos da un indicio sobre el margen del nivel de confianza que es recomendable utilziar.
# Se va reduciendo la caja a medida que aumenta el intervalo de confianza.



############ Grafico de correlacion
#Se explica que hay una gran correlacion.

#install.packages('corrplot')
library(corrplot)

df.final.sinbinarios <- df.final[,-1]

p.mat <- cor.mtest(df.final.sinbinarios, conf.level = .95)
M <- cor(df.final.sinbinarios)

corrplot(M, type = "upper", order = "hclust", 
         p.mat = p.mat$p, sig.level = 0.01)

# Se puede ver una gran correlacion entre las variables que representan los distintos nivel de confianza
# de nma y nex. Se se pueden apreciar las coreelaciones que se desctan por no alcanzar un nivel 
# de significancia minimo.



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





  
#Dadas las distribuciones mostradas anteriormente y las correlaciones existentes entre los distintos niveles de ocnfianza de las
#variables nma y nex, se decide utilizar ambas variables con un nivel de confianza de 0.7.
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

# De forma preliminar podemos decir que existe una correlacion entre las varialbes que indican 
# distancias, tal como dd y dm. Tambien se puede ver una fuerte correlacion negativa entre el valor 
# de amfm y nma.c cabe destacar que las correlaciones de la clase no la consideramos, ya que no 
# tienene informacion relevante para el modelo.


#Graficos de distribuones de los par de valores de todas las correlaciones.
p1 <- ggplot(datos.07, aes(x=dd, y=nex.c)) +
  geom_point() + geom_density_2d()

p1 <- ggplot(datos.07, aes(x=dd, y=nma.c)) +
  geom_point() + geom_density_2d()

p2 <- ggplot(datos.07, aes(x=dd, y=dm)) +
  geom_point() + geom_density_2d()

p3 <- ggplot(datos.07, aes(x=nex.c, y=nma.c)) +
  geom_point() + geom_density_2d()

p4 <- ggplot(datos.07, aes(x=nex.c, y=dm)) +
  geom_point() + geom_density_2d()

p5 <- ggplot(datos.07, aes(x=nma.c, y=dm)) +
  geom_point() + geom_density_2d()

grid.arrange(p1,p2,p3,p4,p5)


#No existe una correlacion lineal entre los datos, por lo mismo, se estima que conveniente utilizar el agrupamiento por modelos
class <- df$class

#Ya que previamente se noto que para los niveles de confianza grandes, existen muchos datos atipicos, y con muy
#pequeños el gradod de distribucion es muy amplio.
#Se procede a analizar los BIC obtenidos con difernetes grados de alpha, con el fin de analizar
#cual prevee de un mejor grado de agrupamiento.
library(mclust)


#BIC con alpha 0.5
datos.05$ps <- df$ps
BIC.05 <- mclustBIC(datos.05)
summary(BIC.05)
plot(BIC.05)
# BIC = -12350.75

#BIC  con alpha 0.6
datos.06$ps <- df$ps
BIC.06 <- mclustBIC(datos.06)
summary(BIC.06)
plot(BIC.06)
# BIC = -10321.87

#BIC  con alpha 0.7
datos.07$ps <- df$ps
BIC.07 <- mclustBIC(datos.07)
summary(BIC.07)
plot(BIC.07)
# BIC = -8761.879 

#BIC  con alpha 0.8
datos.08$ps <- df$ps
BIC.08 <- mclustBIC(datos.08)
summary(BIC.08)
plot(BIC.08)
# BIC = -6090.932

#BIC  con alpha 0.9
datos.09$ps <- df$ps
BIC.09 <- mclustBIC(datos.09)
summary(BIC.09)
plot(BIC.09)
# BIC = -4869.198

#BIC  con alpha 1.0
datos.10$ps <- df$ps
BIC.10 <- mclustBIC(datos.10)
summary(BIC.10)
plot(BIC.10)
# BIC = -2312.941

#Antes y despues de agregar la variable binria
#Antes y despues de agregar la variable binria
#Antes y despues de agregar la variable binria

#BIC con alpha 0.5
datos.05 <- datos.05[,-5]
BIC.05 <- mclustBIC(datos.05)
summary(BIC.05)
plot(BIC.05)
# BIC = -11386.18

#BIC  con alpha 0.6
datos.06 <- datos.06[,-5]
BIC.06 <- mclustBIC(datos.06)
summary(BIC.06)
plot(BIC.06)
# BIC = -9118.87

#BIC  con alpha 0.7
datos.07 <- datos.07[,-5]
BIC.07 <- mclustBIC(datos.07)
summary(BIC.07)
plot(BIC.07)
# BIC = -6740.192

#BIC  con alpha 0.8
datos.08 <- datos.08[,-5]
BIC.08 <- mclustBIC(datos.08)
summary(BIC.08)
plot(BIC.08)
# BIC = -2764.229

#BIC  con alpha 0.9
datos.09 <- datos.09[,-5]
BIC.09 <- mclustBIC(datos.09)
summary(BIC.09)
plot(BIC.09)
# BIC = 2637.225

#BIC  con alpha 1.0
datos.10 <- datos.10[,-5]
BIC.10 <- mclustBIC(datos.10)
summary(BIC.10)
plot(BIC.10)
# BIC = 5825.522


#La categorica se elimina.
# Alpha = 1.0


#A juzgar por los valores anteriores, se aprecia que el valor que maximiza al BIC
#corresponde a los datos que presentan un alpha = 1.0
#Por esto mismo, se decidio utilizar este set de datos para conformar el modelo propuesto

library(dplyr)

df_test <- df[,-c(2,3,4,5,6,7,9,10,11,12,13,15,16,19,20)]
df_test.10 <- filter(df_test, q==1)
df_test.10.sinq <- df_test.10[,-1]


#Se porceden a probar las difernetes formas que puede tomar el modelo, con tal de encontrar el que mas se adecue
#al contexto de nuestro problema.
modelo1 = Mclust(datos.10 ,prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="VEV")
summary(modelo1)
# BIC = 2928.169

modelo2 = Mclust(datos.10 ,prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="EVV")
summary(modelo2)
# BIC = ERROR

modelo3 = Mclust(datos.10 ,prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="EVI")
summary(modelo3)
# Tira warning
# BIC = -7474.871

modelo4 = Mclust(datos.10 ,prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="EEV")
summary(modelo4)
# BIC = 22.63847

modelo5 = Mclust(datos.10 ,prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="VEE")
summary(modelo5)
# BIC = ERROR

#####################
# Sin control
#####################

modelo1 = Mclust(datos.10, modelNames ="VEV")
summary(modelo1)
#BIC = 5825.522

modelo2 = Mclust(datos.10 , modelNames ="EVV")
summary(modelo2)
#BIC = 5075.423

modelo3 = Mclust(datos.10 , modelNames ="EVI")
summary(modelo3)
#BIC = 5120.628

modelo4 = Mclust(datos.10 , modelNames ="EEV")
summary(modelo4)
#BIC = 137.7975

modelo5 = Mclust(datos.10 , modelNames ="VEE")
summary(modelo5)
#BIC = 3686.46


######### SOLO CON DATOS DE CALIDAD == 1

###########################best modelo###########################
modelo1 = Mclust(df_test.10.sinq, modelNames ="VEV")
summary(modelo1)
#BIC = 5941.877
######################################################

modelo2 = Mclust(df_test.10.sinq , modelNames ="EVV")
summary(modelo2)
#BIC = 4971.762

modelo3 = Mclust(df_test.10.sinq , modelNames ="EVI")
summary(modelo3)
#BIC = 5092.469

modelo4 = Mclust(df_test.10.sinq , modelNames ="EEV")
summary(modelo4)
#BIC = 54.43531

modelo5 = Mclust(df_test.10.sinq , modelNames ="VEE")
summary(modelo5)
#BIC = 3449.44

modelo6 = Mclust(df_test.10.sinq, G=1:20, modelNames ="VEV")
summary(modelo6)
#BIC = 6020.932



#install.packages("factoextra")
library(factoextra)

#Grafico de la distribucion de los datos dentro de los clusters
#fviz_mclust(modelo.07, what = "classification", geom = "point",
#            pallete = "jco")

#Mientrasn mas grande el punto; Mas le costo decidir la asignacion de grupo.
fviz_mclust(modelo1, what = "uncertainty", pallete = "jco")

#Con esta tabla se conforman los valores de Sensibilidad y especificidad.
table(class, modelo1$classification)




