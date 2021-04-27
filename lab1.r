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

# Uno representa cantidad de exucdados y microneurismos encontrados.
valoresCantidad = c(3:16)
# El otro alude a la distancia eucladiana y el diametro del disco.
valoresDistancia = c(17,18)
df.normalizado <- df
df.normalizado[, c(valoresCantidad, valoresDistancia)] = scale(df[, c(valoresCantidad, valoresDistancia)])
df.normalizado$class = as.factor(df.normalizado$class)

########### Determinar datos con baja calidad.
#sapply(df, function(x) sum(is.na(x)))
sum(df$q==0)


########### Eliminacion de valores de baja calidad
#Identificar valores q == 0, y eliminarlos.
#bool.values <- df$q==0
#df <- df[!bool.values,]
#Para confirmar si se eliminaron.
#sum(df$q==0)


########### Se convierten en factor las variables binarias.
#df$q <- factor(df$q)
#df$ps <- factor(df$ps)
#df$amfm <- factor(df$amfm)
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


############ Grafico de torta
## Create a frequency table
df.table <- table(df$class)
colors <- terrain.colors(2)

# Create a pie chart 
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

############ Matriz de covarianza
#install.packages('ggplot2')
#install.packages("GGally")
library(GGally)
library(ggplot2)
ggcorr(df) + 
    labs(title="Feature covariance matrix")


nrow(df)


############ Grafico de correlacion
#install.packages('corrplot')
library(corrplot)

p.mat <- cor.mtest(df, conf.level = .95)
M <- cor(df)

corrplot(M, type = "upper", order = "hclust", 
         p.mat = p.mat$p, sig.level = 0.01)

#Para el informe
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = p.mat$p, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)


boxplot.width =  ggboxplot(data = df, x = "class", y = "width", color = "class") + border()


#GRAFICOS DE DISTRIBUCIOB

ggplot(df.normalizado, aes(x = nex.a, y = nma.a, color=class))+ facet_wrap(~amfm)+
  geom_point()

ggplot(df.normalizado, aes(x = nex.b, y = nma.b, color=class))+ facet_wrap(~amfm)+
  geom_point()

ggplot(df.normalizado, aes(x = nex.c, y = nma.c, color=class))+ facet_wrap(~amfm)+
  geom_point()

ggplot(df.normalizado, aes(x = nex.d, y = nma.d, color=class))+ facet_wrap(~amfm)+
  geom_point()

ggplot(df.normalizado, aes(x = nex.e, y = nma.e, color=class))+ facet_wrap(~amfm)+
  geom_point()

ggplot(df.normalizado, aes(x = nex.f, y = nma.f, color=class))+ facet_wrap(~amfm)+
  geom_point()

sp <- ggplot(df.normalizado, aes(x=nma.a, y=nex.a)) +
  geom_point()
  sp + geom_density_2d()
  # Gradient color
  sp + stat_density_2d(aes(fill = ..level..), geom="polygon")
  # Change the gradient color
  sp + stat_density_2d(aes(fill = ..level..), geom="polygon")+
    scale_fill_gradient(low="blue", high="red")

#No se ve ninguna relacion a simple vista; Es necesario ustilizar una agrupacion por modelo

class <- df$class

#Modelo con alpha 0.5
data.sinclass.05 <- datos.05[,-20]
BIC.05 <- mclustBIC(data.sinclass.05)
summary(BIC.05)
plot(BIC.05)
# BIC = 2504.353

#Modelo con alpha 0.6
data.sinclass.06 <- datos.06[,-20]
BIC.06 <- mclustBIC(data.sinclass.06)
summary(BIC.06)
plot(BIC.06)
# BIC = 4926.973

#Modelo con alpha 0.7
data.sinclass.07 <- datos.07[,-20]
BIC.07 <- mclustBIC(data.sinclass.07)
summary(BIC.07)
plot(BIC.07)
# BIC = 6630.694 

#Modelo con alpha 0.8
data.sinclass.08 <- datos.08[,-20]
BIC.08 <- mclustBIC(data.sinclass.08)
summary(BIC.08)
plot(BIC.08)
# BIC = -5858.015

#Modelo con alpha 0.9
data.sinclass.09 <- datos.09[,-20]
BIC.09 <- mclustBIC(data.sinclass.09)
summary(BIC.09)
plot(BIC.09)
# BIC = -4651.728

#Modelo con alpha 1.0
data.sinclass.10 <- datos.10[,-20]
BIC.10 <- mclustBIC(data.sinclass.10)
summary(BIC.10)
plot(BIC.10)
# BIC = -2108.609


#Tomando en consideracion los BIC obtenidos, se estima que la mejor opcione es 
#alpha = 0.7, ya ahi el BIC alcanza su maximo valor.

clPairs(data.sinclass.07, class)

#Se procede a generar el modelo utilizando el set de tatos que maximiza el BIC.

modelo.07 = Mclust(data.sinclass.07, x = BIC.07)
summary(modelo.07)

#install.packages("factoextra")
library(factoextra)

#Grafico de la distribucion de los datos dentro de los clusters
fviz_mclust(modelo.07, what = "classification", geom = "point",
            pallete = "jco")

#Ver como se estan agrupando los datos en relacion a la clasificacion anterior que se le dio a los grupos.
plot(modelo.07, what = "classification")

#Con esta tabla se conforman los valores de Sensibilidad y especificidad.
table(class, modelo.07$classification)
