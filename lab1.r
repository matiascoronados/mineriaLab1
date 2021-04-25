#https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Prognostic)

url.data.set <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/00329/messidor_features.arff'
data.raw <- read.csv(url.data.set, header=FALSE, comment.char = "@")

df <- data.frame(data.raw)
colnames(df) <- c(
    "q",      #  0 The binary result of quality assessment. 0 = bad quality 1 = sufficient quality
    "ps",     #  1 The binary result of pre-screening, 1 indicates severe retinal abnormality and 0 its lack
    "nma.a",  #  2 Number of MAs found at the confidence levels alpha = 0.5
    "nma.b",  #  3 Number of MAs found at the confidence levels alpha = 0.6
    "nma.c",  #  4 Number of MAs found at the confidence levels alpha = 0.7
    "nma.d",  #  5 Number of MAs found at the confidence levels alpha = 0.8
    "nma.e",  #  6 Number of MAs found at the confidence levels alpha = 0.9
    "nma.f",  #  7 Number of MAs found at the confidence levels alpha = 1.0
    "nex.a",  #  8 Number of Exudates found at the confidence levels alpha = 0.5
    "nex.b",  #  9 Number of Exudates found at the confidence levels alpha = 0.6
    "nex.c",  # 10 Number of Exudates found at the confidence levels alpha = 0.7
    "nex.d",  # 11 Number of Exudates found at the confidence levels alpha = 0.8
    "nex.e",  # 12 Number of Exudates found at the confidence levels alpha = 0.9
    "nex.g",  # 13 Number of Exudates found at the confidence levels alpha = 1.0
    "nex.f",  # 14 Number of Exudates found at the confidence levels alpha = 1.0
    "nex.h",  # 15 Number of Exudates found at the confidence levels alpha = 1.0
    "dd",     # 16 The euclidean distance of the center of the macula and the center of the optic disc
    "dm",     # 17 The diameter of the optic disc
    "amfm",   # 18 The binary result of the AM/FM-based classification
    "class"   # 19 Class label. 1 = contains signs of DR, 0 = no signs of DR
)

#Eliminar datos con de baja calidad. (q = 0)
bool.values <- df$q==0
df <- df[!bool.values,]

#Para confirmar si se eliminaron.
sum(df$q==0)

# Media ; Mediana; 1Q ; 3Q; Max; Min
summary(df)

#Determinar missing values
#sapply(df, function(x) sum(is.na(x)))
sum(df$q==1)

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

install.packages('reshape')

library(reshape)

#Grafico de cajas
long = melt(df[,c(1:ncol(df)-1)])

ggplot(long) + 
    geom_boxplot(aes(variable, value)) + 
    coord_flip() +
    labs(title="Unimodal feature distribution", x='Feature', y='Scaled value')

#install.packages('ggplot2')
#install.packages("GGally")
library(GGally)
library(ggplot2)
ggcorr(df) + 
    labs(title="Feature covariance matrix")

#Distribucion dispersion
#install.packages('GGally')
library(GGally)

df$q <- factor(df$q)
df$ps <- factor(df$ps)
df$amfm <- factor(df$amfm)
df$class <- factor(df$class)

df$q <- as.numeric(df$q)
df$ps <- as.numeric(df$ps)
df$amfm <- as.numeric(df$amfm)
df$class <- as.numeric(df$class)

ggpairs(df, aes(colour=class, alpha=0.4))

require(mclust)
mod1 = Mclust(iris[,1:4]) #DEFAULT 
summary(mod1)

mod2 = Mclust(iris[,1:4], G = 3)  #Numero de grupos = 3.
summary(mod2, parameters = TRUE)

mod6 = Mclust(iris[,1:4], prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="EII")  
#"EII" = spherical, equal volume # Using prior #The function priorControl is used to specify a conjugate prior for EM within MCLUST. 
summary(mod6,parameter = TRUE)
plot(mod6, what = "classification")

class<-iris$Species

BIC<-mclustBIC(iris[,1:4], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
plot(BIC)  #se grafican los BIC por configuración de parámetros
summary(BIC)  # se presentan los mejores valores BIC

mod11=Mclust(iris[,1:4],x=BIC) # en base al mejor valor BIC se realiza el mclust
summary(mod11)#se muestra resultado y tabla de clustering


plot(mod11, what = "classification")  #se grafica la configuración de agrupamientos.
legend("bottomright", legend = 1:2,
       col = mclust.options("classPlotColors"),
       pch = mclust.options("classPlotSymbols"),title = "Class labels:")

table(class, mod11$classification) #distribución de clases por cada grupo.


class<-iris$Species
mod12 = Mclust(iris[,1:4], G=3, prior = priorControl(functionName="defaultPrior", shrinkage=0.1), modelNames ="VEV")  
table(class, mod12$classification) #distribución de clases por cada grupo.
summary(mod12)
plot(mod12, what = "classification")  #se grafica la configuración de agrupamientos.
legend("bottomright", legend = 1:3,
       col = mclust.options("classPlotColors"),
       pch = mclust.options("classPlotSymbols"),title = "Class labels:")