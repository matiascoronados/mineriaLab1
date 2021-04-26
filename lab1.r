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

numericFeats = c(3:16)
eyeFeats = c(17,18)
df.normalizado <- df
df.normalizado[, c(numericFeats, eyeFeats)] = scale(df[, c(numericFeats, eyeFeats)])
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

############ Dstribucion de dispersion
#install.packages('GGally')
ggplot(df.normalizado, aes(x = nma.a, y = nma.b, color=class))+ facet_wrap(~amfm)+
  geom_point()+
  geom_smooth(method=lm)

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

#No se ve ninguna relacion a simple vista; Es necesario ustilizar una agrupacion por modelo




########## Codigo del profe.
#corrplot(df, p.mat = res1$p, sig.level = .05)
require(mclust)


mod1 = Mclust(df[,1:19]) #DEFAULT 
summary(mod1)


mod3 = Mclust(df[,1:19], G = 10)  #Numero de grupos = 3.
summary(mod3)



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

# calculate collinearity
corMatMy <- cor(bc_data[,2:31])
corrplot(corMatMy, order = "hclust", tl.cex = 0.7)



