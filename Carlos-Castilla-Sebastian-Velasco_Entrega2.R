#install.packages('ggplot2')
#install.packages('ggthemes')
#install.packages('corrplot')
#install.packages('randomForest')


library('ggplot2')
library('ggthemes')
library('randomForest')
library('corrplot')

# Antes de empezar con cualquier parte del trabajo seteo un seed.
set.seed(420)

theme_set(theme_tufte())  # from ggthemes

d1<-read.table("student-mat.csv",sep=",",header=TRUE)
d2<-read.table("student-por.csv",sep=",",header=TRUE)

#Hacemos la uni?n de los datasets de los estudiantes de la clase de portuges y matem?ticas
d3 <- rbind(d1, d2)

#Seg?n el archivo de Kaggle, 382 estudiantes se repiten en los dos datasets. En tanto debemos detectarlos y solo 
#dejar una instancia. El archivo de la plataforma nos provees las columnas que nos permitir?n hacer una clara 
#identificaci?n a falta de un ID por estudiante
columnas.identificadoras <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

#Ahora, haciendo uso de la funcion rownames(), que nos permite obtener strings con los nombre de las columnas, y 
# la funci?n unique(), que nos permite identificar los ?ndices de las columnas que no est?n repetidas, podremos saber con exactitud
#cuales elemntos no se repiten. Ahora, filas.unicas contendr? esta informaci?n.
filas.unicas <- rownames(unique(d3[,as.character(columnas.identificadoras)]))

#ahora, creamos una variable que contiene estas las filas que no est?n repetidas. 
#adem?s, excluiremos las columnas G1,G2 y G3 ya que no se pueden considerar como causas sino efectos de los dem?s clasificadores
d <-d3[filas.unicas, 1:30]

#comprobaciones de que el dataframe solamente tiene 650 elementos. 
nrow(d)


#comprobaciones de que el dataframe solamente tiene en cuenta 30 columnas y que dentro de sus nombres no se incluyen G1,G2 o G3. 
ncol(d)
colnames(d)

#Ahora bien, nuestras variables objetivo ser?n Dalc y Wacl. Estas, nos permiten determinar el nivel de consumo de alcohol
# en d?a de semana (Dalc) y en fines de semana (Walc) en una escala del 1 al 5. Sin embargo, para prop?sitos de lograr 
#resultados satisfactorios con algunos algoritmos como el arbol de decisi?n se ha convertido en una variable binaria. 

#Dentro del dataset d, se crear? una columna llamada DalcBina donde se guardar? el valor binario determinado por un condicional 
#que evalua si el valor es mayor o menor que 3. En caso de ser mayor a 3 se guardar? un valor de "yes", de lo contrario "no"
d$DalcBina <- ifelse(d$Dalc>3, "yes", "no")

#Dentro del dataset d, se crear? una columna llamada WalcBina donde se guardar? el valor binario determinado por un condicional 
#que evalua si el valor es mayor o menor que 3. En caso de ser mayor a 3 se guardar? un valor de "yes", de lo contrario "no"
d$WalcBina <- ifelse(d$Walc>3, "yes", "no")

# Tambi?n, se usar? una ecuaci?n que nos permite encontrar cu?l ser?a el consumo de alcohol en la totalidad que dar?a sumar Walc y Dalc. Se
# le denomina Alc y consiste en multiplicar los d?as que comprender?a Walc y Dalc de la semana sobre el total de estos. Determinando as? un valor
#que es un espectro m?s amplio de ?mbos. Se utiliza la funci?n round() con par?metro digits=0 para obtener numeros redondeados del 1-5 de nuevo.
d$Alc <- round((d$Walc*2+d$Dalc*5)/7,digits = 0)

#Hago un summary para saber los quartiles, media y mediana de la variable Alc
summary(d$Alc)

# empiezo con el modelo (random forest)

# Note from where we took the info: we force the model to predict our classification by temporarily changing 
# our target variable to a factor with only two levels using as.factor(). The importance=TRUE argument allows 
# us to inspect variable importance as we'll see, and the ntree argument specifies how many trees we want 
# to grow.

randomF <- randomForest(as.factor(Alc) ~ school + sex + age + address + famsize + Pstatus +
                      Medu + Fedu + Mjob + Fjob + reason + nursery + internet, data=d, importance=TRUE, ntree=5500)


#para ver las variables mas importantes (Entre mayor sea el valor mas importante es dicha variable)
varImpPlot(randomF)

# Aqui pondiramos nuestro dataset de prueba para determinar si personas son propensas al consumo de alcohol o no
#Prediction <- predict(fit, test)
#submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
#write.csv(submit, file = "areYouAnAlcoholic", row.names = FALSE)

#cosas pa corelation matix tuto aqui 
#http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
#cormat <- cor(d)
#corrplot(comrat)
