library('ggplot2')

d1<-read.table("student-mat.csv",sep=",",header=TRUE)
d2<-read.table("student-por.csv",sep=",",header=TRUE)

#Mezclo los dos datasets
d3 <- rbind(d1, d2)

ggplot(d3, aes(x=absences, y=sex)) + 
  geom_point(shape=1)

str(d3)