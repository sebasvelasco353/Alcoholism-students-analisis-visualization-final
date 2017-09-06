
library('ggplot2')
library('ggthemes')
theme_set(theme_tufte())  # from ggthemes

d1<-read.table("student-mat.csv",sep=",",header=TRUE)
d2<-read.table("student-por.csv",sep=",",header=TRUE)

#bind the two datasets into one
d3 <- rbind(d1, d2)

#age vs  Dalc by sex
ggplot(d3, aes(x=age, y=Dalc, color=sex, alpha = 1/10)) + 
  geom_point() +
  scale_colour_hue(l=70) + 
  geom_jitter()


#histogram with family relations on x axis with MEAN
ggplot(d3, aes(x=famrel)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(famrel, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

#histogram with health on x axis with MEAN
ggplot(d3, aes(x=health)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(health, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

#Dalc vs health colored by sex
ggplot(d3, aes(x=Dalc, y=health, color=sex, alpha = 1/10)) + 
  geom_point() +
  scale_colour_hue(l=70) + 
  geom_jitter()

#Histogram on a categorical variable for regular day alcohol and fam support
ggplot(d3, aes(Dalc, fill = famsup))+ 
  geom_bar()

#Histogram on a categorical variable for regular day alcohol and guardian
ggplot(d3, aes(Dalc, fill = guardian))+ 
  geom_bar()

str(d3)