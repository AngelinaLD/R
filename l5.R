library(tidyr)
library(tseries)
library(tidyverse)
library(ggplot2)
library(psych)
library(car)
library(dplyr)
library(magrittr)
library(knitr)
library(HSAUR)
library(readr)
library(corrplot)
library(factoextra)
library(cluster)

#Первичные данные
mydata <- read_delim("lab3.csv", delim = ";", 
                     escape_double = FALSE, trim_ws = TRUE)
mydata

#Описательная статистика
describe(mydata)

#Корреляционная матрица переменных
mydata <- mydata[complete.cases(mydata),]
cor.data <- round(cor(mydata), digits = 4)
cor.data

corrplot(cor.data)

#Стандартизация данных 
nor <- mydata %>% mutate_all (~( scale (.) %>% as.vector ))
nor

#Удаление пропусков
nor <- nor[complete.cases(nor),]
nor <- nor[,1:5]
nor

#Корреляционная матрица переменных
cor.nor <- round(cor(nor), digits = 2)
cor.nor

#Визуализация матрицы корреляции
corrplot(cor.nor)

#Анализ главных компонент
pca.nor <- prcomp(nor, center = TRUE, scale = TRUE)

pca.nor
summary(pca.nor)

plot(pca.nor, type = "l")

exp.percent.nor <- summary(pca.nor)$importance[2, ]
exp.percent.nor

qplot(y = exp.percent.nor, x = names(exp.percent.nor)) + 
  geom_bar(stat = 'identity') +
  labs(x = "Главные компоненты",
       y = "Доли диспесии",
       title = "Доли дисперсии, объясняемые главными компонентами")

biplot(pca.nor, scale = 0 )

#Так как сумма первых 4-х компонент равняется 85% выбираем 4 компоненты из 6

#Собственные векторы
cor.nor

result <- eigen(cor.nor)
result
round(result$vectors, digits=2)

#Пересчёт значений, получение новой таблицы

nor$y1 <- nor$AGE*result$vectors[1,1]+nor$EDU*result$vectors[2,1]+
  nor$PM*result$vectors[3,1]+nor$NHH*result$vectors[4,1]+
  nor$DD*result$vectors[5,1]

nor$y2 <- nor$AGE*result$vectors[1,2]+nor$EDU*result$vectors[2,2]+
  nor$PM*result$vectors[3,2]+nor$NHH*result$vectors[4,2]+
  nor$DD*result$vectors[5,2]

nor$y3 <- nor$AGE*result$vectors[1,3]+nor$EDU*result$vectors[2,3]+
  nor$PM*result$vectors[3,3]+nor$NHH*result$vectors[4,3]+
  nor$DD*result$vectors[5,3]

nor$y4 <- nor$AGE*result$vectors[1,4]+nor$EDU*result$vectors[2,4]+
  nor$PM*result$vectors[3,4]+nor$NHH*result$vectors[4,4]+
  nor$DD*result$vectors[5,4]

nor.2 <- nor[,6:9]
round(nor.2, digits = 4)

#Оптимальное количество классов
fviz_nbclust(nor.2, kmeans, method = "wss") 

#Кластерный анализ методом k-средних
set.seed(1)
k2 <- kmeans(nor.2, centers = 3, nstart = 25)
k2
str(k2)

#Средниe по переменным для каждого кластера 
table_avg <- k2$centers
table_avg

round(aggregate(nor.2, by= list(cluster=k2$cluster),mean),2)
round(aggregate(mydata, by= list(cluster=k2$cluster),mean),2)

#График
fviz_cluster(k2, data = nor.2)

#Дисперсионный анализ
nor.2.2 <- nor.2
nor.2.2$class <- k2$cluster
nor.2.2

va <- aov(class ~ y1 + y2 + y3 + y4, data = nor.2.2)
summary(va)

#Множественная регрессия
lw.nor <- mydata %>% mutate_all (~( scale (.) %>% as.vector ))
lw.nor <- lw.nor[complete.cases(lw.nor),]
lw.nor <- lw.nor[,6]

nor.2$lw <- lw.nor
nor.2

model <- lm(lw$LW ~ y1 + y2 + y3 + y4, data = nor.2)
summary(model)


