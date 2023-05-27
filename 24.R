library(tidyr)
library(tseries)
library(tidyverse)
library(dplyr)

df_1 <- read.csv("heart.csv")
df_1$sex <- as.factor(df_1$sex)
df_1$cp <- as.factor(df_1$cp)
df_1$fbs <- as.factor(df_1$fbs)
df_1$restecg <- as.factor(df_1$restecg)
df_1$exng <- as.factor(df_1$exng)
df_1$slp <- as.factor(df_1$slp)
df_1$caa <- as.factor(df_1$caa)
df_1$thall <- as.factor(df_1$thall)
df_1$output <- as.factor(df_1$output)

x <- summary(df_1)
x
df_1_1 <-nrow(df_1)

df_1$sex <- as.factor(df_1$sex)
levels(df_1$sex) <- c("Женский","Мужской")

df_1$sex <- factor(df_1$sex , labels = c("Женский", "Мужской"))
gender <- table(df_1$sex)
gender

df_2 <- filter(df_1, exng  == "1", trtbps > "140", chol > "200", 
               restecg == "1" | restecg == "2", output == "1")

df_3 <- select(df_1, age, sex, exng) %>% filter(exng == "1")
df_3_0 <- table(df_3$sex)
df_3_0 <- as.vector(df_3_0)
names_1 <- c("Женский","Мужской")
piepercent <- round(100*df_3_0/df_1_1, 1)
par(mar = c(2, 2, 2, 2)) 
names_2 <- paste(names_1, " (", piepercent, "%)", sep = "")
colors = c("red","blue")
pie(df_3_0, names_2,main = "Доля пациентов со стенокардией", col = colors)

df_3_1 <- table(df_3$age)
palet = colorRampPalette(c("black","white"))
colors = palet(50)
par(mar = c(5,2,2,1), las = 1)
barplot(df_3_1,
        main = "Количество пациентов со стенокардией",
        xlab = "Возраст",
        las = 1,
        col = colors)

df_4 <- select(df_1, age, sex, trtbps) %>% filter(trtbps > "140")
df_4_0 <- table(df_4$sex)
df_4_0 <- as.vector(df_4_0)
names_1 <- c("Женский","Мужской")
piepercent <- round(100*df_4_0/df_1_1, 1)
par(mar = c(2, 2, 2, 2)) 
names_2 <- paste(names_1, " (", piepercent, "%)", sep = "")
colors = c("red","blue")
pie(df_3_0, names_2,main = "Доля пациентов с повышенным давлением", col = colors)

df_4_1 <- table(df_4$age)
palet = colorRampPalette(c("black","red"))
colors = palet(50)
par(mar = c(5,6,2,2), las = 1)
barplot(df_4_1,
        main = "Количество пациентов с повышенным давлением",
        xlab = "Возраст",
        las = 1,
        col = colors)

df_5 <- select(df_1, age, sex, chol) %>% filter(chol > "200")
df_5_0 <- table(df_5$sex)
df_5_0 <- as.vector(df_5_0)
names_1 <- c("Женский","Мужской")
piepercent <- round(100*df_5_0/df_1_1, 1)
par(mar = c(2, 2, 2, 2)) 
names_2 <- paste(names_1, " (", piepercent, "%)", sep = "")
colors = c("red","blue")
pie(df_5_0, names_2,main = "Доля пациентов с повышенным холестирином", 
    col = colors)

df_5_1 <- table(df_5$age)
names_1 <- df_5$age
piepercent <- round(100*df_5_0/df_1_1, 1)
par(mar = c(2, 2, 2, 2)) 
names_2 <- paste(names_1, " (", piepercent, "%)", sep = "")
palet = colorRampPalette(c("white","red"))
colors = palet(50)
barplot(df_5_1,
        main = "Количество пациентов с повышенным холестирином",
        xlab = "Возраст",
        ylim = c(0,20),
        las = 1,
        col = colors)

df_6 <- select(df_1, age, sex, restecg) %>% 
  filter(restecg == "1" | restecg == "2")
df_6_0 <- table(df_6$sex)
df_6_0 <- as.vector(df_6_0)
names_1 <- c("Женский","Мужской")
piepercent <- round(100*df_6_0/df_1_1, 1)
par(mar = c(2, 2, 2, 2)) 
names_2 <- paste(names_1, " (", piepercent, "%)", sep = "")
colors = c("red","blue")
pie(df_6_0, names_2,main = "Доля пациентов с подъемом сегмента ST", col = colors)

df_6_1 <- table(df_6$age)
names_1 <- df_6$age
piepercent <- round(100*df_6_0/df_1_1, 1)
par(mar = c(2, 2, 2, 2)) 
names_2 <- paste(names_1, " (", piepercent, "%)", sep = "")
palet = colorRampPalette(c("white","darkblue"))
colors = palet(50)
barplot(df_6_1,
        main = "Количество пациентов с подъемом сегмента ST",
        xlab = "Возраст",
        ylim = c(0,12),
        las = 1,
        col = colors)

df_7 <- select(df_1, age, sex, output) %>% filter(output == "1")
df_7_0 <- table(df_7$sex)
df_7_0 <- as.vector(df_7_0)
names_1 <- c("Женский","Мужской")
piepercent <- round(100*df_7_0/df_1_1, 1)
par(mar = c(2, 2, 2, 2)) 
names_2 <- paste(names_1, " (", piepercent, "%)", sep = "")
colors = c("red","blue")
pie(df_7_0, names_2,main = "Доля пациентов, у которых веротно будет ИМ", 
    col = colors)

df_7_1 <- table(df_7$age)
names_1 <- df_7$age
piepercent <- round(100*df_7_0/df_1_1, 1)
par(mar = c(2, 2, 2, 2)) 
names_2 <- paste(names_1, " (", piepercent, "%)", sep = "")
palet = colorRampPalette(c("white","darkgreen"))
colors = palet(50)
barplot(df_7_1,
        main = "Кол-во пациентов, у которых веротно будет ИМ",
        xlab = "Возраст",
        ylim = c(0,12),
        las = 1,
        col = colors)
