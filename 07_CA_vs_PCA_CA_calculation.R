# title: "Корреспондентный анализ и анализ главных компонент"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Вадим Хайтов, Марина Варфоломеева


## Пример: Птицы в лесах Австралии

# Обилие 102 видов птиц в 37 сайтах в юго-восточной Австралии (Mac Nally, 1989; данные из Quinn, Keough, 2002). Можно ли описать отношения между сайтами небольшим числом главных компонент?
library(readxl)
birds <- read_excel(path = "data/macnally.xlsx")
str(birds)


## Задание: Проведите анализ главных компонент
library(vegan)
birds_pca <-  (birds[,-c(1:2)], scale = )

biplot(  , display = )
screeplot(  , bstick  )
summary(  )

#Лечение эффекта подковы с помощью Трансформации данных
# расстояние Хеллингера (Hellinger distance)
birds_h <- decostand(birds[ , -c(1, 2)], "hellinger")

# хордальное расстояние (chord distance)
birds_ch <- decostand(birds[ , -c(1, 2)], "norm")

## Задание: Проведите анализ главных компонент по трансформированным данным
# Сравните долю дисперсии, объясненной первыми двумя компонентами с результатами анализа нетрансформированных данных.
# - В каком случае объясненная дисперсия больше?
# Сравните получившиеся ординации объектов.
# - Исчез ли "эффект подковы" после трансформации?
# - Изменилась ли группировка объектов?





# симуляция градиента
#Построение искусственно созданного градиента
library(ggplot2)
abundance <- function(k){
  d <- -(1:100)^2 + k*(1:100) + 500
  d[d<0] <- 0
  d
}


dat <- data.frame(Site = 1:100, Sp1 = abundance(5), Sp2 = abundance(50), Sp3 = abundance(100)/10, Sp4 = abundance(150)/10, Sp5 = abundance(200)/10, Sp6 = abundance(250)/10 )

dat2 <- dat[, -1] + rnorm(100, 0, 200)
dat2[dat2 < 0] <- 0
rownames(dat2) <-make.unique(as.character(dat$Site))

ggplot(dat2, aes(x = 1:100))+ geom_point(aes(y=Sp1), color = "black")  + geom_point(aes(y=Sp2), color = "green") +  geom_point(aes(y=Sp3), color = "red")  + geom_point(aes(y=Sp4), color = "blue") + geom_point(aes(y=Sp5), color = "darkblue") + geom_point(aes(y=Sp6), color = "pink") + geom_smooth(aes(y=Sp1), color = "black", se = F)  + geom_smooth(aes(y=Sp2), color = "green", se= F) +  geom_smooth(aes(y=Sp3), color = "red", se = F)  + geom_smooth(aes(y=Sp4), color = "blue", se = F) + geom_smooth(aes(y=Sp5), color = "darkblue", se = F) + geom_smooth(aes(y=Sp6), color = "pink", se = F) + labs(x = "Точки сбора", y = "Обилия видов")


round(dat2)

pca_dat <- rda(dat2)
plot(pca_dat, display = "sites", type = "p", scaling = 2, col = 1:100)

mds_dat <- metaMDS(dat2)

plot(mds_dat, display = "sites")





# Корреспондентный анализ

data(mite)

mite

## Задание: проведите анализ главных компонент по матрице ковариации

mite_pca <- rda(  )
biplot(mite_pca, display = "sites",   scaling = "species", type = "p")

mite_mds <- metaMDS(mite)
plot(mite_mds, display = "sites")

summary(mite_pca)


## Корреспондентный анализ данных "в темную"

mite_ca <- cca(mite)
summary(mite_ca)

biplot(mite_ca) #Почему не работает?




op <- par(mfrow = c(1, 2))
plot(mite_ca, display = "sites")
plot(mite_ca, display = "species")
par(op)

plot(mite_ca, scaling = "species")

plot(mite_ca, scaling = "sites")




# Матричная механка CA

peas <- matrix(c(99, 42, 29, 13), byrow = T, ncol = 2)


sum(peas)


Ft <- sum(peas)

f_i <- apply(peas, 1, FUN = sum)

f_j <- apply(peas, 2, FUN = sum)

p_i <- f_i/Ft #Вектор вероятностей для формы
p_j <- f_j/Ft #Вектор вероятностей для цвета


q <- p_i %*% t(p_j)


E <- (q * Ft)
O <- peas


sum((O - E)^2/E)

chisq.test(x = O, p = q, correct = F)

(O-E)/sqrt(E)


head(mite[ , 1:5])


f_ij <- mite #Частота встречи данного вида в данной пробе, то есть это первичные даные!

p_ij <- mite/sum(mite) #вероятность встретить особь в данной пробе.

Ft <- sum(mite) #Общее количество найденных животных

f_i <- apply(mite, 1, FUN = sum) #Общее количество особей в каждой пробе

p_i <- f_i/Ft #Вектор вероятностей встретить какую-либо особь в данной пробе

f_j <- apply(mite, 2, FUN = sum) #Общее количество особей в каждом виде

p_j <- f_j/Ft #Вектор вероятностей встретить особь данного вида




E <- f_i %*% t(f_j) / Ft

O <- mite


sum((round((O-E)^2/E)))/Ft


chisq.test(x=mite, p=p_ij, correct = F)$statistic/Ft

summary(mite_ca)$tot.chi


Q <- (p_ij - p_i %*% t(p_j))/sqrt(p_i %*% t(p_j))

Q <- as.matrix(Q)


sum(Q^2)


U <- svd(Q)$u
D <- diag(svd(Q)$d)
V <- svd(Q)$v


QQ <- U %*% D %*% t(V)

qplot(as.numeric(Q), as.numeric(QQ))

sum(round(Q - QQ, 1))

A <- t(Q) %*% Q

mite_eigen <- eigen(A)$value

mite_vectors <- eigen(A)$vectors


qplot(mite_eigen, diag(D) )

qplot(mite_eigen, diag(D)^2 )

round(diag(D), 2)

sum(mite_eigen)

D

dim(U)

dim(V)


sum((D)^2)


plot(mite_ca, display = "sites")

scores_sites <- diag(p_i^(-1/2)) %*% U

qplot(scores_sites[,1], scores_sites[,2])


scores_species <- diag(p_j^(-1/2)) %*% V

qplot(scores_species[,1], scores_species[,2])

plot(mite_ca, display = "species", type = "p", scaling = 1)

scores(mite_ca)


screeplot(mite_ca, bstick = T)

