# title: "Корреспондентный анализ и анализ главных компонент"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Вадим Хайтов, Марина Варфоломеева


## Пример: Птицы в лесах Австралии

# Обилие 102 видов птиц в 37 сайтах в юго-восточной Австралии (Mac Nally, 1989; данные из Quinn, Keough, 2002). Можно ли описать отношения между сайтами небольшим числом главных компонент?
library(readxl)
birds <- read_excel(path = "data/macnally.xlsx")
str(birds)


## Задание: Проведите анализ главных компонент



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






#Построение искусственно созданного градиента

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


pca_dat <- rda(dat2)
plot(pca_dat, display = "sites", type = "p", scaling = 2)



# Корреспондентный анализ

data(mite)


## Задание: проведите анализ главных компонент




## Корреспондентный анализ данных "в темную"

mite_ca <- cca(mite)
head(summary(mite_ca))


op <- par(mfrow = c(1, 2))
plot(mite_ca, display = "sites")
plot(mite_ca, display = "species")
par(op)


# Матричная механка CA




