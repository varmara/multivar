# title: "Корреспондентный анализ и анализ главных компонент"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Марина Варфоломеева, Вадим Хайтов


## Пример: Птицы в лесах Австралии

# Обилие 102 видов птиц в 37 сайтах в юго-восточной Австралии (Mac Nally, 1989; данные из Quinn, Keough, 2002). Можно ли описать отношения между сайтами небольшим числом главных компонент?
library(readxl)
birds <- read_excel(path = "data/macnally.xlsx")
str(birds)


## Задание: Проведите анализ главных компонент
library(vegan)
birds_pca <- rda(birds[, -c(1,2)], scale = T )

eig <- eigenvals(birds_pca)

biplot(birds_pca, scaling = 2, display = "sites")
biplot(birds_pca, scaling = 2, display = "species")



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

birds_ch_pca <- rda(birds_ch, scale = T)
eigenvals(birds_ch_pca)
biplot(birds_ch_pca, scaling=2, display = "sites")
biplot(birds_ch_pca, scaling=2, display = "species")

plot(procrustes(birds_ch_pca, birds_pca))


birds_h_pca <- rda(birds_h, scale = T)
eigenvals(birds_h_pca)
biplot(birds_h_pca, scaling=2, display = "sites")



## Ординация до и после трансформации данных
op <- par(mfrow = c(1, 3), cex = 0.9, mar = c(4, 4, 2.5, 0.5))
biplot(bird_pca, display = "sites", scaling = 1, main = "PCA,\nбез трансформации")
plot(procrustes(bird_h_pca, bird_pca), main = "Прокрустово\nпреобразование")
biplot(bird_h_pca, display = "sites", scaling = 1, main = "PCA,\nтрансформация Хеллингера")
par(op)


# Корреспондентный анализ

## Пример: Крысы
# Число грызунов разных видов в нескольких сайтах в южной Калифорнии (Bolger et al. 1997; данные из Quinn, Keough, 2002). Некоторые из этих сайтов оказались изолированы из-за урбанизации. Кроме того, несколько сайтов в исследовании из нефрагментированной местности.

rats <- read.csv("data/bolger1.csv")


## Задание: проведите анализ главных компонент
# - Используйте хордальное расстояние
# - Нарисуйте биплот расстояний

rats_pca <- rda(rats[, -c(1,2)], scale = T)

biplot(rats_pca, scaling = 2, display = "sites")
summary(rats_pca)



## Корреспондентный анализ данных про крыс
rats_ca <- cca(rats[ , -c(1, 2)])
summary(rats_ca)




## Сколько общей инерции объясняют первые две главных оси?


## Сколько главных осей достаточно?



## Биплот расстояний
plot(rats_ca, scaling = 1)

screeplot(rats_ca, bstick = T)

## Создаем функцию, чтобы быстрее рисовать цветные графики
col_ord_plot <- function(ord, scaling = 1, colvec = NULL, colfac, pch = 21, lab.cex = 1, leg.cex = 0.9, leg.pos = "bottom", ncol = 1, display.labs = TRUE, display.legend = TRUE, ...){
  if(is.null(colvec)){ # создаем вектор цветов
  ncolours <- length(levels(colfac))
  colvec <- rainbow(ncolours, s = 0.8, v = 0.9)
  }
  plot(ord, type = "n", scaling = scaling, ...) # пустой график
  # точки, раскрашенные по уровням фактора
  points(ord, display = "sites", scaling = scaling, pch = pch,
         col = colvec[colfac], bg = colvec[colfac], ...)
  if(display.labs == TRUE){ # подписи переменных
  text(ord, display = "species", scaling = scaling, cex = lab.cex)
  }
  if(display.legend == TRUE) { # легенда
  legend(x = leg.pos, legend = levels(colfac), bty = "n", pch = pch,
         col = colvec, pt.bg = colvec, cex = leg.cex, ncol = ncol)
  }
}

## График PCA
col_ord_plot(ord = rats_pca, colvec = c("steelblue", "red2"),    colfac = rats$TYPE, main = "PCA, хордальное расстояние")

## График CA
col_ord_plot(ord = rats_ca, colvec = c("steelblue", "red2"),  colfac = rats$TYPE, leg.pos = "topleft", main = "CA")






## Задание: Проведите корреспондентный анализ данных про птиц
# - исчез ли эффект подковы?


birds_ca <- cca(birds[, -c(1,2)])
str(birds)
birds$HABITAT <- factor(birds$HABITAT)

birds_mds <- metaMDS(birds[, -c(1,2)], distance = "bray")

col_ord_plot(ord = birds_ca,  colfac = birds$HABITAT, leg.pos = "topleft", main = "CA", display.labs = F)

col_ord_plot(ord = birds_mds,  colfac = birds$HABITAT, leg.pos = "topleft", main = "CA", display.labs = F)


plot(procrustes(birds_mds, birds_ca))

plot(procrustes(birds_mds, birds_pca))


length(eigenvals(birds_pca))

length((birds_ca))

summary(birds_ca)

ncol(birds[, -c(1,2)])
