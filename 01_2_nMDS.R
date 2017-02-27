# ---
# title: "Ординация. Многомерное шкалирование."
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Марина Варфоломеева, Вадим Хайтов
# ---

# install.packages(scatterplot3d)
# install.packages(gridExtra)

## Пример: Сообщества бентоса акватории Долгой губы (о. Б. Соловецкий, Белое море)
# Нинбург, 1990; Хайтов и др., 2013

abund <- read.table("data/dolg_abundance.txt", skip = 1, header = TRUE, sep = ";")
hydrol <- read.table("data/dolg_hydrology.txt", skip = 1, header = TRUE, sep = ";")

log_abund <- log(abund[,-1] + 1)
row.names(log_abund) <- abund[,1]

# ## Задание: Прямая ординация станций в осях Температуры и Солености
# Постройте диаграмму, отражающую ординацию станций в осях Температуры и Солености.
# Модифицируйте график так, чтобы была еще видна связь с суммарным обилием видов в пробах.





# Неметрическое многомерное шкалирование
# Трансформируем данные
log_abund <- log(abund[,-1] + 1)
row.names(log_abund) <- abund$Station

library (vegan)
ord <- metaMDS(log_abund, distance = "bray", k = 2) # результаты сохраняются в объекте ord
ord$points

ordiplot(ord, display = "sites", type = "t")

## Графическое представление результатов средствами пакета `vegan`
ordiplot(ord, display = "sites" )
# text(ord) #Можно добавить обозначения сайтов (объектов)

# ## Задание: Графическое представление результатов средствами `ggplot2`
# - Исследуйте объект `ord` и извлеките из него данные с новыми координатами
# - Постройте график ординации при помощи пакета `ggplot2`
# - Раскрасьте точки на ординации согласно глубине (данные в `hydrol`)

str(ord)


## Задание:
# - Создайте датафрейм, содержащий исходные данные (без логарифмирования) только по сайтам S17, S33, S37, S38, S44, S59.
# - Постройте ординацию этих объектов с использованием в качестве меры различия коэффициент Брея-Куртиса.
# - Измерьте линейкой расстояния между точками на ординации
# - Сравните матрицу коэффициентов Брея-Куртиса и матрицу расстояний между точками на ординации.

obj <- c("S17", "S33", "S37", "S38", "S44", "S59")


red_abund <- log_abund[abund$Station %in% obj, ]

ord1 <- metaMDS(red_abund)

plot(ord1, display = "site", type = "t")

## Взаиморасположение точек на плоскости подобно взаиморасположению точек в многомерном пространстве признаков
library(ggplot2)
dist_compare <- data.frame(Bray = as.vector(vegdist(red_abund[, -1])), MDS = as.vector(vegdist(ord1$points, method = "euclidean")))
ggplot(dist_compare, aes(x = Bray, y = MDS)) + geom_point(size = 4) + xlab("Bray-Curtis dissimilarity") + ylab("Distance between points on ordination")

## Диаграмма Шепарда
stressplot(ord1)

## Задание:
# Постройте диаграмму Шепарда вместе с монотонной регрессией на полном материале по Долгой губе. Найдите величину стресса.
# Надежна ли такая ординация?

stressplot(metaMDS(log_abund))

metaMDS(log_abund)$stress

## MDS в трехмерном пространстве
library(scatterplot3d)
ord3 <- metaMDS(log_abund, k = 3, trace = FALSE)
scatterplot3d(x = ord3$points[,1], y = ord3$points[,2], z = ord3$points[,3], xlab = "MDS 1", ylab = "MDS 2", zlab = "MDS 3")




# Сравнение ординаций

## Задание:
# - Постройте ординацию всех станций с использованием Евклидова расстояния


ord4 <- metaMDS(log_abund, distance = "eucl", k = 2, trace = FALSE)



## Насколько похожи ординации, полученные при использовании коэффициента Брея-Куртиса и Евклидова расстояния?
op <- par(mfrow = c(1, 2))
ordiplot(ord, display = "site", type = "text", main = "Bray-Curtis")
ordiplot(ord4, display = "site", type = "text", main = "Euclidean")
par(op)



## Прокурстово преобразование
procrust <- procrustes(ord, ord4)
plot(procrust)
text(procrust, adj = 1, cex = 0.6)


# Трактовка результатов ординации
th <- theme( panel.background = element_rect(fill = "white", color = "black"), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), axis.text = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), legend.position = "bottom")

pl6 <- ggplot(data.frame(ord$points), aes(x = MDS1, y = MDS2, fill = hydrol$Depth)) + geom_point(shape = 21, size = 4) + scale_fill_gradient(low = "white", high = "black") + labs(fill = "Depth") + th

pl7 <- ggplot(data.frame(ord$points), aes(x = MDS1, y = MDS2, fill = hydrol$Sal)) + geom_point(shape = 21, size = 4)  + scale_fill_gradient(low = "cyan", high = "darkblue") + labs(fill = "Salinity")+ th

pl8 <- ggplot(data.frame(ord$points), aes(x = MDS1, y = MDS2, fill = hydrol$Temp)) + geom_point(shape = 21, size = 4) + scale_fill_gradient(low = "yellow", high = "red")  + labs(fill = "Temperature")+ th

pl9 <- ggplot(data.frame(ord$points), aes(x = MDS1, y = MDS2, fill = hydrol$Water_content)) + geom_point(shape = 21, size = 4) + scale_fill_gradient(low = "green", high = "black")  + labs(fill = "Water content") + th

library(gridExtra)
grid.arrange(pl6, pl7, pl8, pl9, ncol = 2)
