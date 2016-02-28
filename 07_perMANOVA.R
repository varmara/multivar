# ---
# title: "Тестирование гипотез на основе многомерных данных: PERMANOVA"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Марина Варфоломеева, Вадим Хайтов
# ---

## Пример: Поведение песчанок в тесте открытое поле
# **Гипотеза:** Разные виды песчанок демонстрируют различия поведения в тесте "Открытое поле"
# Виды:
# * Карликовая песчанка (_Gerbillus gerbillus_)
# * Монгольская песчанка (_Meriones unguiculatus_)
# * Жирнохвостая песчанка (_Pachyuromys duprasi_)
# Оценка поведения песчанок трех видов по семи признакам
# * Время до выхода в квадрат открытого поля
# * Количество актов мочеиспускания
# * Количество актов дефекации
# * Количество пересеченных квадратов
# * Число вертикальных стоек
# * Количество актов смещенной активности
# * Время проведенное в центре квадрата открытого поля

## Данные наблюдений
pesch <- read.csv("data/pesch.csv", header = TRUE, sep = ";")
head(pesch)

# ## Задание
# - Какое расстояние можно использовать с этими данными?
# - Как можно преобразовать данные?
# - Постройте ординацию объектов в осях MDS и раскрасьте точки в соответствии с видами





# PERMANOVA
library(vegan)
permanova_pesch <- adonis(log_pesch[3:9] ~ log_pesch$Species, method = "euclidean")
permanova_pesch




# Условия применимости PERMANOVA

dist_pesch <- vegdist(log_pesch[,3:ncol(pesch)], method  = "euclidian")
PCO_pesch <- betadisper(dist_pesch, log_pesch$Species)
plot(PCO_pesch)
anova(PCO_pesch) #PERMDISP2
boxplot(PCO_pesch)



# Более подробная интерпретация результатов perMANOVA

library(ggplot2)
#dist_pesch <- vegdist(log_pesch[,3:ncol(pesch)], method = "euclidian")
mds_pesch <- metaMDS(log_pesch[,3:ncol(pesch)], distance = "euclidian", trace = FALSE)
mds_pesch <- as.data.frame(mds_pesch$points)
mds_pesch$Species <- pesch$Species
pl_pesch <- ggplot(mds_pesch, aes(x = MDS1, y = MDS2, colour = Species))
pl_pesch + geom_point(size = 5)

## Попарные сравнения

pair <- combn(unique(as.character(log_pesch$Species)), 2)
ncomb <- dim(pair)[2]
x <- log_pesch[, -c(1:2)]
y <- log_pesch$Species
for (i in 1:ncomb) {
  filter <- y %in% pair[, i]
  posthoc <- adonis(x[filter, ] ~ y[filter], method = "euclidean")$aov.tab$Pr[1]
  cat(pair[, i], ": p = ", posthoc, "\n", sep = " ")
}



# ## Задание
# Выясните, какой из признаков сильнее всего различается между видами?




# Более сложные дизайны в PERMANOVA

# Отфильтруем исходные данные
log_pesch2 <- log_pesch[log_pesch$Species != "zhirnokhvost", ]

## Двухфакторный PERMANOVA
twofact_pesch <- adonis(log_pesch2[,3:ncol(pesch)] ~ log_pesch2$Gender * log_pesch2$Species, method = "euclidian")
twofact_pesch

## Здесь возможен иерархический дизайн
nested_pesch <- adonis(log_pesch2[, 3:ncol(pesch)] ~  log_pesch2$Gender, strata = log_pesch2$Species, method = "euclidian")
nested_pesch

# ## Задание
# + Создайте датафрейм из файла `simulated_data.csv` (Это данные симулированные по алгоритму, приведенному в справке по функции `adonis()`)
# + В полученном датафрейме описано обилие двух видов на экспериментальных площадках двух типов: без добавления и с добавлением NO3, по 6 повторнотей в каждом эксперименте.
# Эксперименты были независимо проведены на 3 полях.
# + Оцените, зависит ли структура совместного поселения этих двух видов от концентрации NO3.







