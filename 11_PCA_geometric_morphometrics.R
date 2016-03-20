# title: "Специальные случаи применения анализа главных компонент"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Марина Варфоломеева, Вадим Хайтов


# install.packages("geomorph", dependencies = TRUE)


# Классический подход к морфометрии

## Пример: морфометрия черепах
# Черепахи - единственные живые представители анапсид (череп не имеет височных окон). Морфология черепа важна для их систематики (Claude et al., 2004).
# Данные - 24 разных измерения черепов черепах 122 ныне живущих пресноводных, морских и наземных видов и одного ископаемого (из Zuur et al. 2007).

turt <- read.table("data/turtles.txt", header = TRUE)
turt$Environment3 <- factor(turt$Environment3, levels = c(0, 1, 2, 9), labels = c("Freshwater", "Terrestrial", "Marine", "Fossil"))
colnames(turt)

boxplot(x = turt[8:31])

# ## Задание: Проведите анализ главных компонент
# - Сколько изменчивости объясняют компоненты?
# - Сколько компонент достаточно для описания данных?



## Задание:

### Придумайте способ избавится от влияния размера





# ## Задание:
# - Проведите анализ главных компонент по центрированным данным (`dbcent`). При помощи скольки компонент можно адекватно описать данные?
# - Постройте график факторных нагрузок. Изменилась ли интерпретация компонент?



## График с раскрашенными точками
op <- par(mfrow = c(1, 2), mar = c(4, 4, 0.5, 0.5), cex = 1.3)
biplot(turt_db_pca, display = "species", scaling = 2)
# цвета
colvec <- c("orange2", "limegreen", "steelblue", "red3")
# пустой график
plot(turt_db_pca, type = "n", scaling = 1)
# точки, раскрашенные по уровням фактора turt$Environment3
points(turt_db_pca, display = "sites", scaling = 1, pch = 21, col = colvec[turt$Environment3], bg = colvec[turt$Environment3])
# легенда
legend("bottomright", legend = levels(turt$Environment3), bty = "n", pch = 21, col = colvec, pt.bg = colvec)
par(op)


# Геометрическая морфометрия
## Пример: Форма головы Апалачских саламандр рода _Plethodon_
# _Plethodon jordani_ и _P.teyahalee_ встречаются вместе и раздельно.
# В совместно обитающих популяциях меняется форма головы обоих видов. В разных группах популяций этот процесс параллельно приводит к одинаковым результатам. По-видимому, одной из причин параллельной эволюции может быть межвидовая конкуренция (Adams, 2004, 2010).

library(geomorph)
data(plethodon)
str(plethodon, vec.len = 2, give.attr = F)



plotRefToTarget(plethodon$land[, , 1], plethodon$land[, ,10], method = "points", mag = 1, links = plethodon$links)



op <- par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plotAllSpecimens(plethodon$land[, , 1:3], links=plethodon$links)
plotAllSpecimens(plethodon$land,links=plethodon$links)
par(op)


## Прокрустов анализ
## Выравниваем головы саламандр
Y.gpa <- gpagen(plethodon$land)
plotAllSpecimens(Y.gpa$coords,links=plethodon$links)

## Усредненная форма
ref <- mshape(Y.gpa$coords)
plotRefToTarget(ref, ref, method = "TPS", links = plethodon$links)

## Отличия от усредненной формы (несколько способов)
# матрица, в которой хранится разметка общего графика
m <- matrix(data = c(1, 1, 2, 2,
                  3, 3, 3, 3,
                  3, 3, 3, 3),
          nrow = 3, ncol = 4, byrow = TRUE)
l <- layout(m, heights = c(3, 2))
# layout.show(l) # можно просмотреть разметку
# Графики
op <- par( mar = c(4, 4, 1, 1))
# 1) изменение конфигурации обозначено векторами
plotRefToTarget(ref, Y.gpa$coords[, , 11], method = "vector", mag = 1, links = plethodon$links)
# 2) формы обозначены точками
plotRefToTarget(ref, Y.gpa$coords[, , 11], method = "points", mag = 1, links = plethodon$links)
# 3) сплайн
plotRefToTarget(ref, Y.gpa$coords[, , 11], method = "TPS", mag = 1, links = plethodon$links)
par(op)



## Шаг 2. Создаем морфопространство
plotTangentSpace(Y.gpa$coords)


## Можно раскрасить по группам
op <- par(mar = c(4, 4, 0, 0))
gp <- as.factor(paste(plethodon$species, plethodon$site)) # группа должна быть фактором
# задаем соответствие цветов уровням фактора
colvec <- c("Jord Allo" = "yellow2",
            "Jord Symp" = "orange",
            "Teyah Allo" = "green4",
            "Teyah Symp" = "green1")
# вектор цветов в порядке заданном фактором gp
colvec <- colvec[match(gp, names(colvec))]
res <- plotTangentSpace(Y.gpa$coords, groups = colvec, verbose = TRUE)
par(op)

# легенда
op <- par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = levels(gp), bty = "n", pch = 21, col = "grey20", pt.bg = levels(as.factor(colvec)))
par(op)



# ## Задание:
# Исследуйте структуру объекта результатов
# - Cколько процентов изменчивости объясняют первые 2 или 3 компоненты?
# - Как изменяется форма вдоль 2 компоненты в отрицательном и положительном направлении относительно средней формы? Постройте график







## Составной рисунок
mat <- matrix(c(0, 1, 0,
                2, 6, 3,
                0, 4, 5),
              nrow = 3, ncol = 3, byrow = TRUE)
l <- layout(mat, widths = c(1, 2, 1), heights = c(1, 3, 1))
# layout.show(l)
op <- par(mar = c(0, 0 , 0, 0)) # параметры для 1-4 графиков
# графики форм (слева, справа, снизу, сверху)
plotRefToTarget(M1 = ref, M2 = res$pc.shapes$PC2max, method = "TPS", links = plethodon$links)
plotRefToTarget(M1 = ref, M2 = res$pc.shapes$PC1min, method = "TPS", links = plethodon$links)
plotRefToTarget(M1 = ref, M2 = res$pc.shapes$PC1max, method = "TPS", links = plethodon$links)
plotRefToTarget(M1 = ref, M2 = res$pc.shapes$PC2min, method = "TPS", links = plethodon$links)
# легенда снизу слева
par(mar = c(0.5, 0, 0, 0)) # параметры для легенды
plot.new(); legend("center", legend = levels(gp), bty = "n", pch = 21, col = "grey20",pt.bg = levels(as.factor(colvec)), cex = 2)
# в центре
par(mar = c(4, 4 , 1, 1), cex = 1) # параметры для последнего графика
plotTangentSpace(Y.gpa$coords, warpgrids = FALSE, groups = colvec)
par(op)




# Эволюционные изменения формы

## Фило-морфопространство саламандр рода Plethodon
data(plethspecies)
str(plethspecies, vec.len = 2, give.attr = F)

## Выравниваем средние формы для видов
Yphyl.gpa <- gpagen(plethspecies$land)

## Наложение филогенетического дерева и анцестральных форм на график PCA ординации
plotGMPhyloMorphoSpace(plethspecies$phy, Yphyl.gpa$coords)
