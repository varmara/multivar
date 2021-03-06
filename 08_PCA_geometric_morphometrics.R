# title: "(Геометрическаая) морфометрия"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Марина Варфоломеева


# # Классический подход к морфометрии ###################################

# ## Пример: морфометрия черепах ######################################################
#
# Черепахи - единственные живые представители анапсид (череп не имеет височных окон). Морфология черепа важна для их систематики (Claude et al., 2004).
#
# Данные - 24 разных измерения черепов черепах 122 ныне живущих пресноводных, морских и наземных видов и одного ископаемого.
# из Zuur et al. 2007
#
#
# ## Читаем данные
turt <- read.table("data/turtles.txt", header = TRUE)
turt$Environment3 <- factor(turt$Environment3, levels = c(0, 1, 2, 9), labels = c("Freshwater", "Terrestrial", "Marine", "Fossil"))
colnames(turt)

# ## Чтобы понять, нужно ли стандартизовать исходные данные, построим боксплот
boxplot(x = turt[8:31])


# ## Задание 1: Проведите анализ главных компонент -------------------------
#
# - Сколько изменчивости объясняют компоненты?
# - Сколько компонент достаточно для описания данных?






# ## Что странного в этой картинке?
biplot(turt_pca, display = "species", scaling = 2)





# ## Двойное центрирование #############################
# Функция, которая может центрировать вектор
center <- function(x){
  x - mean(x, na.rm = TRUE)
}
# применяем эту функцию к каждой строке
dbcent <- t(apply(turt[, 8:31], 1, center))
# получившийся датафрейм пришлось транспонировать,
# поскольку apply() результаты от каждой строки
# возвращает в виде столбцов


# ## Задание 3 --------------------------------------
#
# - Проведите анализ главных компонент по центрированным данным (`dbcent`). При помощи скольки компонент можно адекватно описать данные?
# - Постройте график факторных нагрузок. Изменилась ли интерпретация компонент?






# ## Код для графика ординации черепах по морфометрии черепов
op <- par(mfrow = c(1, 2), mar = c(4, 4, 0.5, 0.5), cex = 1.3)
biplot(turt_db_pca, display = "species", scaling = 2)
# цвета для графика факторных координат
colvec <- c("orange2", "limegreen", "steelblue", "red3")
# пустой график
plot(turt_db_pca, type = "n", scaling = 1)
# точки, раскрашенные по уровням фактора turt$Environment3
points(turt_db_pca, display = "sites", scaling = 1, pch = 21,
       col = colvec[turt$Environment3], bg = colvec[turt$Environment3])
# легенда
legend("bottomright", legend = levels(turt$Environment3), bty = "n", pch = 21,
       col = colvec, pt.bg = colvec)
par(op)




# # Геометрическая морфометрия ################################

# ## Пример: Форма головы Апалачских саламандр рода _Plethodon_
#
# _Plethodon jordani_ и _P.teyahalee_ встречаются вместе и раздельно.
# В совместно обитающих популяциях меняется форма головы обоих видов. В разных группах популяций этот процесс параллельно приводит к одинаковым результатам. По-видимому, одной из причин параллельной эволюции может быть межвидовая конкуренция (Adams, 2004, 2010).

# ## Морфометрия головы саламандр

# install.packages("geomorph", dependencies = TRUE)
library(geomorph)
data(plethodon)
str(plethodon, vec.len = 2, give.attr = F)


# ## Сырые морфометрические данные еще не выровнены
plotRefToTarget(plethodon$land[, , 1], plethodon$land[, ,10],
                method = "points", mag = 1, links = plethodon$links)

op <- par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plotAllSpecimens(plethodon$land[, , 1:3], links=plethodon$links)
plotAllSpecimens(plethodon$land,links=plethodon$links)
par(op)


# ## Геометрическая морфометрия

# ## Шаг 1. Выравниваем данные при помощи обобщенного прокрустова анализа
#
# Generalized Procrustes Analysis (GPA)
Y.gpa <- gpagen(plethodon$land, print.progress = FALSE)
plotAllSpecimens(Y.gpa$coords,links=plethodon$links)

# ## Усредненная форма
ref <- mshape(Y.gpa$coords)
plotRefToTarget(ref, ref, method = "TPS", links = plethodon$links)

# ## Можем посмотреть, как отличается любой из образцов от усредненной формы

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
plotRefToTarget(ref, Y.gpa$coords[, , 11],
                method = "vector", mag = 1,
                links = plethodon$links)
# 2) формы обозначены точками
plotRefToTarget(ref, Y.gpa$coords[, , 11],
                method = "points", mag = 1,
                links = plethodon$links)
# 3) сплайн
plotRefToTarget(ref, Y.gpa$coords[, , 11],
                method = "TPS", mag = 1,
                links = plethodon$links)
par(op)



# ## Шаг 2. Создаем морфопространство
plotTangentSpace(Y.gpa$coords)


# ## Можно раскрасить по группам
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
plot.new(); legend("center", legend = levels(gp),
                   bty = "n", pch = 21,
                   col = "grey20",
                   pt.bg = levels(as.factor(colvec)))
par(op)




# ## Задание 4 ---------------------
#
# Исследуйте структуру объекта результатов
#
# - Cколько процентов изменчивости объясняют первые 2 или 3 компоненты?
# - Как изменяется форма вдоль 2 компоненты в отрицательном и положительном направлении относительно средней формы? Постройте график





# # Эволюционные изменения формы ---------------------

# ## Фило-морфопространство саламандр рода Plethodon
# P. serratus, P. cinereus, P. shenandoah, P. hoffmani, P. virginia, P. nettingi, P. hubrichti, P. electromorphus, P. richmondi
data(plethspecies)
str(plethspecies, vec.len = 2, give.attr = F)

# ## Выравниваем средние формы для видов
Yphyl.gpa <- gpagen(plethspecies$land) #GPA-alignment

# ## Наложение филогенетического дерева и анцестральных форм на график PCA ординации
plotGMPhyloMorphoSpace(plethspecies$phy, Yphyl.gpa$coords)

