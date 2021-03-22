# ---
# title: "(Геометрическая) морфометрия"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Марина Варфоломеева
# ---


# # Классический подход к морфометрии

# ## Пример: морфометрия черепах

# Черепахи - единственные живые представители анапсид (череп не имеет височных окон). Морфология черепа важна для их систематики (Claude et al., 2004).
# Данные - 24 разных измерения черепов черепах 122 ныне живущих пресноводных, морских и наземных видов и одного ископаемого. (Из Zuur et al. 2007)

# ## Читаем данные
turt <- read.table("data/turtles.txt", header = TRUE)
turt$Environment3 <- factor(turt$Environment3, levels = c(0, 1, 2, 9), labels = c("Freshwater", "Terrestrial", "Marine", "Fossil"))
colnames(turt)


# Нужно ли стандартизовать исходные данные?
boxplot(x = turt[8:31])

# ## Делаем анализ главных компонент
library(vegan)
turt_pca <- rda(turt[, 8:31], scale = TRUE)

# ## Сколько компонент достаточно для описания данных?
eig <- eigenvals(turt_pca)[1:5]
eig*100/sum(eig) # доля объясненной изменчивости
screeplot(turt_pca, bstick = TRUE)

# ## Что странного в этой картинке?
biplot(turt_pca, display = "species", scaling = 2)

# - Как вы думаете, почему у всех переменных большие нагрузки по первой компоненте?
# - Что отражает первая компонента?


# ## Двойное центрирование - один из классических способов избавиться от влияния размера

# Функция, которая может центрировать вектор
center <- function(x){
  x - mean(x, na.rm = TRUE)
}
# Почему для двойного центрирования перед PCA достаточно применить эту функцию к строкам?
dbcent <- t(apply(turt[, 8:31], 1, center))

# PCA
turt_db_pca <- rda(dbcent, scale = TRUE)
eig_db <- eigenvals(turt_db_pca)[1:5]
eig_db*100/sum(eig_db)
screeplot(turt_db_pca, bstick = TRUE)
biplot(turt_db_pca, display = "species", scaling = 2)


# ## Код для графика ординации черепах по морфометрии черепов
op <- par(mfrow = c(1, 2), mar = c(4, 4, 0.5, 0.5), cex = 1)
biplot(turt_db_pca, display = "species", scaling = 2)
# цвета для графика факторных координат
colvec <- c("orange2", "limegreen", "steelblue", "red3")
# пустой график
plot(turt_db_pca, type = "n", scaling = 1)
# точки, раскрашенные по уровням фактора turt$Environment3
points(turt_db_pca, display = "sites", scaling = 1, pch = 21,
       col = colvec[turt$Environment3], bg = colvec[turt$Environment3])
# легенда
legend("topright", legend = levels(turt$Environment3), bty = "n", pch = 21,
       col = colvec, pt.bg = colvec)
par(op)


# # Геометрическая морфометрия -----

# ## Пример: Форма головы Апалачских саламандр рода _Plethodon_
#
# _Plethodon jordani_ и _P.teyahalee_ встречаются вместе и раздельно.
# В совместно обитающих популяциях меняется форма головы обоих видов. В разных группах популяций этот процесс параллельно приводит к одинаковым результатам. По-видимому, одной из причин параллельной эволюции может быть межвидовая конкуренция (Adams, 2004, 2010).

# install.packages("geomorph", dependencies = TRUE)
library(geomorph)
data(plethodon)
str(plethodon, vec.len = 2, give.attr = F)

# ## Сырые морфометрические данные еще не выравнены
# Два образца для примера
plotRefToTarget(plethodon$land[, , 1], plethodon$land[, ,10],
                method = "points", mag = 1,
                links = plethodon$links)
# Слева - три образца, справа - все. Жирные точки - центроиды соответствующих меток
op <- par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
plotAllSpecimens(plethodon$land[, , 1:3], links=plethodon$links)
plotAllSpecimens(plethodon$land,links=plethodon$links)
par(op)


## Шаг 1. Выравниваем данные при помощи обобщенного прокрустова анализа ---

gpa <- gpagen(plethodon$land, print.progress = FALSE)
plotAllSpecimens(gpa$coords,links=plethodon$links)

# ## Усредненная форма
ref <- mshape(gpa$coords)
plotRefToTarget(ref, ref, method = "TPS", links = plethodon$links)

# ## Можем посмотреть, как отличается любой из образцов от усредненной формы

# Изменение формы можно представить графически несколькими способами


# матрица, в которой хранится разметка общего графика
m <- matrix(data = c(1, 2,
                     3, 3),
            nrow = 2, ncol = 2, byrow = TRUE)
l <- layout(m, heights = c(1, 1), widths = c(1, 1))
# layout.show(l) # можно просмотреть разметку

# Графики
op <- par( mar = c(0, 0, 0, 0))
# 1) изменение конфигурации обозначено векторами
plotRefToTarget(ref, gpa$coords[, , 11],
                method = "vector", mag = 1,
                links = plethodon$links)
# 2) формы обозначены точками
plotRefToTarget(ref, gpa$coords[, , 11],
                method = "points", mag = 1,
                links = plethodon$links)
# 3) сплайн
plotRefToTarget(ref, gpa$coords[, , 11],
                method = "TPS", mag = 1,
                links = plethodon$links)
par(op)



# ## Шаг 2. Создаем морфопространство
ord <- gm.prcomp(gpa$coords)
plot(ord, main = "PCA")

#
#
# ## Можно раскрасить по группам
op <- par(mar = c(4, 4, 1, 1))
gp <- as.factor(paste(plethodon$species, plethodon$site)) # группа должна быть фактором
# задаем соответствие цветов уровням фактора
colvec <- c("Jord Allo" = "yellow2",
            "Jord Symp" = "orange",
            "Teyah Allo" = "green4",
            "Teyah Symp" = "green1")
# вектор цветов в порядке заданном фактором gp
colvec <- colvec[match(gp, names(colvec))]
# график
plot(ord, bg = colvec, pch = 21, col = "grey20")
# легенда
legend("topright", legend = levels(gp),
                   bty = "n", pch = 21,
                   col = "grey20",
                   pt.bg = levels(as.factor(colvec)))
par(op)


# ## Доля объясненной изменчивости и факторные координаты
expl <- round(ord$d[1:5]/sum(ord$d) * 100, 1) # Доля изменчивости объясненной 1-5 компонентами
head(ord$x[, 1:5]) # Факторные координаты по 1-5 компонентам

# ## Чтобы легко рисовать изменения формы вдоль главной компоненты нам понадобится функция
plot_shape_change <- function(ord, ref_shape, PC,
                              horiz = TRUE,
                              gridPars = NULL, ...){
  if(horiz){
    op <- par(mfrow = c(1, 2), mar = c(0, 0 , 0, 0))
    plotRefToTarget(M1 = ref_shape, M2 = ord$shapes[[PC]]$min,
                    gridPars = gridPars,  ...)
    plotRefToTarget(M1 = ref_shape, M2 = ord$shapes[[PC]]$max,
                    gridPars = gridPars, ...)
    par(op)
    } else {
     op <- par(mfrow = c(2, 1), mar = c(0, 0 , 0, 0))
     plotRefToTarget(M1 = ref_shape, M2 = ord$shapes[[PC]]$max,
                     gridPars = gridPars,  ...)
     plotRefToTarget(M1 = ref_shape, M2 = ord$shapes[[PC]]$min,
                     gridPars = gridPars, ...)
     par(op)
    }
}

# ## Изменение формы вдоль главных компонент относительно средней формы
plot_shape_change(ord, ref_shape = gpa$consensus, PC = 1, links = plethodon$links, method = "TPS")

plot_shape_change(ord, ref_shape = gpa$consensus, PC = 2, links = plethodon$links, method = "TPS", horiz = FALSE)

# ## Можно нарисовать одновременно изменение формы вдоль обеих компонент и ординацию

my_gridPar <- gridPar(tar.pt.size = 0.6, grid.lwd = 0.7)

gg_pca <- plot_grid(
  ~ plot_shape_change(ord, ref_shape = gpa$consensus, PC = 2,
                      horiz = FALSE, links = plethodon$links,
                      method = "TPS", gridPars = my_gridPar),
  ~ {plot(ord, bg = colvec, pch = 21, col = "grey20")
    legend("topright", legend = levels(gp),  bty = "n",
           pch = 21, col = "grey20",
           pt.bg = levels(as.factor(colvec)))},
  NULL,
  ~ plot_shape_change(ord, ref_shape = gpa$consensus, PC = 1,
                      links = plethodon$links,
                      method = "TPS", gridPars = my_gridPar),
  ncol = 2, rel_heights = c(5, 1), rel_widths = c(1, 4)
)

gg_pca


# # Эволюционные изменения формы

# ## Фило-морфопространство саламандр рода Plethodon
data(plethspecies)
str(plethspecies, vec.len = 2, give.attr = F)

# ## Выравниваем средние формы для видов
species_gpa <- gpagen(plethspecies$land) #GPA-alignment

# ## Наложение филогенетического дерева и анцестральных форм на график PCA ординации

# Филоморфопространство
pca_with_phylo <- gm.prcomp(species_gpa$coords, phy = plethspecies$phy)
plot(pca_with_phylo, phylo = TRUE)
