# title: "Неметрическое многомерное шкалирование, envfit, ordisurf"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Марина Варфоломеева, Вадим Хайтов

# Загрузка пакетов ###############################

# Чтение файлов
library(readxl)

# Обработка данных
library(tidyr)
library(dplyr)
library(broom)

# Графики
library(ggmap)
theme_set(theme_bw())

# install.packages("devtools")
# devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan)

# Многомерная статистика
library(vegan)


# Неметрическое многомерное шкалирование ##########

# Карта пригородов Санкт-Петербурга

# Матрица расстояний
dist_spb <- read_excel("data/dist_spb.xlsx", sheet = "dist")
D <- as.matrix(dist_spb[, -1]) %>%
  `rownames<-`(dist_spb$name) %>%
  as.dist()

# Координаты городов
coord <- read_excel("data/dist_spb.xlsx", sheet = "coord")

# Фон карты
load(file = "data/dist_spb_dat.RData")

# Карта окрестностей спб
gg_spb <- ggmap(map_dat) +
  geom_point(data = coord, aes(x = lng, y = lat,
                               size = population/1000),
             alpha = 0.8, colour = "grey20") +
  geom_text(data = coord, aes(x = lng, y = lat,
                              label = name),
            vjust = -0.3, hjust = -0.05) +
  theme(legend.position = "none") +
  labs(x = "Долгота", y = "Широта",
       size = "Население,\nтыс. чел.")
gg_spb


# ## Расстояния по автодорогам

# Проекция при помощи анализа главных координат (PCoA)
ordiplot(wcmdscale(d = D, k = 2), type = "t")

# Ординация nMDS
op <- par(mar = c(3, 3, 0.1, 0.1), mgp = c(2, 1, 0))
spb_ord <- metaMDS(D)
ordiplot(spb_ord, type = "t")
par(op)

# >- Что странного в этих картах?




# # Как работает nMDS #############################


# Реальные расстояния
obs_dist <- D %>%
  tidy() %>%
  rename("observed" = "distance")
# Расстояния на ординации
ord_dist <- spb_ord$points %>%
  vegdist(method = "euclidean") %>%
  tidy() %>%
  rename("nmds" = "distance")
# Все вместе (отсортированное)
distances <- merge(obs_dist, ord_dist) %>%
  arrange(observed)

distances

# график зависимости расстояний между точками на ординации и соответствующих им
# значений коэффициентов различия в исходном пространстве
ggplot(distances, aes(x = observed, y = nmds)) + geom_point() +
  xlab("Observed dissimialrity") + ylab("Ordination distance")


# ## Диаграмма Шепарда
stressplot(spb_ord)

# ## Монотонная регрессия

# ## Стресс --- мера качества ординации
spb_ord$stress


# # Пример #######################################

# ## Симбионты мидий

# Данные можно скачать [с сайта Pangaea](https://doi.pangaea.de/10.1594/PANGAEA.870537?format=textfile)

# [Krapivin 2017](https://doi.org/10.1594/PANGAEA.870537), [Krapivin et al. 2018](https://doi.org/10.3354/dao03259)

# ## Открываем данные

dat <- read.delim("data/Krapivin_2017_Medulis-symb.tab", skip = 36)
str(dat)

# ## Приводим в порядок названия переменных

colnames(dat) # Было
# удаляем мешающие точки в названиях столбцов
colnames(dat) <- gsub("[.]", replacement = "", colnames(dat))
# переименовываем нужные переменные
dat <- dat %>%
  rename(L = "MedulisLshellmm",
         Age = "Medulisagea")
colnames(dat) # Стало

# ## Приводим в порядок данные

# Делаем сайт фактором
dat$Site <- factor(dat$Site, levels = c(2, 3, 1), labels = c("G","L","S"))

# Сливаем редкие виды в одну категорию (кроме трематод)
colSums(dat[, 10:23])
f_remove <- c("Nematoda", "Microsetella", "Copepoda",
              "Chironomidae", "Halacaridae", "Jaeraspp",
              "Ostrac")
dat$Other <- rowSums(dat[, f_remove])

# Суммарная численность симбионтов
f_sp <- c("Urastomaspp", "Renicolaspp", "Himasthlaspp",
          "Metacercaria", "Gymnophallusspp", "Alg",
          "Other")
dat$Total  <- rowSums(dat[, f_sp])

# Данные для анализа

# Только мидии с симбионтами и возрастом от 3 до 8 лет
dat <- dat[dat$Total != 0 & dat$Age %in% 3:8, ]
spec <- dat[, f_sp]                         # виды-симбионты
env <- dat[, c("Zone", "Site", "L", "Age")] # свойства мидий-хозяев



# # Ординация сообществ симбионтов в мидиях
#
# ## Задание 1 ------------------------------------
#
# Постройте ординацию мидий по обилию разных
# видов-симбионтов с использованием коэффициента
# различия Брея-Куртиса.
# Следите, чтобы алгоритму удалось найти финальное
# решение. Если необходимо, настройте вызов
# `metaMDS()`
#
# Вычислите стресс для получившейся ординации.
#
# Нарисуйте простейший график при помощи функции ordiplot().

# ord_mus <-











# Палитры
pal_col <- c("red", "green", "steelblue")
pal_sh <- c(1, 2, 0)

# Украшенный график
ordiplot(ord_mus, type = "n")
points(ord_mus, col = pal_col[env$Zone], pch = pal_sh[env$Site])

# ## Украшенный график с центроидами видов
ordiplot(ord_mus, type = "n")
points(ord_mus, col = pal_col[env$Zone], pch = pal_sh[env$Site])
text(ord_mus, display = "spec", cex = 0.9, col = "grey20")

# # Визуализация ординации в ggplot2
#
# ## Задание 2 -----------------------------------
#
# Используя данные, приведенные ниже, постройте
# график ординации при помощи `ggplot2`.
# Покажите точками --- мидий, цветом --- зону
# литорали, формой --- сайт, размером маркеров ---
# размер мидий.
# Добавьте текстом центроиды переменных.

# Координаты точек (мидий)
ord_mus_pt <- data.frame(env, scores(ord_mus, display = "sites"))
head(ord_mus_pt, 2)

# Координаты центроидов переменных (видов-симбионтов)
ord_mus_sp <- data.frame(scores(ord_mus, display = "species"))
ord_mus_sp$Species <- rownames(ord_mus_sp)
head(ord_mus_sp, 2)


# ## Решение: График с точками

# gg_ord_mus <-




# ## Решение: График с точками и центроидами видов-симбионтов

# gg_ord_mus_sp <-











# # Интерпретация ординации: envfit #############

ef <- envfit(ord_mus, env[, c("Zone", "Site", "L", "Age")])

ef$vectors

ef$factors


# ## График с векторами и центроидами, найденными `envfit()`

ordiplot(ord_mus, type = "n")
points(ord_mus, col = pal_col[env$Zone], pch = pal_sh[env$Site])
plot(ef)

# ## Для ggplot-графика удобно использовать вспомогательный пакет
# install.packages("devtools")
# devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan)
ord_mus_ef <- fortify(ef)
ord_mus_ef

# ## ggplot2 версия графика `envfit()` {.smaller}
gg_ord_mus +
  geom_segment(data = ord_mus_ef[ord_mus_ef$Type == "Vector", ],
               aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm"))) +
  geom_text(data = ord_mus_ef[ord_mus_ef$Type == "Vector", ],
            aes(x = NMDS1, y = NMDS2, label = Label, hjust = 1.1, vjust = 1)) +
  geom_text(data = ord_mus_ef[ord_mus_ef$Type == "Centroid", ],
            aes(x = NMDS1, y = NMDS2, label = Label, hjust = 1.1, vjust = 1))



# # Интерпретация ординации: ordisurf #########################

# ## График с поверхностью, найденной `ordisurf()`

par(mfrow = c(1, 2))
os_L <- ordisurf(ord_mus, env$L, method = "REML")
os_Age <- ordisurf(ord_mus, env$Age, method = "REML")
par(mfrow = c(1, 1))


# ## Интерпретация результатов `ordisurf()`

summary(os_L)

summary(os_Age)

# ## Для ggplot-графика понадобится добыть данные о контурах

fortify_ordisurf <- function(model) {
  # Fortifies an object of class ordisurf to produce
  # a dataframe of contour coordinates
  # for subsequent plotting in ggplot.
  xy <- expand.grid(x = model$grid$x, y = model$grid$y)
  xyz <- cbind(xy, c(model$grid$z))
  names(xyz) <- c("x", "y", "z")
  return(na.omit(xyz))
}

ord_mus_os <- fortify_ordisurf(os_Age)
head(ord_mus_os, 4)

# ## ggplot2 версия графика с поверхностью, найденной `ordisurf()`

ggplot(data = ord_mus_os, aes(x = x, y = y, z = z)) +
  stat_contour(aes(colour = ..level..)) +
  labs(x = "NMDS1", y = "NMDS2", colour = "Age")


# ## Финальный график
f_vect <- ord_mus_ef$Type == "Vector" & ord_mus_ef$Label == "L"


ggplot(data = ord_mus_pt, aes(x = NMDS1, y = NMDS2)) +
  stat_contour(data = ord_mus_os,
               aes(x = x, y = y, z = z, colour = ..level..),
               binwidth = 0.25) +
  geom_point(data = ord_mus_pt,
             aes(fill = Zone, shape = Site),
             alpha = 0.5, size = 3) +
  scale_shape_manual(values = c(21, 24, 22)) +
  geom_text(data = ord_mus_sp,
            aes(label = Species), size = 5) +
  geom_segment(data = ord_mus_ef[f_vect, ],
               colour = "blue", size = 1,
               aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm"))) +
  geom_text(data = ord_mus_ef[f_vect, ],
            colour = "blue", size = 6,
            aes(label = Label, hjust = 1.1, vjust = 0.7)) +
  guides(fill = guide_legend(override.aes = list(shape = 22, alpha = 1))) +
  coord_fixed() +
  labs(colour = "Age")



# # Самостоятельная работа ############################
#
# ## Задание 3 -----------------------------------
#
# Во всех примерах необходимо:
#
# 1. Разобраться с данными.
# 2. Построить ординацию объектов (описаний, проб и т.п.).
# 3. Визуализировать связь между полученной ординацией и параметрами среды.
# 4. Сделать выводы о наиболее важных факторах.
#
# **Источники данных**
#
# 1. Фауна Долгой губы (данные В.М.Хайтова).
# 2. растительные сообщества во франции La Mafragh (данные из работы de Belair et al. 1987, данные `mafragh`, пакет `ade4`).
# 3. Деревья на острове Barro Colorado (данные из работы Condit et al. (2002), данные `BCI`, пакет `vegan`).

