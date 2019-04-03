# title: "Анализ главных компонент"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Марина Варфоломеева, Вадим Хайтов


library(ggplot2)
library(cowplot)

# # Вспомним математику PCA на игрушечном примере =============
#
# ## Пример: Размеры медуз

# Данные о размерах медуз _Catostylus mosaicus_ (Lunn & McNeil 1991). Медузы собраны в реке Хоксбери (Новый Южный Уэльс, Австралия): часть --- на острове Дангар, другая --- в заливе Саламандер.

# ## Исходные данные

jelly <- read.delim("data/jellyfish.csv")

X_raw <- jelly[, 2:3]


gg <- ggplot(as.data.frame(X_raw), aes(x = width, y = length)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)
gg + coord_equal(xlim = c(0, 25), ylim = c(0, 25))

# ## Как работает PCA?

X <- scale(X_raw, center = TRUE, scale = FALSE) # Центрируем
A <- cov(X)    # Матрица ковариаций
E <- eigen(A)            # Спектральное разложение
U <- E$vectors           # Собственные векторы
Lambda <- E$values       # Собственные числа

# # Результаты работы PCA ==================================

# ## Собственные векторы == Факторные нагрузки
dimnames(U) <- list(colnames(X),paste0("PC", 1:ncol(X)))
U

# ## Собственные числа
Lambda

Explained <- Lambda/sum(Lambda)       # Доля объясненной изменчивости
Explained

# ## Факторные координаты
Y <- X %*% U # Координаты точек в новом пространстве

gg_rotated <- gg %+% as.data.frame(Y) +
  aes(x = PC1, y = PC2) +
  labs(title = "После вращения") +
  coord_equal(ylim = c(-10, 10))
gg_rotated


gg_rotated +
  aes(colour = jelly$location) +
  scale_color_brewer("Location", palette = "Set1") +
  labs(title = "Результаты PCA",
       x = paste0("PC1, ", round(Explained[1] * 100, 1), "%"),
       y = paste0("PC2, ", round(Explained[2] * 100, 1), "%"))


# # Восстановление исходных данных и их редукция ===========

# ## Восстанавливаем исходные данные с использованием разного числа главных компонент
X_back_full <- X %*% U %*% t(U)
gg_back_full <- gg %+% as.data.frame(X_back_full) +
  labs(x = "width", y = "length", title = "Восстановленные")

X_back_pc1 <- X %*% U[, 1] %*% t(U[, 1])
gg_back_pc1 <- gg_back_full %+% as.data.frame(X_back_pc1) +
  labs(title = "Редуцированные")

plot_grid(gg %+% as.data.frame(X) + ggtitle("Исходные"),
          gg_back_full, gg_back_pc1, align = "h", nrow = 1)



# # Действительно многомерные данные =======================

# ## Пример: Потребление белков в странах Европы с разными видами продуктов питания

# Данные из Weber, 1973

protein <- read.table(file="data/protein.csv", sep="\t", dec=".", header=TRUE)
protein$region <- factor(protein$region)
rownames(protein) <- protein$country
head(protein)

# ## Делаем PCA
library(vegan)
prot_pca <- rda(protein[, -c(1, 2)],
                scale = TRUE)
biplot(prot_pca)


# ## Разбираемся с результатами PCA
summary(prot_pca)

# # 1. Сколько компонент нужно оставить? ===================

eigenvals(prot_pca) # собственные числа

bstick(prot_pca) # ожидаемое по Broken Stick Model

screeplot(prot_pca, type = "lines", bstick = TRUE) # график собственных чисел

# # 2. Графики факторных нагрузок и ординации ==============

# Внимание! Координаты объектов или переменных можно
# получить в нескольких вариантах, отличающихся масштабом.
# От этого масштаба будет зависеть интерпретация.


op <- par(mfrow = c(1, 2))
# График факторных нагрузок
biplot(prot_pca, display = "species", scaling = "species")
# График факторных координат
biplot(prot_pca, display = "sites")
par(op)


# ## Те же самые графики можно построить в ggplot2
df_load <- as.data.frame(scores(prot_pca, display = "species",
                                choices = c(1, 2, 3), scaling = "species"))
# поправки для размещения подписей
df_load$hjust[df_load$PC1 >= 0] <- -0.1
df_load$hjust[df_load$PC1 < 0] <- 1
df_load$vjust[df_load$PC2 >= 0] <- -0.1
df_load$vjust[df_load$PC2 < 0] <- 1
library(grid) # для стрелочек
ar <- arrow(length = unit(0.25, "cm"))

p_load <- ggplot(df_load) +
  geom_text(aes(x = PC1, y = PC2, label = rownames(df_load)),
            size = 3, vjust = df_load$vjust, hjust = df_load$hjust) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               colour = "grey40", arrow = ar) +
  coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9))

## График ординации в ggplot
df_scores <- data.frame(protein[, 1:2],
  scores(prot_pca, display = "sites", choices = c(1, 2, 3), scaling = "sites"))

p_scores <- ggplot(df_scores, aes(x = PC1, y = PC2, colour = region)) +
  geom_text(aes(label = country)) +
  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))

plot_grid(p_load, p_scores, align = "h",
          rel_widths = c(0.36, 0.64))



# # 3. Интерпретация компонент =============================

scores(prot_pca, display = "species",
       choices = c(1, 2, 3), scaling = 0)



# # PCA и nMDS =============================================
#
# ## Задание 1
#
# - Постройте ординацию стран при помощи nMDS с использованием евклидова расстояния
# - Постройте график ординации
# - Нанесите при помощи envfit векторы изменения исходных переменных





# # Создание составных переменных при помощи PCA ===========

# ## Задание 2 ---------------------------------------------

# При помощи дисперсионного анализа проверьте, различается
# ли значение первой главной компоненты ("Мясо -- злаки и
# орехи") между разными регионами Европы

