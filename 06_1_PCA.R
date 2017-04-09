#' ---
#' title: "Анализ главных компонент"
#' subtitle: "Анализ и визуализация многомерных данных с использованием R"
#' author: Марина Варфоломеева, Вадим Хайтов

#' Данные о размерах медуз _Catostylus mosaicus_ (Lunn & McNeil 1991). Медузы собраны в реке Хоксбери (Новый Южный Уэльс, Австралия): часть --- на острове Дангар, другая --- в заливе Саламандер.

#' ## Сделаем PCA вручную

jelly <- read.delim("data/jellyfish.csv")

X_raw <- jelly[, 2:3]

library(ggplot2)
gg <- ggplot(as.data.frame(X_raw), aes(x = width, y = length)) +
  geom_point(size = 2) +
  coord_equal()
gg

#' ## Центрируем исходные данные
X <- scale(X_raw, center = TRUE, scale = FALSE)

# График центрированных данных
gg_centered <- gg %+% as.data.frame(X) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  coord_equal() +
  ggtitle("Центрированные данные")
gg_centered

#' ## PCA
A <- cov(X)    # Матрица ковариаций
E <- eigen(A)  # Спектральное разложение
E$vectors      # Собственные векторы
E$values       # Собственные числа

# Доля объясненной изменчивости
E$values/sum(E$values)
# Координаты точек в новом пространстве
Y <- X %*% E$vectors

#' ## Главные компоненты в исходном пространстве
# Коэф. угла наклона новых осей в старом пространстве
slope_1 <- E$vectors[1, 1] / E$vectors[2, 1]
slope_2 <- E$vectors[1, 2] / E$vectors[2, 2]

gg_centered +
  geom_abline(slope = slope_1, intercept = 0,
              linetype = "dashed",
              colour = "orangered", size = 1) +
  geom_abline(slope = slope_2, intercept = 0,
              linetype = "dashed",
              colour = "violet", size = 1)


#' ## Ординация точек в пространстве главных компонент
gg_rotated <- gg %+% as.data.frame(Y) +
  aes(x = V1, y = V2) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  coord_equal(ylim = c(-10, 10)) +
  labs(title = "После вращения", x = "PC1", y = "PC2")
gg_rotated


#' # Восстановление исходных данных и их редукция

#' ## Восстанавливаем __полные__ исходные данные
X_back_full <- X %*% E$vectors %*% t(E$vectors)

gg_back_full <- gg_centered %+%
  as.data.frame(X_back_full) +
  aes(x = V1, y = V2) +
  labs(x = "width", y = "length")

library(cowplot)
plot_grid(gg_centered,
          gg_back_full + ggtitle("Восстановленные"),
          align = "h")

#' ## Восстанавливаем __неполные__ исходные данные
X_back_pc1 <- X %*% E$vectors[, 1] %*% t(E$vectors[, 1])

gg_back_pc1 <- gg_centered %+% as.data.frame(X_back_pc1) +
  aes(x = V1, y = V2) +
  labs(x = "width", y = "length")

plot_grid(gg_centered,
          gg_back_full + ggtitle("Восстановленные"),
          gg_back_pc1 + ggtitle("Редуцированные"),
          nrow = 1, align = "h")


#' # Действительно многомерные данные
#' ## Пример: Потребление белков в странах Европы с разными видами продуктов питания
# Данные из Weber, 1973

protein <- read.table(file="data/protein.csv", sep="\t", dec=".", header=TRUE)
protein$region <- factor(protein$region)
rownames(protein) <- protein$country
head(protein)

#' ## Задание
#'
#' - Постройте ординацию стран при помощи nMDS с использованием евклидова расстояния
#' - Постройте график ординации
#' - Нанесите при помощи envfit векторы изменения исходных переменных
#'
#' ## Решение



#' # PCA: сколько компонент нужно оставить?

library(vegan)
prot_pca <- rda(protein[, -c(1, 2)], scale = FALSE)
summary(prot_pca)



#' ## Сколько компонент нужно оставить, если мы хотим редуцировать данные?

#' ## График собственных чисел

eigenvals(prot_pca) # собственные числа
bstick(prot_pca) # ожидаемое по Brocken Stick Model

screeplot(prot_pca, type = "lines", bstick = TRUE) # график собственных чисел


#' # Интерпретация компонент

scores(prot_pca, display = "species",
       choices = c(1, 2, 3), scaling = 0)



#' # Графики факторных нагрузок и ординации

op <- par(mfrow = c(1, 2))
# График факторных нагрузок
biplot(prot_pca, display = "species", scaling = 2)
# График факторных координат
biplot(prot_pca, display = "sites")
par(op)


#' ## Те же самые графики можно построить в ggplot

# График факторных нагрузок в ggplot
df_load <- as.data.frame(scores(prot_pca, display = "species",
                                choices = c(1, 2, 3), scaling = 2))
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
  scores(prot_pca, display = "sites", choices = c(1, 2, 3), scaling = 1))

p_scores <- ggplot(df_scores, aes(x = PC1, y = PC2, colour = region)) +
  geom_text(aes(label = country)) +
  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))


plot_grid(p_load, p_scores, align = "h",
          rel_widths = c(0.36, 0.64))
#


#' # Создание комплексных переменных при помощи PCA


df <- data.frame(region = protein$region,
  scores(prot_pca, display = "sites", choices = c(1, 2, 3), scaling = 1))
mod <- lm(PC1 ~ region, data = df)
anova(mod)


#' ## Проверка условий применимости дисперсионного анализа

mod_diag <- fortify(mod)
res_p <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + geom_point(aes(size = .cooksd)) + geom_hline(yintercept = 0) + geom_smooth(method="loess", se=FALSE)
mean_val <- mean(mod_diag$.stdresid)
sd_val <- sd(mod_diag$.stdresid)
norm_p <- ggplot(mod_diag, aes(sample = .stdresid)) + geom_point(stat = "qq") + geom_abline(intercept = mean_val, slope = sd_val)
plot_grid(res_p, norm_p, ncol = 2, rel_widths = c(0.55, 0.45))



#' ## График значений первой компоненты по регионам

df$region <- reorder(df$region, df$PC1, FUN=mean)
ggplot(df, aes(x = region, y = PC1, colour = region)) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = 1) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))


#' ## Пост-хок тест

TukeyHSD(aov(mod))
