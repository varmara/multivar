# ---
# title: "Анализ главных компонент"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Марина Варфоломеева, Вадим Хайтов
# ---

# ## Пример: Потребление белков в странах Европы с разными видами продуктов питания
# Данные из Weber, 1973

# ## Открываем данные
protein <- read.table(file="data/protein.csv", sep="\t", dec=".", header=TRUE)
protein$region <- factor(protein$region)
rownames(protein) <- protein$country
head(protein)

# ## Делаем PCA
library(vegan)
prot_pca <- rda(protein[, -c(1, 2)], scale = TRUE)
op <- par(mar = c(5, 4, 0, 2) + 0.1)
biplot(prot_pca)
par(op)


# ## Разбираемся с результатами PCA
summary(prot_pca)

# # 1. Сколько компонент нужно оставить?

eigenvals(prot_pca) # собственные числа

bstick(prot_pca) # ожидаемое по Broken Stick Model

screeplot(prot_pca, type = "lines", bstick = TRUE) # График собственных чисел


# # 2. Графики факторных нагрузок и ординации
# ## Параметр `scaling`
# Внимание! Координаты объектов или переменных можно получить в нескольких вариантах, отличающихся масштабом. От этого масштаба будет зависеть интерпретация.

# Графики в vegan
op <- par(mfrow = c(1, 2))
# График факторных нагрузок
biplot(prot_pca, display = "species", scaling = "species")
# График факторных координат
biplot(prot_pca, display = "sites")
par(op)

# ## Те же самые графики можно построить в ggplot2
library(ggplot2)
# Данные для графиков
df_load <- as.data.frame(scores(prot_pca, display = "species", choices = c(1, 2, 3), scaling = "species"))
# поправки для размещения подписей
df_load$hjust <- ifelse(df_load$PC1 >= 0, -0.1, 1)
df_load$vjust <- ifelse(df_load$PC2 >= 0, -0.1, 1)
library(grid) # для стрелочек
ar <- arrow(length = unit(0.25, "cm"))
## График нагрузок в ggplot
p_load <- ggplot(df_load) +
  geom_text(aes(x = PC1, y = PC2, label = rownames(df_load)),
            size = 3, vjust = df_load$vjust, hjust = df_load$hjust) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               colour = "grey40", arrow = ar) +
  coord_equal(xlim = c(-2, 2), ylim = c(-2, 2))

## График ординации в ggplot
df_scores <- data.frame(protein[, 1:2],
  scores(prot_pca, display = "sites", choices = c(1, 2, 3), scaling = "sites"))
p_scores <- ggplot(df_scores, aes(x = PC1, y = PC2, colour = region)) +
  geom_text(aes(label = country)) +
  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2))

# Все вместе
library(cowplot)
plot_grid(p_load, p_scores, align = "h", rel_widths = c(0.36, 0.64))


# # 3. Интерпретация компонент

# Факторные нагрузки оценивают вклады переменных в изменчивость по главной компоненте
scores(prot_pca, display = "species", choices = c(1, 2, 3), scaling = 0)




# # Создание составных переменных при помощи PCA


# ## При помощи дисперсионного анализа можно проверить, различается ли значение первой главной компоненты ("Мясо -- злаки и орехи") между разными регионами Европы

# Значения факторов (= факторные координаты)
df <- data.frame(region = protein$region,
  scores(prot_pca, display = "sites", choices = c(1, 2, 3), scaling = "sites"))
mod <- lm(PC1 ~ region, data = df)
anova(mod)

# ## Проверка условий применимости дисперсионного анализа
mod_diag <- fortify(mod)
res_p <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + geom_point(aes(size = .cooksd)) + geom_hline(yintercept = 0) + geom_smooth(method="loess", se=FALSE)
mean_val <- mean(mod_diag$.stdresid)
sd_val <- sd(mod_diag$.stdresid)
norm_p <- ggplot(mod_diag, aes(sample = .stdresid)) + geom_point(stat = "qq") + geom_abline(intercept = mean_val, slope = sd_val)
plot_grid(res_p, norm_p, ncol = 2, rel_widths = c(0.55, 0.45))


# ## График значений первой компоненты по регионам
df$region <- reorder(df$region, df$PC1, FUN=mean)
ggplot(df, aes(x = region, y = PC1, colour = region)) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = 1) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))

# ## Пост-хок тест
TukeyHSD(aov(mod))

