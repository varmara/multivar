# ---
# title: "Кластерный анализ (Решения)"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Марина Варфоломеева, Вадим Хайтов
# ---


## Задание 1 -------------------------------------------------
# - Постройте ординацию nMDS данных о морфометрии волков и ирисов
# - Оцените качество ординации
# - Обоснуйте выбор коэффициента
# - Раскрасьте точки на ординации волков в зависимости от
# географического происхождения (`group`), а на ординации
# ирисов --- от вида (`Species`)

## Решение: Волки

library(vegan)
library(ggplot2); theme_set(theme_bw())
s_w <- scale(Wolves[, 4:ncol(Wolves)]) ## стандартизируем
ord_w <- metaMDS(comm = s_w, distance = "euclidean", autotransform = FALSE)
dfr_w <- data.frame(ord_w$points, Group = Wolves$group)
gg_w <- ggplot(dfr_w, aes(x = MDS1, y = MDS2)) + geom_point(aes(colour = Group))

gg_w

## Решение: Ирисы

ord_i <- metaMDS(comm = siris[, -5], distance = "euclidean",
                 autotransform = FALSE)
dfr_i <- data.frame(ord_i$points, Species = siris$Species)
gg_i <- ggplot(dfr_i, aes(x = MDS1, y = MDS2)) + geom_point(aes(colour = factor(Species)))

gg_i


## Задание 2 ------------------------------------------------
# Оцените для данных об ирисах при помощи кофенетической
# корреляции качество кластеризаций, полученных разными
# методами.
# Какой метод дает лучший результат?

d_i <- dist(x = siris[, -5], method = "euclidean")

hc_single_i <- hclust(d_i, method = "single")
ph_single_i <- as.phylo(hc_single_i)

hc_avg_i <- hclust(d_i, method = "average")
ph_avg_i <- as.phylo(hc_avg_i)

hc_compl_i <- hclust(d_i, method = "complete")
ph_compl_i <- as.phylo(hc_compl_i)

hc_w2_i <- hclust(d_i, method = "ward.D2")
ph_w2_i <- as.phylo(hc_w2_i)

c_single_i <- cophenetic(ph_single_i)
c_compl_i <- cophenetic(ph_compl_i)
c_avg_i <- cophenetic(ph_avg_i)
c_w2_i <- cophenetic(ph_w2_i)

cor(d_i, as.dist(c_single_i))
cor(d_i, as.dist(c_compl_i))
cor(d_i, as.dist(c_avg_i))
cor(d_i, as.dist(c_w2_i))


## Задание 3 ===============================================
# Постройте танглграмму для данных о морфометрии ирисов из
# дендрограмм, полученных методом ближайшего соседа и
# методом Варда.

den_single_i <- as.dendrogram(hc_single_i)
den_w2_i <- as.dendrogram(hc_w2_i)

set.seed(395)
untang_i <- untangle_step_rotate_2side(den_single_i,
                                       den_w2_i,
                                       print_times = F)

# танглграмма
tanglegram(untang_i[[1]], untang_i[[2]],
           highlight_distinct_edges = FALSE,
           common_subtrees_color_lines = F,
           main = "Tanglegram",
           main_left = "Left tree",
           main_right = "Right tree",
           columns_width = c(8, 1, 8),
           margin_top = 3.2, margin_bottom = 2.5,
           margin_inner = 4, margin_outer = 0.5,
           lwd = 1.2, edge.lwd = 1.2,
           lab.cex = 1, cex_main = 1)
