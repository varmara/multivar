library(vegan)
library(ggplot2)
library(ape)
library(dendextend)

# Пример на лекции
library(candisc)
data("Wolves")
dat <- Wolves[, 4:ncol(Wolves)]
grp <- Wolves[, 1]
# d <- vegdist(x = dat, method = "bray")
d <- dist(x = scale(dat), method = "euclidean")

# Домашнее задание
library(chemometrics)
data(hyptis)
dat <- hyptis[, 1:7]
grp <- hyptis[, 8:9]
d <- dist(x = scale(dat), method = "euclidean")

# Самостоятельное для лекции
data("iris")
Species <- substr(iris$Species, 0, 2)
rownames(iris) <- make.unique(Species)
set.seed(191231)
ids <- sample(nrow(iris), 50)
# ids <- 1:nrow(iris)
dat <- iris[ids, -5]
grp <- iris[ids, 5]
d <- dist(x = dat, method = "euclidean")


## Ординация
ord <- metaMDS(comm = d)
dfr <- data.frame(ord$points, grp)
ggplot(dfr, aes(x = MDS1, y = MDS2)) + geom_point(aes(colour = factor(grp)))

## Метод ближайшего соседа в R
hc_single <- hclust(d, method = "single")
ph_single <- as.phylo(hc_single)
plot(ph_single, type = "phylogram", cex = 0.7)
axisPhylo()

den_single <- as.dendrogram(hc_single)
plot(den_single)


## Метод отдаленного соседа в R
hc_compl <- hclust(d, method = "complete")
ph_compl <- as.phylo(hc_compl)
plot(ph_compl, type = "phylogram", cex = 0.7)
axisPhylo()
den_compl <- as.dendrogram(hc_compl)
plot(den_compl)

## Метод невзвешенного попарного среднего в R
hc_avg <- hclust(d, method = "average")
ph_avg <- as.phylo(hc_avg)
plot(ph_avg, type = "phylogram", cex = 0.7)
axisPhylo()
den_avg <- as.dendrogram(hc_avg)
plot(den_avg)

## Метод Варда в R
hc_w2 <-hclust(d, method = "ward.D2")
ph_w2 <- as.phylo(hc_w2)
plot(ph_w2, type = "phylogram", cex = 0.7)
axisPhylo()
den_w2 <- as.dendrogram(hc_w2)
plot(den_w2)

set.seed(395)
untang <- untangle_step_rotate_2side(den_compl, den_w2, print_times = F)
tanglegram(untang[[1]], untang[[2]],
           highlight_distinct_edges = FALSE,
           common_subtrees_color_lines = F,
           main = "Tanglegram",
           main_left = "Left tree",
           main_right = "Right tree",
           columns_width = c(8, 1, 8),
           margin_top = 3.2,
           margin_bottom = 2.5,
           margin_inner = 4,
           margin_outer = 0.5,
           lwd = 1.2, edge.lwd = 1.2,
           lab.cex = 1, cex_main = 1, cex_sub = 1, sub = "")

