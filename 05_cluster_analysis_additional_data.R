
library(cluster.datasets)
library(vegan)
library(cluster)
library(pvclust)

# British butterfly -----
data("british.butterfly.appearance")
head(british.butterfly.appearance)
colSums(is.na(british.butterfly.appearance))


bri <- british.butterfly.appearance[complete.cases(british.butterfly.appearance), -1]
rownames(bri) <- british.butterfly.appearance$name[complete.cases(british.butterfly.appearance)]
# vegdist(bri[, -1], method = "altGower")
dist <- daisy(bri, metric = "gower")

hc <- hclust(dist, method = "average")
plot(hc)


my_gower <- function(x){
  daisy(x, metric = "gower")
}
cl_boot <- pvclust(bri, method.hclust = "complete",
                   nboot = 1000, method.dist = my_gower,
                   iseed = 42)
plot(cl_boot)


# Окисление-брожение у Candida (производство кислот)
data(candida.oxidation.fermentation)
colSums(is.na(candida.oxidation.fermentation))
candida <- candida.oxidation.fermentation[, -1]
rownames(candida) <- candida.oxidation.fermentation$name
can <- ifelse(candida == "+", 1, 0)
# dist <- vegdist(can, method = "euclidean")
dist <- vegdist(can, method = "jaccard")
ord <- metaMDS(can, distance = "jaccard", autotransform = FALSE)
plot(ord, type = "n")
points(ord, col = as.factor(rowSums(can)), pch = 19)
legend("topright", legend = levels(as.factor(rowSums(can))), col = 1:5, pch = 19)
hc <- hclust(dist)
plot(hc)
complete3 <- cutree(tree = hc, k = 3)
plot(silhouette(x = complete3, dist = dist))

my_jaccard <- function(x){
  vegdist(x, "jaccard")
}
cl_boot <- pvclust(can, method.hclust = "complete",
                   nboot = 1000, method.dist = my_jaccard,
                   iseed = 42)
plot(cl_boot)
