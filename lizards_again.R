#Lizard task again ----
library(readxl)
library(vegan)
library(ape)
library(dendextend)
library(ggplot2)
library(RColorBrewer)

# Функция для превращения лейблов в цвета
# (группы определяются по `n_chars` первых букв)
get_colours <- function(dend, n_chars, palette = "Dark2"){
  labs <- get_leaves_attr(dend, "label")
  group <- substr(labs, start = 0, stop = n_chars)
  group <- factor(group)
  cols <- brewer.pal(length(levels(group)), name = palette)[group]
  return(cols)
}

liz<-as.data.frame(read_excel('./data/Sleepy lizard.xlsx'))
str(liz)
liz$Treatment<-factor(liz$Treatment, levels=1:2, labels=c('N', 'D'))
liz$Habitat<-factor(liz$Habitat, levels=1:3, labels=c('N', 'A', 'P'))
liz$mix<-paste(liz$Treatment, liz$Habitat)
rownames(liz) <- make.unique(as.character(liz$mix))
log_liz <- decostand(liz[,c(13:18)], method = 'log')
dist_liz <- vegdist(log_liz, method  = "euclidean")

hc_single_liz <- hclust(dist_liz, method = "single")
ph_single_liz <- as.phylo(hc_single_liz)

hc_compl_liz <- hclust(dist_liz, method = "complete")
ph_compl_liz <- as.phylo(hc_compl_liz)

hc_avg_liz <- hclust(dist_liz, method = "average")
ph_avg_liz <- as.phylo(hc_avg_liz)

hc_w2_liz <-hclust(dist_liz, method = "ward.D2")
ph_w2_liz <- as.phylo(hc_w2_liz)

c_single_liz <- cophenetic(ph_single_liz)
c_compl_liz <- cophenetic(ph_compl_liz)
c_avg_liz <- cophenetic(ph_avg_liz)
c_w2_liz <- cophenetic(ph_w2_liz)

cor(dist_liz, as.dist(c_single_liz))
cor(dist_liz, as.dist(c_compl_liz))
cor(dist_liz, as.dist(c_avg_liz))
cor(dist_liz, as.dist(c_w2_liz))

plot(ph_avg_liz, type = "phylogram", cex = 0.7)
axisPhylo()
den_avg_liz <- as.dendrogram(hc_avg_liz)
plot(den_avg_liz)

ord_liz<-metaMDS(log_liz, distance = "euclidean",
                 autotransform = FALSE)
ord_liz$stress
dfr_liz <- data.frame(ord_liz$points, Group = liz$Treatment)
gg_liz <- ggplot(dfr_liz, aes(x = MDS1, y = MDS2)) +
  geom_point(aes(colour = Group))

cols <- get_colours(dend = den_avg_liz, n_chars = 2)
den_avg_c_liz_treat <- color_labels(dend = den_avg_liz, col = cols)
plot(den_avg_c_liz_treat, horiz = F)

cols <- get_colours(dend = den_avg_liz, n_chars = 3)
den_avg_c_liz_hab <- color_labels(dend = den_avg_liz, col = cols)
plot(den_avg_c_liz_hab, horiz = F)

legend("topright", legend = levels(as.factor(liz$mix)), col = brewer.pal(length(unique(liz$mix)), name = "Dark2"), pch = 19)

