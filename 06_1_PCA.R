###########################
#Метод главных компонент
#
#В. Хайтов, М.Варфоломеева
###########################

library(ggplot2)
library(grid)
library(gridExtra)

theme_set(theme_bw(base_size = 10) + theme(legend.key = element_blank()))
update_geom_defaults("point", list(shape = 19, size = 4))

## Ортогональня матрица
A <- matrix(c( 0,  0, 10,
               0,  5, 0,
               40, 0, 0  ), ncol = 3)
A %*% t(A)

normA <-  matrix(c( 0,  0, 1,
                    0,  1, 0,
                    1,  0, 0  ), ncol = 3)


# Проверяем свойство ортогональной матрицы
solve(normA)

t(normA)


## Собственные числа
set.seed(12345)
# Создаем некоторую матрицу
Dat <- data.frame(Sp1 = round(rnorm(7, 150, 5)), Sp2 = round(rnorm(7, 150, 5)), Sp3 = round(rnorm(7, 150, 5)), Sp4 = round(rnorm(7, 150, 5)), Sp5 = round(rnorm(7, 150, 5)) )
rownames(Dat) <- c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5", "Sample6", "Sample7" )

Dat

# Стандартизируем значения
X <- scale(Dat)

A <- round(cor(Dat), 2)

round(t(X) %*% X / (nrow(X) - 1), 2)


EIG <- eigen(A)
L <- EIG$values #Вектор собственных чисел
V <- EIG$vectors #Матрица собственных векторов

diag(L)

t(V)

solve(V)

round(t(V) - solve(V)) #Свойство ортонормальных матриц

A


round(V %*% diag(L) %*% t(V), 2) #В точности матрица A





#Работаем с реальными данными
jelly <- read.delim("data/jellyfish.csv")

head(jelly, 10)


library(ggplot2)
theme_set(theme_bw(base_size = 20) + theme(legend.key = element_blank()))
update_geom_defaults("point", list(shape = 19, size = 4))
p_raw <- ggplot(jelly, aes(x = width, y = length)) +   geom_point() +   geom_hline(yintercept = 0) +   geom_vline(xintercept = 0) +   coord_equal()

p_raw


## Центрируем данные

jelly$c_width <- jelly$width - mean(jelly$width)
jelly$c_length <- jelly$length - mean(jelly$length)
jelly <- jelly[order(jelly$c_width), ]

p_centered <- ggplot(jelly, aes(x = c_width, y = c_length)) +   geom_point() +   geom_hline(yintercept = 0) +   geom_vline(xintercept = 0) +  coord_equal()

p_centered + ggtitle("Центр координат сместился")

head(jelly[, c(1, 4:5)], 10)

Covar <- cov(jelly[, c("c_width", "c_length")])
eigen_values <- eigen(Covar)$values
eigen_vectors <- eigen(Covar)$vectors
t(eigen_vectors)
solve(eigen_vectors)


#НАходим кординаты в новых осях

fincoord <- as.matrix(jelly[,4:5]) %*% eigen_vectors

fincoord <- as.data.frame(fincoord)

colnames(fincoord) <- c("PC1", "PC2")


p_rotated <- ggplot(fincoord, aes(x = PC1, y = PC2)) +   geom_point() +     geom_hline(yintercept = 0) +    geom_vline(xintercept = 0) +   coord_equal(ratio = 1, ylim = c(-10, 10))

p_rotated + ggtitle("После поворота осей")

round(cor(fincoord), 2) #Новые оси не коррелируют



## SVD

SVD <-svd(jelly[,4:5])
U <- SVD$u
V <- SVD$v
D <- SVD$d

##############################
# Все одинаково! PCA - это частный случай SVD
#############################

qplot(fincoord$PC1, U %*% diag(D) [,1], geom = "point") + labs(x="PC1")

eigen_vectors

V


## Анализ многомерных данных
protein <- read.table(file="data/protein.csv", sep="\t", dec=".", header=TRUE)
protein$region <- factor(protein$region)
rownames(protein) <- protein$country
head(protein)

## Ваш код для построения орлинации MDS
library(vegan)

ord_nmds <- metaMDS(protein[, 3:ncol(protein)], distance = "euclidean", trace = 0)

plot(ord_nmds, display = "site", type = "t", cex = 0.8)


ef <- envfit(ord_nmds, protein[, 3:ncol(protein)])

plot(ord_nmds, display = "site", type = "t", cex = 0.8)
plot(ef)



## Проводим PCA
prcomp(protein[, -c(1, 2)])

A <- cor(protein[, -c(1, 2)])

eigen(A)$values


prot_pca <- rda(protein[, -c(1, 2)], scale = TRUE)

summary(prot_pca)


EV <- eigenvals(prot_pca) # собственные числа

EV/sum(EV)


bstick(prot_pca) # ожидаемое по Brocken Stick Model

screeplot(prot_pca, type = "lines", bstick = TRUE) # график собственных чисел

# Данные для ggplot
eig <- data.frame(pc = factor(1:length(eigenvals(prot_pca))),
           eigenval = as.vector(eigenvals(prot_pca)),
           bstick = bstick(prot_pca))
# График
eig_p_kaiser <- ggplot(eig, aes(x = pc, y = eigenval)) +
  geom_line(aes(group = 1), colour = "red") + geom_point(colour = "red") +
  geom_hline(yintercept = mean(eigenvals(prot_pca)), colour = "gray70") +
  labs(x = "Компоненты", y = "Собственные числа")

# график с brocken stick model
eig_p_bstick <- eig_p_kaiser +
  geom_line(aes(x = pc, y = bstick, group = 1)) +
  geom_point(aes(x = pc, y = bstick))

eig_p_bstick



biplot(prot_pca, display = "species", scaling = 2)

biplot(prot_pca, display = "species", scaling = 1)


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
            size = 5, vjust = df_load$vjust, hjust = df_load$hjust) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               colour = "grey40", arrow = ar) +
  coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9))
p_load


biplot(prot_pca, display = "sites")

# Значения факторов (= факторные координаты)
head(scores(prot_pca, display = "sites", choices = c(1, 2, 3), scaling = 1))

df_scores <- data.frame(protein[, 1:2],
  scores(prot_pca, display = "sites", choices = c(1, 2, 3), scaling = 1))

round(cor(df_scores$PC3, df_scores$PC2 ), 2)



p_scores <- ggplot(df_scores, aes(x = PC1, y = PC2, colour = region)) + geom_point() +
  geom_text(aes(label = country), hjust = 1, vjust = -1) +
  coord_equal(xlim = c(-1.4, 1.4), ylim = c(-1.4, 1.4))
p_scores

grid.arrange(p_load, p_scores, ncol = 2, widths = c(0.42, 0.58))


##Используем комплексную переменную для ANOVA

# Значения факторов (= факторные координаты)
df <- data.frame(region = protein$region,
  scores(prot_pca, display = "sites", choices = c(1, 2, 3), scaling = 1))
mod <- lm(PC1 ~ region, data = df)
anova(mod)

mod_diag <- fortify(mod)

library(gridExtra)
res_p <- ggplot(data = mod_diag, aes(x = .fitted, y = .stdresid)) + geom_point(aes(size = .cooksd)) + geom_hline(yintercept = 0) + geom_smooth(method="loess", se=FALSE)
mean_val <- mean(mod_diag$.stdresid)
sd_val <- sd(mod_diag$.stdresid)
norm_p <- ggplot(mod_diag, aes(sample = .stdresid)) + geom_point(stat = "qq") + geom_abline(intercept = mean_val, slope = sd_val)
grid.arrange(res_p, norm_p, ncol = 2, widths = c(0.55, 0.45))

## Рисуем средние для каждого региона
df$region <- reorder(df$region, df$PC1, FUN=mean)
ggplot(df, aes(x = region, y = PC1, colour = region)) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = 1) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))

## Постхоки
TukeyHSD(aov(mod))

