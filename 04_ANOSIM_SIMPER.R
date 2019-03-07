# ---
# title: "Модельные матрицы, ANOSIM и SIMPER"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Вадим Хайтов, Марина Варфоломеева
# ---
com <- read.csv("data/mussel_beds.csv", sep=';', header = T)
ascam <- read.csv("data/ASCAM.csv", sep=';', header = T)

str(com)



# ANOSIM: Analysis Of Similarity

## Задание
# - Постройте ординацию всех описаний датасета `com` (логарифмированные данные) в осях nMDS на основе матрицы Брея-Куртиса
# - Раскрасьте разными цветами точки, относящиеся к двум разным группам: "Large-dominated" и "Small-dominated"


library(vegan)
library(ggplot2)

log_com <- decostand(com[,-c(1:3)],  method = "log")

ord_log_com <- metaMDS(log_com, distance = "bray", k=2,  autotransform = F)

MDS <- data.frame(ord_log_com$points)

ggplot(MDS, aes(x = MDS1, y = MDS2, fill = com$Mussel_size)) +
  geom_point(shape = 21, size = 4) +
  scale_fill_manual(values = c("red", "blue")) + ggtitle(paste("Stress = ", round(ord_log_com$stress, 2))) + theme_bw()


ord_log_com$stress







# **Задание:**
#
# 1. Вычислите матрицу коэффициентов Брея-Куртиса на основе матрицы `log_com`
# 2. Разверните полученную матрицу в вектор.
# 3. На основе полученного вектора создайте вектор, содержащий ранги расстояний.

dist_com <- vegdist(log_com, method = "bray")
#
# write.table(as.data.frame(dist_com), "clipboard", sep = "\t", row.names = F)

unfold_dist_com <- as.vector(dist_com)

rank_dist_com <- rank(unfold_dist_com)




# **Задание:**
#
#   4. Создайте треугольную матрицу `dummy_dist`, той же размерности, что и матрица `dist_com`, в которой `0` будет с стоять в тех ячейках, которые соответствуют межгрупповым расстояниями, а   `1` -- внутригрупповым.

dummy_dist <- dist(as.numeric(com$Mussel_size))

dummy_dist <- ifelse(dummy_dist == 0, 0, 1)




# **Задание:**
#
# 5. Вычислите средние значения рангов внутри- и межгрупповых расстояний
# 6. Вычислите R-статистику

dists <- data.frame(rank = rank_dist_com, dummy = as.vector(dummy_dist))

library(dplyr)
library(doBy)

summaryBy(rank ~ dummy, FUN = mean, data = dists)

mean_dists <- dists %>% group_by(dummy) %>% summarize(rank_type = mean(rank))

n <- nrow(log_com)

R_glob <- (mean_dists$rank_type[2] - mean_dists$rank_type[1])/(n*(n-1)/4)

###





# **Задание:**
#
# 7. Напишите пользовательскую функцию (пусть она будет называться `R_perm`), которая пермутировала бы принадлежность каждого объекта к той или иной группе и вычисляла бы значение R-статистики для новой комбинации.
# 8. Используя функцию `for()` вычислите 10000 значений  R-статистики и запишите их в вектор.


R_perm <- function(comm = log_com, group = com$Mussel_size){
  require(vegan)
  dist_com <- vegdist(comm)
  rank_dist_com <- rank(dist_com)
  dummy_dist <- dist(sample(as.numeric(group))) #Перемешиваем группы
  dummy_dist <- ifelse(dummy_dist == 0, 0, 1)
  dists <- data.frame(rank = rank_dist_com, dummy = as.vector(dummy_dist))
  require(dplyr)
  mean_dists <- dists %>% group_by(dummy) %>% summarize(rank_type = mean(rank))
  n <- nrow(log_com)
  R_perm <- (mean_dists$rank_type[2] - mean_dists$rank_type[1])/(n * (n - 1)/4)
  R_perm
}


R_perm()

R_perms <- rep(NA, 10000)

for(i in 1:10000) R_perms[i] <- R_perm()

R_perms[10000] <- R_glob



# **Задание:**
#
# 9. Постройте частотную гистограмму, характеризующую распределение пермутационных оценок.
# 10. Нанесите на нее полученное значение $R_{global}$.
# 11. Вычислите уровень значимости.

R_perms <- data.frame(R_perms)

Pl_our <- ggplot(R_perms, aes(x = R_perms)) + geom_histogram() + geom_vline(xintercept = R_glob) + xlim(-0.2, 0.2)

mean(R_perms >= R_glob)




## Процедура ANOSIM в пакете `vegan`
com_anosim <- anosim(log_com,
           grouping = com$Mussel_size,
           permutations = 9999,
           distance = "bray")

## Задание
# Изучите структуру объекта `com_anosim` и постройте частотное распределение значений $R_{global}$, полученных при каждом акте пермутации

R_perms_vegan <- data.frame(vegan_R = com_anosim$perm)

Pl_vegan<- ggplot(R_perms_vegan, aes(x = vegan_R)) + geom_histogram()+ geom_vline(xintercept = R_glob) + xlim(-0.2, 0.2)


library(gridExtra)

grid.arrange(Pl_our, Pl_vegan)


com_anosim

plot(com_anosim)

## Ограничения (Assumptions) для применения ANOSIM

# Внутригрупповые расстояния (ранги)
plot(com_anosim, main = "Dissimilarity ranks \n between and within classes")





## Задание

# + Постройте ординацию в осях nMDS, раскрасив точки в разные цвета в зависимости от номера мидиевой банки
# + Проверьте гипотезу о различиях в структуре сообществ на разных банках
# + Проверьте условия применимости ANOSIM
# + Проведите попарное сравнение всех банок

ggplot(MDS, aes(x = MDS1, y = MDS2, fill = com$Bank)) +
  geom_point(shape = 21, size = 4) +
  scale_fill_manual(values = c("red", "blue", "green")) +
  labs(fill = "Mussel beds") + ggtitle(paste("Stress = ", round(ord_log_com$stress, 3), sep = " "))


bank_anosim <- anosim(log_com, grouping = com$Bank)

plot(bank_anosim)


## Модельные матрицы, тест Мантела и ANOSIM

n <- length(com$Bank)

m <- vegdist(as.numeric(com$Bank), method = "euclidean")
mm <- m
mm[m > 0] <- (1/sum(m > 0))/(n*(n-1)/4)
mm[m == 0] <- (-1/sum(m == 0))/(n*(n-1)/4)

mantel(vegdist(log_com), mm, method = "spearman")







# SIMPER: Similarity Percentages

## Какие признаки зависимой матрицы вносят наибольший вклад в формирование различий между группами?
log_com_simper <- simper(log_com, group = com$Mussel_size, permutations = 999)
summary (log_com_simper)


## Задание
# Выявитие виды, отвечающие за различия в сообществах разых банок

log_com_simper2 <- simper(log_com, group = com$Bank, permutations = 9999)
summary (log_com_simper2)









library(ade4)
data(package = "ade4")

data(chickenk)

chickenk$Mortality


chickenk$FarmStructure





patch <- read.table("data/mussel_patches.csv", sep = ";", header = TRUE)

str(patch)
