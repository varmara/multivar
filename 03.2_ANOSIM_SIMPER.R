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

log_com <- decostand()

ord_log_com <- metaMDS(, distance = "bray", k=2)

MDS <- data.frame()

ggplot(MDS, aes(x = MDS1, y = MDS2, fill = )) +
  geom_point(shape = 21, size = 4) +
  scale_fill_manual() + ggtitle(paste("Stress = ", round(, 2))) + theme_bw()










# **Задание:**
#
# # 1. Вычислите матрицу коэффициентов Брея-Куртиса на основе матрицы `log_com`
# # 2. Разверните полученную матрицу в вектор.
# # 3. На основе полученного вектора создайте вектор, содержащий ранги расстояний.
#
dist_com <- vegdist( , method = "bray")
#
# write.table(as.data.frame(dist_com), "clipboard", sep = "\t", row.names = F)

unfold_dist_com <-


rank_dist_com <-




# **Задание:**
#
#   4. Создайте треугольную матрицу `dummy_dist`, той же размерности, что и матрица `dist_com`, в которой `0` будет с стоять в тех ячейках, которые соответствуют межгрупповым расстояниями, а   `1` -- внутригрупповым.

dummy_dist <- dist()

dummy_dist <- ifelse(dummy_dist == 0, 0, 1)




# **Задание:**
#
# 5. Вычислите средние значения рангов внутри- и межгрупповых расстояний
# 6. Вычислите R-статистику

dists <- data.frame(rank = rank_dist_com, dummy = as.vector(dummy_dist))

library(dplyr)


mean_dists <- dists %>% group_by() %>% summarize(rank_mean = )

n <- nrow(log_com)

R_glob <-


###





# **Задание:**
#
# 7. Напишите пользовательскую функцию (пусть она будет называться `R_perm`), которая пермутировала бы принадлежность каждого объекта к той или иной группе и вычисляла бы значение R-статистики для новой комбинации.
# 8. Используя функцию `for()` вычислите 10000 значений  R-статистики и запишите их в вектор.


R_perm <- function(comm = log_com, group = com$Mussel_size){
  require(vegan)

  ##
  ##

  dummy_dist <- dist(sample(as.numeric(group))) #Перемешиваем группы
  ##
  ##
  ##
  ##
  ##
  R_p
}


R_perm()



for(i in 1:10000) R_perms[i] <- R_perm(comm = log_com, group = com$Mussel_size)

R_perms[10000] <-



# **Задание:**
#
# 9. Постройте частотную гистограмму, характеризующую распределение пермутационных оценок.
# 10. Нанесите на нее полученное значение $R_{global}$.
# 11. Вычислите уровень значимости.

R_perms <- data.frame()

Pl_our <- ggplot(R_perms, aes(x = R_perms)) + geom_histogram() + geom_vline(xintercept = ) + xlim(-0.2, 0.2)


#P-value




## Процедура ANOSIM в пакете `vegan`
com_anosim <- anosim(log_com,
           grouping = com$Mussel_size,
           permutations = 9999,
           distance = "bray")

## Задание
# Изучите структуру объекта `com_anosim` и постройте частотное распределение значений $R_{global}$, полученных при каждом акте пермутации

R_perms_vegan <- data.frame(vegan_R = )

Pl_vegan<- ggplot(R_perms_vegan, aes(x = vegan_R)) + geom_histogram()+ geom_vline(xintercept = R_glob) + xlim(-0.2, 0.2)


library(gridExtra)

grid.arrange(Pl_our, Pl_vegan)



## Ограничения (Assumptions) для применения ANOSIM

# Внутригрупповые расстояния (ранги)
plot(com_anosim, main = "Dissimilarity ranks \n between and within classes")





## Задание

# + Постройте ординацию в осях nMDS, раскрасив точки в разные цвета в зависимости от номера мидиевой банки
# + Проверьте гипотезу о различиях в структуре сообществ на разных банках
# + Проверьте условия применимости ANOSIM
# + Проведите попарное сравнение всех банок

ggplot( , aes(x = MDS1, y = MDS2, fill = )) +
  geom_point(shape = 21, size = 4) +
  scale_fill_manual(values = c("red", "blue", "green")) +
  labs(fill = "Mussel beds") + ggtitle(paste("Stress = ", round(ord_log_com$stress, 3), sep = " "))


bank_anosim <- anosim(log_com, grouping = )

plot(bank_anosim)


# ## Модельные матрицы, тест Мантела и ANOSIM
#
# n <- length(com$Bank)
#
# m <- vegdist(as.numeric(com$Bank), method = "euclidean")
# mm <- m
# mm[m > 0] <- (1/sum(m > 0))/(n*(n-1)/4)
# mm[m == 0] <- (-1/sum(m == 0))/(n*(n-1)/4)
#
# mantel(vegdist(log_com), mm, method = "spearman")







# SIMPER: Similarity Percentages

## Какие признаки зависимой матрицы вносят наибольший вклад в формирование различий между группами?
log_com_simper <- simper(log_com, group = com$Mussel_size, permutations = 999)
summary (log_com_simper)


## Задание
# Выявитие виды, отвечающие за различия в сообществах разых банок

log_com_simper2 <- simper(log_com, group = com$Bank, permutations = 9999)
summary (log_com_simper2)




### Самостоятельная работа


