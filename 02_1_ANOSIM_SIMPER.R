# ---
# title: "Модельные матрицы, ANOSIM и SIMPER"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Вадим Хайтов, Марина Варфоломеева
# ---

# Тестирование гипотезы о соответствии ожидаемому паттерну: метод модельных матриц

## Пример: Динамика сообществ мидиевых банок {.columns-2}
# Существуют ли направленные многолетние изменения в размерной структуре поселений мидий и в структуре сообщества (Khaitov, 2013)?
com <- read.csv("data/mussel_beds.csv", sep=';', header = T)
ascam <- read.csv("data/ASCAM.csv", sep=';', header = T)

##Задание
# Рассмотрите многолетние изменения структуры сообщества и размерной структуры мидий на мидиевой банке Vor2
# Постройте рисунок, аналогичный приведенному на приведенном слайде
# Hint 1. Прологарифмируйте данные.
# Hint 2. Примените наиболее подходящий коэффициет для оценки расстояний между объектами.

## Градиентная модельная матрица
gradient_model <- vegdist(com$Year[com$Bank == "Vor2"], method="euclidian")
gradient_model

## Тестируем гипотезу о наличии градиента с помощью теста Мантела
dist_vor2_com <- vegdist(vor2_log_com, method = "bray")
dist_vor2_ascam <- vegdist(vor2_log_ascam, method = "euclidean")

### 1) Наличие градиента в структуре сообщества
mantel(dist_vor2_com, gradient_model)

### 2) Наличие градиента в размерной структуре мидий
mantel(dist_vor2_ascam, gradient_model)

## Прослеживается ли связь между размерной структурой мидий и структурой сообщества?

### Не самое правильное решение
mantel(dist_vor2_com, dist_vor2_ascam)

### Более корректное решение
mantel.partial(dist_vor2_com, dist_vor2_ascam, gradient_model)

## Задание
# 1. Выясните есть ли многолетний градиент в динамике размерной струтуры и структуры сообщества на банке Vor4.
# 2. Оцените связь между размерной структурой мидий и структурой сообщества.

## Циклическая модельная матрица
cycmod <- function(x){
  points <- data.frame(X=c(1:x), Y=c(1:x))
  for (i in 1:x) {
    points$X[i] <- cos(2*pi*(i-1)/x)
    points$Y[i] <- sin(2*pi*(i-1)/x)
  }
  return(points)
}

qplot(cycmod(nrow(mds_vor2_ascam))$X, cycmod(nrow(mds_vor2_ascam))$Y, xlab="X", ylab="Y", geom = "point", size = 4)

cycl_model <- round(vegdist(cycmod(nrow(mds_vor2_ascam)), method = "euclidean"))
cycl_model

## Выявляется ли циклическая составляющая в динамике размерной структуры?
mantel(dist_vor2_ascam, cycl_model)

## Более корректная оценка
mantel.partial(dist_vor2_ascam, cycl_model, gradient_model)


# ANOSIM: Analysis Of Similarity

## Задание
# - Постройте ординацию всех описаний датасета `com` (логарифмированные данные) в осях nMDS на основе матрицы Брея-Куртиса
# - Раскрасьте разными цветами точки, относящиеся к двум разным группам: "Large-dominated" и "Small-dominated"



## Процедура ANOSIM в пакете `vegan`
com_anosim <- anosim(log_com,
           grouping = com$Mussel_size,
           permutations = 999,
           distance = "bray")

## Задание
# Изучите структуру объекта `com_anosim` и постройте частотное распределение значений $R_{global}$, полученных при каждом акте пермутации



## Ограничения (Assumptions) для применения ANOSIM

# Внутригрупповые расстояния (ранги)
plot(com_anosim, main = "Dissimilarity ranks \n between and within classes")

## Задание

+ Постройте ординацию в осях nMDS, раскрасив точки в разные цвета в зависимости от номера мидиевой банки
+ Проверьте гипотезу о различиях в структуре сообществ на разных банках
+ Проверьте условия применимости ANOSIM
+ Проведите попарное сравнение всех банок

## Модельные матрицы и ANOSIM
m <- vegdist(as.numeric(com$Bank), method = "euclidean")
mm <- m
mm[m > 0] <- 1
mm[m == 0] <- 0
mantel(vegdist(log_com), mm, method = "pearson")




# SIMPER: Similarity Percentages

## Какие признаки зависимой матрицы вносят наибольший вклад в формирование различий между группами?
log_com_simper <- simper(log_com, group = com$Mussel_size, permutations = 999)
summary (log_com_simper)


## Задание
# Выявитие виды, отвечающие за различия в сообществах разых банок

