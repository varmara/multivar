###########################################
# Краткое введение в мир матричной алгебры
###########################################

#Анализ и визуализация многомерных данных с использованием R"
# Вадим Хайтов, Марина Варфоломеева


# Транспонирование матриц
A <- matrix(1:12, ncol = 3)

B <- t(A)



##Сложение матриц
Large <- data.frame(Sp1 = round(rnorm(5, 10, 2)), Sp2 = round(rnorm(5, 10, 3)), Sp3 = round(rnorm(5, 10, 2)))

rownames(Large) <- c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5" )

Small <- data.frame(Sp1 = round(rnorm(5, 50, 5)), Sp2 = round(rnorm(5, 50, 5)), Sp3 = round(rnorm(5, 50, 5)))

rownames(Small) <- c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5" )


##Две матрицы
Large
Small

## Сложение двух матриц
Large + Small

## Простое умножение
A * 4

## Простое умножение на вектор
A * c(10, 11, 12, 13)


## Пример простого умножениея матрицы на вектор
Rpocessed_portion <- c(1, 1, 1/2, 1/3, 1/4)
Processed_Factor <- 1/Rpocessed_portion

Small * Processed_Factor

## Находим номализованный вектор
Vec <- 1:5


## Скалярное произвеение векторов

N <- c(20, 40, 32, 45, 80, 50, 10)
Fert <- c( 0,  0,   1,   2,   2,   0,   0)

t(N) %*% (Fert)




## Простейшая демографическая модель
T1 <- c(20, 40, 32, 45, 80, 50, 10)
Age <- c("0", "1-10", "11-20", "21-35", "36-45", "46-55", "56-65")
Pop <- data.frame(Age, T1)

Lesl <- matrix(
c( 0,  0,   1,   2,   2,   0,   0,
  0.6, 0,   0,   0,   0,   0,   0,
   0,  0.7, 0,   0,   0,   0,   0,
   0,  0,   0.8, 0,   0,   0,   0,
   0,  0,   0,   0.7, 0,   0,   0,
   0,  0,   0,   0,   0.6, 0,   0,
   0, 0,    0,   0,   0,  0.2, 0  ),
byrow = T,
ncol = 7)

# Последовательно преображуем демографические вектора с помощью матрицы Лесли
Pop$T2 <- as.vector( Lesl %*% (Pop$T1 ))
Pop$T3 <- as.vector( Lesl %*% (Pop$T2 ))
Pop$T4 <- as.vector( Lesl %*% (Pop$T3 ))
Pop$T5 <- as.vector( Lesl %*% (Pop$T4 ))
Pop$T6 <- as.vector( Lesl %*% (Pop$T5 ))
Pop$T7 <- as.vector( Lesl %*% (Pop$T6 ))
Pop$T8 <- as.vector( Lesl %*% (Pop$T7 ))
Pop$T9 <- as.vector( Lesl %*% (Pop$T8 ))
Pop$T10 <- as.vector( Lesl %*% (Pop$T9 ))


## Визуализация
library(ggplot2)
library(reshape)
Pop2 <- melt(Pop)
ggplot(Pop2, aes(x=Age, y = value)) + geom_bar(stat = "identity") + facet_wrap(~variable, ncol = 2) + labs(x = "Возраст", y = "Число особей")

## Свойства матричных произведений
B <- matrix(1:24, ncol = 4)
C <- matrix(1:12, ncol = 3)

B %*% C

C %*% B

C %*% t(C)

t(C) %*% C

B <- matrix(1:9, ncol = 3)
C <- matrix(11:19, ncol = 3)

B %*%  C

C %*% B

t(B %*% C)

t(C) %*% t(B)

B %*% t(B)

t(B) %*% B


## Инверсия матрицы

X <- matrix(c(seq(1, 8),10), nrow = 3, byrow = T)


## Определитель матрицы
det(X)

## Инверсия матрицы
solve(X)


## Получаем единичную матрицу
round(solve(X) %*% X )

## Решаем систему линейных уравнений
Coef <- matrix(c(1 , 2 , 3 ,
         4 , 5 , 6 ,
         7 , 8 , 10), byrow = T, ncol = 3)
Val <- c(2,4,10)

solve(Coef) %*% Val


## Подбираем коэффициенты линейной регрессии вручную
data(cars)
Mod <- lm(dist ~ speed, data = cars)
coefficients(Mod)

## ------------------------------------------------------------------------
X <- data.frame(Int = 1, x = cars$speed)
X <- as.matrix(X)

y <- cars$dist
betas <- solve(t(X) %*% X) %*% (t(X) %*%y)

plot(X %*% betas, fitted(Mod))



## SVD
set.seed(12345)
B <- matrix(round(runif(50, 1, 5))  , byrow = T, ncol=5) #Некоторая матрица
SVD <- svd(B) #Сингулярное Разложение матрицы B с помощью функции svd()
V <- SVD$v #"Вспомогательная" матрица - левые сингулярные вектора
D <- SVD$d #Вектор сингулярных чисел
U <- SVD$u #"Вспомогательная" матрица - правые сингулярные вектора

#Получаем исходную матрицу из трех "вспомогательных"
U %*% diag(D) %*% t(V)


# Код для поиска редуцированной матрицы




#  Подобие исходной и редуцированной матрицы
library(ggplot2)
Dat <- data.frame(Init = rep(as.vector(B), 4), SingValue = rep(2:5, each = length(as.vector(B))), Calc = c(as.vector((U[,1:2] %*% diag(D[1:2]) %*% t(V[,1:2]))), as.vector((U[,1:3] %*% diag(D[1:3]) %*% t(V[,1:3]))), as.vector((U[,1:4] %*% diag(D[1:4]) %*% t(V[,1:4]))), as.vector((U[,1:5] %*% diag(D[1:5]) %*% t(V[,1:5])))))

ggplot(Dat, aes(x = Init, y = Calc)) + geom_point(size = 2) + labs(x = "значения в исходной матрицы", y = "значения в редуцированной матрице") + facet_wrap(~SingValue) + geom_abline(slope = 1)


## Пример с SVD для обработки изображений
load("data/face.rda")

faceData

#Вспомогательная функция
gg_face <- function(x) {
  library(reshape)
  library(ggplot2)
    rotate <- function(x) t(apply(x, 2, rev))
  dd <- rotate(x)
  ddd <- melt(dd)
  ggplot(ddd, aes(X1, X2)) + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "darkblue",   high =  "white" )
}

gg_face(faceData)


## Проводим SVD для матрицы faceData
SVD_face <- svd(faceData)

U <- SVD_face$u
D <- SVD_face$d
V <- SVD_face$v

reduction <- function(x) U[,1:x] %*% diag(D[1:x]) %*% t(V[, 1:x])

gg_face(reduction(4))

