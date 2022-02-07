#' title: "Краткое введение в мир линейной алгебры. Часть 3"
#' subtitle: "Анализ и визуализация многомерных данных с использованием R"
#' author: Вадим Хайтов, Марина Варфоломеева



#' Во многих многомерных методах требуется найти оси максимального варьирования

set.seed(123456789)
set.seed(123456789)

x1 <- rnorm(500, 30, 4)
y1 <- rnorm(500, 700, 50)
x2 <- rnorm(500, 40, 5)
y2 <- 10 * x2 + 200 + rnorm(500, 0, 100)

XY <-data.frame(x = c(x1, x2), y = c(y1, y2) )

qplot(XY$x, XY$y) + labs(x = "Переменная 1", y = "Переменная 2") +
  geom_point(aes(x = mean(x), y = mean(y)), size = 4, color = "yellow")


#' ## Нормализуем векторы

x_norm <- XY$x/sqrt(sum(XY$x)^2)
y_norm <- XY$y/sqrt(sum(XY$y)^2)


XY_norm <- data.frame(x = x_norm, y = y_norm)


ggplot(XY_norm , aes(x = x, y = y)) + geom_point() +
  geom_point(aes(x = mean(x), y = mean(y)), size = 4, color = "yellow")


#' ## Центрируем нормализованные векторы

XY_norm_cent <- as.data.frame(scale(XY_norm,  center = TRUE, scale = FALSE))

ggplot(XY_norm_cent , aes(x = x, y = y)) + geom_point() +
  geom_point(aes(x = mean(x), y = mean(y)), size = 4, color = "yellow")


#' ## Находим ковариационную матрицу {.smaller}

mXY_norm_cent <- as.matrix(XY_norm_cent)

Sxy_norm_cent <- t(mXY_norm_cent) %*% mXY_norm_cent/(nrow(mXY_norm_cent) - 1)

Sxy_norm_cent

#' ## Находим собственные числа и собственные векторы {.smaller}

eig <- eigen(Sxy_norm_cent) # Стандартная функция R для извлечения собственных чисел и собственных векторов

Lambda <- eig$values # Собственные числа

diag(Lambda)

U <- eig$vectors # Собственные векторы

U %*% diag(Lambda) %*% solve(U)



#' ## Стандартизованные собственные векторы {.smaller}

U_scaled <- U %*% sqrt(diag(Lambda)) #

(U %*% sqrt(diag(Lambda))) %*% t(U %*% sqrt(diag(Lambda)))


#' ## Рисуем собственные векторы {.smaller}

PC1 <- data.frame(x = c(mean(XY_norm_cent$x), U_scaled[1, 1]),
                  y = c(mean(XY_norm_cent$y),  U_scaled[2,1]))

PC2 <- data.frame(x = c(mean(XY_norm_cent$x),  U_scaled[1, 2]),
                  y = c(mean(XY_norm_cent$y),  U_scaled[2,2]))

ggplot(XY_norm_cent, aes(x = x, y = y)) + geom_point() +
  geom_point(aes(x = mean(x), y = mean(y)), size = 4, color = "yellow") +
  geom_line(data = PC1, aes(x = x, y = y), color = "yellow", size = 1.5)  +
  geom_line(data = PC2, aes(x = x, y = y), color = "yellow", size = 1.5) +
  coord_equal()



#' ## Рисуем главные оси {.smaller .columns-2}

ggplot(XY_norm_cent, aes(x = x, y = y)) + geom_point() +
  geom_point(aes(x = mean(x), y = mean(y)), size = 4, color = "yellow") +
  geom_line(data = PC1, aes(x = x, y = y), color = "yellow", size = 1.5)  +
  geom_line(data = PC2, aes(x = x, y = y), color = "yellow", size = 1.5) +
  coord_equal() + geom_abline(slope = tan(acos(U[1,1])), color = "blue") +
  geom_abline(slope = (U[2,2])/(U[1,2]), color = "blue")

#' ## Вращение осей {.smaller .columns-2}

#' Вращающая матрица

angle <- -1 * acos(U[1,1]) #Отрицательный угол, так как поворачиваем оси по часовой стрелке

Rot <- matrix(c(cos(angle), sin(angle),
                -sin(angle), cos(angle)), nrow = 2)
Rot


XY_norm_cent_rot <- as.data.frame(t(Rot %*% t(mXY_norm_cent)))

ggplot(XY_norm_cent, aes(x = x, y = y)) +
  geom_point(color = "gray") +
  geom_point(data = XY_norm_cent_rot, aes(x = V1, y = V2)) +
  labs(x = "Первая главная ось", y = "Вторая главная ось")


## Задание
# Исследуйте структуру матрицы *X*.


set.seed(123456789)

x1 <- c(rnorm(250, 30, 4), rnorm(250, 60, 4))
x2 <- rnorm(500, 70, 50)
x3 <- rnorm(500, 40, 5)
x4 <- c(rnorm(100, 10, 5), rnorm(100, 40, 5), rnorm(100, 70, 5), rnorm(200, 100, 5))
x5 <- c(rnorm(250, 50, 5), rnorm(250, 100, 5))


X <-data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5)




# *****************


#' ## Сингулярное разложение матрицы средствами R {.smaller}

set.seed(123456789)

B <- matrix(round(runif(50, 1, 5))  , byrow = T, ncol=5) #Некоторая матрица

SVD <- svd(B) #Сингулярное Разложение матрицы B с помощью функции svd()

V <- SVD$v #"Вспомогательная" матрица - левые сингулярные векторы

D <- SVD$d #Вектор сингулярных чисел

U <- SVD$u #"Вспомогательная" матрица - правые сингулярные векторы


U %*% diag(D) %*% t(V)

#' ## Задание
#'
#' Вычислите матрицу, которая получится при использовании только 1 и 2 сингулярного числа для матрицы, использованной на предыдущем слайде.
#' ##Решение


B_reconstructed <-


qplot(as.vector(B), as.vector(B_reconstructed)) + geom_abline()






#####################################################
# Работа с изобажениями, как с матричными объектами #
#####################################################

################## Поворот изображения ##########################3


load("data/face.rda")

faceData
dim(faceData)

library(reshape2)

faceData_XY <- melt(faceData) ## Переводим матрицу в два вектора координат и вектор значений интенсивности заливки

names(faceData_XY) <- c("X1", "X2", "value")


ggplot(faceData_XY, aes(X1, X2)) + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "darkblue",   high =  "white" ) + coord_equal()


################ Применение сингулярного разложения матриц  в сжатии изображений #########333

load("data/face.rda")

gg_face <- function(x) {
  library(reshape2)
  library(ggplot2)
  rotate <- function(x) t(apply(x, 2, rev))
  dd <- rotate(x)
  ddd <- melt(dd)
  ggplot(ddd, aes(Var1, Var2)) + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "darkblue",   high =  "white" ) + coord_equal()
}

gg_face(faceData)


SVD_face <- svd(faceData)

U_face <- SVD_face$u
D_face <- SVD_face$d
V_face <- SVD_face$v




#' ##Рекоструируем изображение, используя только часть информации

reduction <- function(x, U, D, V) U[,1:x] %*% diag(D[1:x]) %*% t(V[, 1:x])

gg_face(reduction(2, U_face, D_face, V_face))




### Самостоятельная работа ############################


# Задание про волков


# Восстановление изображения


# Сжатие изображения




