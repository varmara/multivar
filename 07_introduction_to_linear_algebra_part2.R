#' title: "Краткое введение в мир линейной алгебры. Часть 3"
#' subtitle: "Анализ и визуализация многомерных данных с использованием R"
#' author: Вадим Хайтов, Марина Варфоломеева



#' Во многих многомерных методах требуется найти оси максимального варьирования

set.seed(123456789)

x <- rnorm(1000, 50, 10)
y <- 10 * x + rnorm(1000, 0, 100)

XY <-data.frame(x = x, y = y)

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
  geom_abline(slope = tan(acos(U[1,1]) + acos(U[2,1]) + acos(U[2,2])), color = "blue")



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


B_reconstructed <- U[ ,1:5] %*% diag(D[1:5]) %*% t(V[ ,1:5])


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


# Задание: Поверните изображение на угол 30 и 90 градусов


angle <-  -30*pi/180 #Задаем угол поворота в радианах

# Вращающая матрица
Rot <- matrix(c(cos(angle), sin(angle),
                -sin(angle), cos(angle)), nrow = 2)

Image_rot <-   data.frame(t((Rot) %*% t(faceData_XY[, 1:2] )), value = faceData_XY[3]) #Надо заполнить пропуски

ggplot(Image_rot, aes(X1, X2)) + geom_point(aes(color = value), size = 5) + scale_fill_gradient(low = "darkblue",   high =  "white" )


# Задание: Проведите масштабирование полученного изображения

Scale <- matrix(c(3, 0, 0, 1), nrow = 2)

Image_trans <-   data.frame(t((Scale) %*% t(Image_rot[,1:2])), value = faceData_XY$value)

ggplot(Image_trans, aes(X1, X2)) + geom_point(aes(color = value), size = 5) + scale_fill_gradient(low = "darkblue",   high =  "white" ) + coord_equal()



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




# Задание про волков

# Здесь надо немного попреобразовывть объекты

Lambda <- as.matrix(read.csv("data/Eigen_wolv.csv"))

Lambda <- as.numeric(Lambda)

U <- as.matrix(read.csv("data/U_wolv.csv"))

cor_wol <- U %*% diag(Lambda) %*% solve(U) #Реконструируем матрицу корреляций

cor_wol <- as.dist(cor_wol)

plot(hclust(cor_wol))




#' ##Рекоструируем изображение, используя только часть информации

reduction <- function(x, U, D, V) U[,1:x] %*% diag(D[1:x]) %*% t(V[, 1:x])

gg_face(reduction(30, U_face, D_face, V_face))


# Помощь в самостоятельной работе


u1 <- as.matrix(read.csv("data/u1.csv"))
d1 <- as.matrix(read.csv("data/d1.csv"))
v1 <- as.matrix(read.csv("data/v1.csv"))

# Реконструкция изображения
gg_face(reduction(2, u1, d1, v1))



