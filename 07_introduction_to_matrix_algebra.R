#' title: "Краткое введение в мир матричной алгебры. Часть 2"
#' subtitle: "Анализ и визуализация многомерных данных с использованием R"
#' author: Вадим Хайтов, Марина Варфоломеева


#' ##Транспонирование матриц
A <- matrix(1:12, ncol = 3)
A

B <- t(A)
B

#' ##Сложение матриц

A + 4

A + A
#' Но! Нельзя складывать матрицы разных размеров
A + B

#' ##Простое умножение

A * 4

#' Простое умножение матрицы на вектор возможно только если число элементов в векторе равно числу строк в матрице

A * c(10, 11, 12, 13)

#' Длина вектора

Vec <- 1:5

sqrt(sum(Vec^2))

norm(t(Vec), type = "F") #Аналогчное решение

#' ##  Скалярное произведение векторов

#'
#' В доме есть следующие электроприборы.
#'
#' Электроприбор | Количество | Мощность (Вт) |
#' --------------|------------|---------------|
#' Чайник        | 2 шт       |       1200    |
#' Обогреватели  | 3 шт.      |    1300       |
#' Осушитель     | 1 шт.      |     1100 |
#' Стиральная машина | 1 шт.| 1500 |
#' Фен | 2 шт. | 800 |
#'
#' Вопрос: Какова будет суммарная мощность всех электроприборов, если их включить одновременно?
#'
#' ## Решение








#' ## Задание
#'
#' Выясните, являются ли ортогональными следующие векторы?

a <- c(0, 1)
b <- c(1, 0)
c <- c(1, 1)
d <- c(1, -1)

#' Аналитическое решение










#' ## Нормализованные векторы
#' ##Задание
#' Найдите нормализованный вектор для следующего вектора и определите его длину

Vec <- 1:5
Vec

#' ##Решение




#' ## Матричное умножение матрицы на вектор {.smaller .columns-2}

#' Пусть, есть матрица
A
A %*% c(10, 10, 10)

#' Но! если поменять местами множители, то будет ошибка
c(10, 10, 10) %*% A



#' ## Матричное умножение вектора на матрицу

c(10, 10, 10, 10) %*% A

A %*% c(10, 10, 10, 10)



#' ##Умножение матриц {.smaller .columns-2}
A

B

A %*% B

A %*% A



#'
#' ## Зачем это нужно?  {.smaller .columns-2}
#' ### Бытовой пример
#' Представим себе, что вы решили купить четыре товара, по следующим ценам
#'
#' Товар | Цена
#' ------|-----
#' Товар 1 | 10
#' Товар 2 | 20
#' Товар 3 | 30
#' Товар 4 | 40
#'
#'
#'
#' Прямых выходов на продавца у вас нет, но есть три посредника, которые выставляют следующие "накрутки" цен.
#'
#' Посредники  | Товар 1 | Товар 2 | Товар 3 | Товар 4
#' ------------|---------|---------|---------|--------
#' Посредник 1 | 0.1     | 0.15    | 0.05    | 0.05
#' Посредник 2 | 0.15    | 0.15    | 0.09    | 0.01
#' Посредник 3 | 0.2     | 0.05    | 0.1     | 0.1



# Какой из посредников выгоднее?
#' ## Решение

cost <- c()

retailer <- matrix(c(0.1, 0.15, 0.05, 0.05,
                     0.15, 0.15, 0.09, 0.01,
                     0.2, 0.05, 0.1, 0.1 ),  = TRUE, ncol = )



#' ## Матрицы позволяют преобразовывать системы векторов
#'
#' Начальная система расположения точек
#'


y = c(2,2,3,3,2,2,3,4,5,6,6,5,4,3,2)
x = c(2,3,4,5,6,7,7,7,6,5,4,3,2,2,2)

Image <- cbind((x), (y))

qplot(Image[,1], Image[,2] ) + geom_polygon(fill = "red") + coord_fixed()


#' Поворот изображения на заданный угол

angle <- 30*pi/180

Rot <- matrix(c(cos(angle), sin(angle),
                -sin(angle), cos(angle)), nrow = 2)

Image_trans <-   t((Rot) %*% t(Image))


qplot(Image_trans[,1], Image_trans[,2] ) +
  geom_polygon(fill = "red") + coord_fixed()









#' Масштабирующая матрица
Scale <- matrix(c(1, 0, 0, 2), nrow = 2)

Image_trans2 <-   t((Scale) %*% t(Image_trans))

qplot(Image_trans2[,1], Image_trans2[,2] ) +
  geom_polygon(fill = "red") + coord_fixed()


########################

# Еще преобразованя за счет матричных опреаций

Scale <- matrix(c(1, 0, 0.8, 1), nrow = 2)

Image_trans <- data.frame(t((Scale) %*% t(faceData_XY[ ,1:2])), faceData_XY[3])

ggplot(Image_trans, aes(X1, X2)) + geom_point(aes(color = value), size = 5) + scale_fill_gradient(low = "darkblue",   high =  "white" )






#' ## Ковариационная матрица

M <- matrix(c(1,2,3,4,5,5,2,1,2,5,2,1,3,5,4,6,8,4,0,2), ncol = 4)
M

#' Матрица центрированных значений
Cent_M <- scale(M, center = TRUE, scale = FALSE)
Cent_M

#' Вычислите ковариационную матрицу с помощью методов матричной алгебры и сравните ее с матрицей, полученной с помощью функции `cov()`

Cov_M <-


diag(Cov_M)

#' Сравним

apply(M, 2, FUN = function(x)sd(x)^2)

#' ## Вычисление матрицы  корреляций с помощью матричной алгебры {.smaller .columns-2}

Stand_M <- scale(M         )
Stand_M


# Вычисление вручную
Cor_M <-
Cor_M



cor(M) # Стандартная функция R



#' Во многих многомерных методах требуется найти оси максимального варьирования

set.seed(123456789)

x <- rnorm(1000, 50, 10)
y <- 10 * x + rnorm(1000, 0, 100)
XY <-data.frame(x = x, y = y)
qplot(XY$x, XY$y) + labs(x = "Переменная 1", y = "Переменная 2") +
  geom_point(aes(x = mean(x), y = mean(y)), size = 4, color = "yellow")


#' ## Нормализуем векторы {.smaller}

x_norm <- XY$x/sqrt(sum(XY$x)^2)
y_norm <- XY$y/sqrt(sum(XY$y)^2)


XY_norm <- data.frame(x = x_norm, y = y_norm)


ggplot(XY_norm , aes(x = x, y = y)) + geom_point() +
  geom_point(aes(x = mean(x), y = mean(y)), size = 4, color = "yellow")


#' ## Центрируем нормализованные векторы {.smaller}

XY_norm_cent <- as.data.frame(scale(XY_norm,          ))

ggplot(XY_norm_cent , aes(x = x, y = y)) + geom_point() +
  geom_point(aes(x = mean(x), y = mean(y)), size = 4, color = "yellow")


#' ## Находим ковариационную матрицу {.smaller}

mXY_norm_cent <- as.matrix(XY_norm_cent)

Sxy_norm_cent <-

Sxy_norm_cent

#' ## Находим собственные числа и собственные векторы {.smaller}

eig <- eigen(Sxy_norm_cent) # Стандартная функция R для извлечения собственных чисел и собственных векторов

Lambda <- eig$values # Собственные числа

Lambda

U <- eig$vectors # Собственные векторы

U

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
  geom_line(data = PC1, aes(x = x, y = y), color = "yellow", size = 1.5,  arrow = ar)  +
  geom_line(data = PC2, aes(x = x, y = y), color = "yellow", size = 1.5,  arrow = ar) +
  coord_equal()



#' ## Рисуем главные оси {.smaller .columns-2}

ggplot(XY_norm_cent, aes(x = x, y = y)) + geom_point() +
  geom_point(aes(x = mean(x), y = mean(y)), size = 4, color = "yellow") +
  geom_line(data = PC1, aes(x = x, y = y), color = "yellow", size = 1.5,  arrow = ar)  +
  geom_line(data = PC2, aes(x = x, y = y), color = "yellow", size = 1.5,  arrow = ar) +
  coord_equal() + geom_abline(slope = tan(acos(U[1,1])), color = "blue") +
  geom_abline(slope = tan(acos(U[1,1]) + acos(U[2,1]) + acos(U[2,2])), color = "blue")



#' ## Вращение осей {.smaller .columns-2}

#' Вращающая матрица

angle <- - acos(U[1,1]) #Отрицательный угол, так как поворачиваем оси по часовой стрелке

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









#####################################################
# Работа с изобажениями, как с матричными объектами #
#####################################################

################## Поворот изображения ##########################3


load("data/face.rda")

faceData

library(reshape2)

faceData_XY <- melt(faceData) ## Переводим матрицу в два вектора координат и вектор значений интенсивности заливки

ggplot(faceData_XY, aes(X1, X2)) + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "darkblue",   high =  "white" ) + coord_equal()


# Задание: Поверните изображение на угол 30 и 90 градусов


angle <-  30*pi/180 #Задаем угол поворота в радианах

# Вращающая матрица
Rot <- matrix(c(cos(angle), sin(angle),
                -sin(angle), cos(angle)), nrow = 2)

Image_rot <-   data.frame(t((Rot)  t(  )), faceData_XY[3]) #Надо заполнить пропуски

ggplot(Image_rot, aes(X1, X2)) + geom_point(aes(color = value), size = 5) + scale_fill_gradient(low = "darkblue",   high =  "white" )


# Задание: Проведите масштабирование полученного изображения





################ Применение сингулярного разложения матриц  в сжатии изображений #########333

load("data/face.rda")

gg_face <- function(x) {
  library(reshape)
  library(ggplot2)
  rotate <- function(x) t(apply(x, 2, rev))
  dd <- rotate(x)
  ddd <- melt(dd)
  ggplot(ddd, aes(X1, X2)) + geom_tile(aes(fill = value)) + scale_fill_gradient(low = "darkblue",   high =  "white" ) + coord_equal()
}

gg_face(faceData)



SVD_face <- svd(faceData)

U <- SVD_face$u
D <- SVD_face$d
V <- SVD_face$v

#' ##Рекоструируем изображение, используя только часть информации

reduction <- function(x) U[,1:x] %*% diag(D[1:x]) %*% t(V[, 1:x])
gg_face(reduction(23))



