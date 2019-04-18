
######################################################################
#МЕХАНИКА КОРРЕСПОНДЕНТНОГО И КАНОНИЧЕСКОГО КОРРЕСПОНДЕНТНОГО АНАЛИЗА#
######################################################################

#Вадим Хайтов, Марина Варфоломеева

# Проблемы PCA

library(readxl)
birds <- read_excel(path = "data/macnally.xlsx")

# имена переводим в нижний регистр
colnames(birds) <- tolower(colnames(birds))

# Проведите анализ главных компонент и визуализируйте его результаты для данных приведенных в датасете macnally.xlsx














# Механика Корреспондентного анализа

library(vegan)
data(mite)
data(mite.env)
data(mite.xy)
head(mite[ , 1:6], 2)
str(mite.xy)
str(mite.env)


mite_cca <- cca(mite)
summary(mite_cca)

sum(eigenvals(mite_cca))
bstick(mite_cca)

chisq.test(mite/sum(mite))

str(mite_cca)

mite_cca$rowsum #Это сумма по строкам, деленная на общее количестов чисел во сей таблице. Это вес строк.
apply(mite, 1, FUN = sum)/sum(mite)


mite_cca$colsum #Это сумма по столбцам, деленная на общее количестов чисел во сей таблице. Это вес колонок.
apply(mite, 2, FUN = sum)/sum(mite)







# Вычисляем матрицу Q


p_ij <- mite/sum(mite)

p_i <- apply(mite, 1, FUN = sum)/sum(mite) #Маргинальная сумма по строкам

p_j <- apply(mite, 2, FUN = sum)/sum(mite) #Маргинальная сумма по столбцам

Q <- (p_ij - p_i %*% t(p_j))/sqrt(p_i %*% t(p_j))


sum(Q^2)


#Или все то же самое в абсолютных значениях

f_ij <- mite

f <- sum(mite)

f_i <- apply(mite, 1, FUN = sum)
length(f_i)

f_j <- apply(mite, 2, FUN = sum)
length(f_j)

# Ожидаемые частоты для нулевой модели, то есть при условии, что все станции и все виды независимы

E <- f_i %*% t(f_j) / f


Q <- (f_ij*f - f_i %*% t(f_j))/(f*sqrt(f_i %*% t(f_j)))

class(Q)

Q <- as.matrix(Q)

Inertia <- sum(Q^2)

# Все то же самое в терминах наблюдаемые (O)  и ожидаемые (E) частоты

O <- mite

sum(((O - E)/sqrt(E) / sum(O))^2 * f) #та же самая инерция

U <- svd(Q)$u
D <- svd(Q)$d
V <- svd(Q)$v

dim(U)

dim(V)


# Матрицы U и V - ортонормальные матрицы. Следовательно инверсия должна быть равна транспонированой матрице
# проверим
round(U %*% t(U))

round(V %*% t(V))

round(as.vector(Q) - as.vector(U %*% diag(D) %*% t(V)))


# Связь SVD и собственных значений

D <- diag(D)

t(D) %*%  D #По главной диагонали будут стоять квадраты сингулярных чисел


round(t(Q) %*% Q -   V %*% t(D) %*% D %*% t(V))


eigen(t(Q) %*% Q)$values #Собственные значения для матрицы ковариации Q'Q

svd((t(Q) %*% Q))$d #Это те же собственные значения.

diag(D)^2 #Квадраты сингулярных чисел


det(t(Q) %*% Q) #Определитель равен нулю, так как одно из собственных значений равно нулю

# Важно!!! При SVD матрицы Q'Q всегда одно из сингулярных чисел (это то же что корень из собственного числа) будет равно 0. Поэтому в анализе используется V (с х с-1)

ncol(mite_cca$CA$v) #вместо 35!!!

plot(mite_cca, scaling = 1, display = "sites")



### CA  для данных про птиц в Австралии

