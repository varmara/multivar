
######################################################################
#МЕХАНИКА КОРРЕСПОНДЕНТНОГО И КАНОНИЧЕСКОГО КОРРЕСПОНДЕНТНОГО АНАЛИЗА#
######################################################################

#Вадим Хайтов, Марина Варфоломеева

# Часть 1.
# Механика Корреспондентного анализа


ibrary(vegan)
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



#####################################################
# Механика Канонического корреспондентного анализа  #
#####################################################

# Просто повтряем, что было раньше в СА

data(mite)

data("mite.env")

f_ij <- mite #Частота встречи данного вида в данной пробе, то есть это первичные даные!

p_ij <- mite/sum(mite) #вероятность встретить особь в данной пробе.

Ft <- sum(mite) #Общее количество найденных животных

f_i <- apply(mite, 1, FUN = sum) #Общее количество особей в каждой пробе

p_i <- f_i/Ft #Вектор вероятностей встретить какую-либо особь в данной пробе

f_j <- apply(mite, 2, FUN = sum) #Общее количество особей в каждом виде

p_j <- f_j/Ft #Вектор вероятностей встретить особь данного вида


Q <- (f_ij*Ft - f_i %*% t(f_j))/(Ft*sqrt(f_i %*% t(f_j))) #Матрица вкладов, вычисленная через частоты
Q <- as.matrix(Q)


# SVD матрицы Q
U <- svd(Q)$u
D <- diag(svd(Q)$d)
V <- svd(Q)$v

Inertia_total <- sum(D^2) #Общая инерция в системе
CA_number <- sum(round(D, 2) !=0) #Количество главныю осей в CA. Обратите внимание на то, что их на одну меньше, чем исходных колонок в матрице Q

############## Теперь переходим к CCA #############

# Находим матрицу предсказанных значений

X <- model.matrix(~SubsDens + WatrCont + Substrate + Topo, data =  mite.env) #Модельная матрица

#Матрица коэффициентов
betas <- solve(t(X) %*% diag(p_i) %*% X) %*% (t(X) %*% diag(p_i)^(1/2) %*% Q)
dim(betas)


Q_pred <- diag(p_i)^(1/2) %*% X %*% betas #Матрица предсказанных значенй

U_pred <- svd(Q_pred)$u
D_pred <- diag(svd(Q_pred)$d)
V_pred <- svd(Q_pred)$v

round(diag(D_pred), 2)

Inertia_constrained <- sum(D_pred^2) #Инерция в ограниченной ординации
CCA_number <- sum(round(D_pred, 2) !=0) #Количество Канонических осей в СCA. Обратите внимание, что их ровно столько же, сколько колонок в модельной матрце X (без учета интерцепта).

Inertia_constrained / Inertia_total #доля общей инерции связанной с предикторами



# Матрица остатков
Q_resid <- Q - Q_pred

U_res <- svd(Q_resid)$u
D_res <- diag(svd(Q_resid)$d)
V_res <- svd(Q_resid)$v

Inertia_unconstrained <- sum(D_res^2) #Инерция ординации остатков
CA_unconstrained_number <- sum(round(D_res, 2) !=0) # Обратите внимение, что их столько же сколько и главных осей в CA

Inertia_unconstrained / Inertia_total #доля общей инерции связанной с остатками

Inertia_unconstrained + Inertia_constrained #Сумма равна общей инерции

Inertia_total


#Сравним квадраты сингулярных чисел (то есть собственные значения матриц ковариаций)

round(data.frame(eigen_CA = diag(D)^2, eigen_CCA_constrained = diag(D_pred)^2, eigen_CCA_unconstrained = diag(D_res)^2), 4)

# Обратите внимание, что они НЕ равны друг другу. То есть CA, constrained CCA и unconstrained CCA - это три разные ординации



############################
# Ординация проб в ограниченных осях




#Координаты проб в канонических осях полученные вручную

constr_CA_samples <- diag(p_i^(-1/2))%*% U_pred


mite_cca <- cca(mite ~ SubsDens + WatrCont + Substrate + Topo, data =  mite.env)

#Координаты проб в канонических осях по версии cca()
mite_cca$CCA$u


plot(mite_cca$CCA$u[,1], constr_CA_samples[,1]) #Это одни и те же числа

# Ординация в неканонических осях

#Координаты проб в неканонических осях полученные вручную

unconstr_CA_samples <- diag(p_i^(-1/2))%*% U_res

#Координаты проб в канонических осях по версии cca()

cca_ord_unconstr <- mite_cca$CA$u



plot(unconstr_CA_samples[,1], cca_ord_unconstr[,1]) #Это одни и те же числа


##Ординация видов в каннических (constrained) осях

#Координаты видов канонических осях полученные вручную

constr_CA_species <- diag(p_j^(-1/2))%*% V_pred

#Координаты видов в канонических осях по версии cca()

cca_sp_constr <- mite_cca$CCA$v


plot(constr_CA_species [,1], cca_sp_constr[,1]) #Это одни и те же числа



