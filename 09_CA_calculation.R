
#######################################
# МЕХАНИКА КОРРЕСПОНДЕНТНОГО  АНАЛИЗА #
#######################################

#Вадим Хайтов, Марина Варфоломеева

# Проблемы PCA

library(readxl)
birds <- read_excel(path = "data/macnally.xlsx")

# имена переводим в нижний регистр
colnames(birds) <- tolower(colnames(birds))

# Проведите анализ главных компонент и визуализируйте его результаты для данных приведенных в датасете macnally.xlsx

#Код для анализа главных компонент

library(vegan)

bird_pca <- rda(birds[,-c(1:2)], scale = T)
screeplot(bird_pca, bstick = T)


# Код для вывода информации об информативности PC

summary(bird_pca)


plot(bird_pca, display = "sites")

# Код для построения биплота

biplot(bird_pca, scaling = "sites")

biplot(bird_pca, scaling = "species")

biplot(bird_pca, scaling = "species", display = "species")







# Механика Корреспондентного анализа

library(vegan)
data(mite)
data(mite.env)
data(mite.xy)
head(mite[ , 1:6], 2)
str(mite)


str(mite.xy)
str(mite.env)


mite_pca <- rda(mite, scaling = TRUE)

screeplot(mite_pca, bstick = T)

biplot(mite_pca,  scaling = "sites", type = "t")

mite$LCIL


plot(mite_pca,  display = "sites", type = "t")


mite_mds <- metaMDS(mite)

plot(mite_mds, display = "site")


mite_ca <- cca(mite)

# biplot(mite_ca)


screeplot(mite_ca, bstick = T)

plot(mite_ca)


#### Матрицы сопряженности #####

peas <- matrix(c(99, 42, 29, 13), byrow = T, ncol = 2)



Ft <- sum(peas)



f_i <- apply(peas, MARGIN = 1, FUN = sum)

f_j <- apply(peas, 2, FUN = sum)



p_i <- f_i / Ft #Вектор вероятностей для формы
p_j <- f_j / Ft #Вектор вероятностей для цвета




q <- p_i %*% t(p_j)




E <- q * Ft

O <- peas


sum((O-E)^2/E)

chisq.test(x = O, p = q, correct = F)


### CA Вручную ##############


Ft <- sum(mite)

f_ij <- mite #Частота встречи данного вида в данной пробе, то есть это первичные даные!

p_ij <- mite/Ft #вероятность встречи данного вида в данной пробе


Ft <- sum(mite) #Общее количество найденных животных

f_i <- apply(mite, MARGIN = 1, FUN = sum) #Общее количество особей в каждой пробе

p_i <- f_i/Ft #Вектор вероятностей встретить какую-либо особь в данной пробе

f_j <- apply(mite, MARGIN = 2, FUN = sum) #Общее количество особей в каждом виде

p_j <- f_j/Ft #Вектор вероятностей встретить особь данного вида


q <- p_i %*% t(p_j) #вероятность встретить особь в данной пробе.


E <- (p_i %*% t(p_j) * Ft)

O <- mite

Chi2 <- sum((O-E)^2/E)

Inertia <- Chi2/Ft




mite_cca <- cca(mite)
summary(mite_cca)


Q1 <- (p_ij - p_i %*% t(p_j))/sqrt(p_i %*% t(p_j))

#Та же матрица, вычисленная через частоты
Q <- (f_ij*Ft - f_i %*% t(f_j))/(Ft*sqrt(f_i %*% t(f_j)))


sum(Q^2)


Q <- as.matrix(Q)





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


f_ij <- mite #Частота встречи данного вида в данной пробе, то есть это первичные даные!

p_ij <- mite/Ft #вероятность встречи данного вида в данной пробе

q <- p_i %*% t(p_j) #вероятность встретить особь в данной пробе.

Ft <- sum(mite) #Общее количество найденных животных

f_i <- apply(mite, MARGIN = 1, FUN = sum) #Общее количество особей в каждой пробе

p_i <- f_i/Ft #Вектор вероятностей встретить какую-либо особь в данной пробе

f_j <- apply(mite, MARGIN = 2, FUN = sum) #Общее количество особей в каждом виде

p_j <- f_j/Ft #Вектор вероятностей встретить особь данного вида


Q <- (p_ij - p_i %*% t(p_j))/sqrt(p_i %*% t(p_j))



summary(mite_ca)

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


### Сингулярное разложеине матрицы сопряженности ######

U <- svd(Q)$u
D <- svd(Q)$d
V <- svd(Q)$v

dim(U)

dim(V)

round(D, 2)

str(D)


Qsvd <- U %*% diag(D) %*% t(V) #матрица "восстановленная" из "вспомогательных" матриц

round(sum(Q - Qsvd)) #разность между исходной и "восстановленной" матрицами


# Связь SVD и собственных значений

D <- diag(D)


dim(D)

round(t(Q) %*% Q -   V %*% t(D) %*% D %*% t(V))

A <- t(Q) %*% Q


eig_values <- eigen(A)$values #Собственные числа матрицы A
eig_vectors <- eigen(A)$vectors #Матрица собственных векторов для матрицы A


plot(eig_values, diag(D))

round(eig_values, 4)

eigen(t(Q) %*% Q)$values #Собственные значения для матрицы ковариации Q'Q

svd((t(Q) %*% Q))$d #Это те же собственные значения.

diag(D)^2 #Квадраты сингулярных чисел


plot(eigen(t(Q) %*% Q)$values, diag(D))

sum(eig_values)


Information <- data.frame(
  CA = 1:length(eig_values),
  Eigenval =round(eig_values, 5),
  Prop_Explained = round(eig_values/sum(eig_values), 5),
  Cumul_Prop=round(cumsum(eig_values/sum(eig_values)),5)
)




CA_samples <- diag(p_i^(-1/2))%*% U[,1:2]


library(ggplot2)
Pl_CA_st <-
  ggplot(as.data.frame(CA_samples), aes(x=V1, y=V2) ) +
  geom_text(label = rownames(mite)) +
  geom_hline(yintercept=0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  labs(x= "CA1", y = "CA2")


Pl_CA_st



CA_species <- diag(p_j^(-1/2))%*% V[,1:2]


Pl_CA_sp <-
  ggplot(as.data.frame( CA_species), aes(x = V1, y = V2) )  +
  geom_hline(yintercept=0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  labs(x= "CA1", y = "CA2") +
  geom_text(label = names(mite))

Pl_CA_sp

# Задание: Проведите корреспондентный анализ данных по птицам Австралии, используя "ручной" метод обработки









