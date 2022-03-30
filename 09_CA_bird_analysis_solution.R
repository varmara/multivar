

### CA вручную  для данных про птиц в Австралии


library(readxl)
birds <- read_excel(path = "data/macnally.xlsx")


bird <- birds[,-c(1,2)]


Ft <- sum(bird) #Общее количество найденных животных


f_ij <- bird #Частота встречи данного вида в данной пробе, то есть это первичные даные!


p_ij <- bird/Ft #вероятность встречи данного вида в данной пробе



q <- p_i %*% t(p_j) #вероятность встретить особь в данной пробе.


f_i <- apply(bird, MARGIN = 1, FUN = sum) #Общее количество особей в каждой пробе

p_i <- f_i/Ft #Вектор вероятностей встретить какую-либо особь в данной пробе


f_j <- apply(bird, MARGIN = 2, FUN = sum) #Общее количество особей в каждом виде

p_j <- f_j/Ft #Вектор вероятностей встретить особь данного вида





Q <- (f_ij*Ft - f_i %*% t(f_j))/(Ft*sqrt(f_i %*% t(f_j)))

Q <- (p_ij - p_i %*% t(p_j))/sqrt(p_i %*% t(p_j))


class(Q)

Q <- as.matrix(Q)


Inertia <- sum(Q^2)



U <- svd(Q)$u
D <- svd(Q)$d
V <- svd(Q)$v


A <- t(Q) %*% Q


eig_values <- eigen(A)$values #Собственные числа матрицы A
eig_vectors <- eigen(A)$vectors #Матрица собственных векторов для матрицы A





eig_values


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
  geom_text(label = rownames(bird)) +
  geom_hline(yintercept=0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  labs(x= "CA1", y = "CA2")






CA_species <- diag(p_j^(-1/2))%*% V[,1:2]


Pl_CA_sp <-
  ggplot(as.data.frame( CA_species), aes(x = V1, y = V2) )  +
  geom_hline(yintercept=0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  labs(x= "CA1", y = "CA2") +
  geom_text(label = names(bird))

Pl_CA_sp



bird_ca <- cca(bird)

summary(bird_ca)

plot(bird_ca, scaling = "site", display = "site")


bird_mds <- metaMDS(bird)

plot(bird_mds, display = "site")







rat <- read.csv("data/bolger1.csv")

rat2 <- rat[, -c(1:2)]



rat_ca <- cca(rat2)

plot(rat_ca, scaling = "species", display = "species")

plot(rat_ca, scaling = "site", display = "site")


rat_mds <- metaMDS(rat2)

plot(rat_mds, type = "t")

