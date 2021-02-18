# Анализ связи между наборами данных
# Вадим Хайов
# Многомерные методы на R

# Пакеты
library(vegan)
library(ggplot2)


# Загружаем данные
data(varespec)
data(varechem)

head(varespec)
str(varespec)


head(varechem)
str(varechem)


sum(is.na(varechem))
sum(is.na(varespec))


# Код для построения ординаци в осях nMDS

stressplot()


scores()

mds_points <- as.data.frame(scores())

ggplot(mds_points, )) + geom_point(aes(), size = 4) + scale_color_gradient(low = "yellow", high = "red") + theme_bw() + theme(legend.position = "bottom") + labs(color = "Концентрация алюминия") + ggtitle(paste("Stress = ", ) ))



# Применяем функцию envfit()
env_fit <- envfit()


# Визуализация результатов
ordiplot(veg_ord, display = "sites")
plot(env_fit)


# Анализ связи с переменными c помощью функции `ordisurf()`


ordiplot(veg_ord, display = "sites")

ordisurf( veg_ord, varechem$Al,
         add = TRUE, col="blue", method = "REML")
ordisurf(veg_ord,varechem$Mn,
         add = TRUE, col="green")



# Задание: Отразите связь ординации растительности со значениями концентрации гумуса.





# Вычисление мантеловской корреляции
dist_com <- vegdist(varespec, method = "bray")
dist_chem <- vegdist(varechem, method = "euclidean")

x <- as.vector(dist_com)
y <- as.vector(dist_chem)

qplot(x, y) + geom_smooth(se = F, method = "lm")





xy <- data.frame (x, y)
mant <- ggplot(xy, aes(x=x, y=y))
mant + geom_point(size=3) + xlab ("Biological dissimilarity") + ylab ("Chemical dissimilarity") + annotate("text", x=0.25, y=0.35, label=paste("Rspearmen =", R, sep=" ")) + theme_bw() + geom_smooth(method = "lm", se = FALSE)



R <- round(cor(x, y, method = "pearson"), 3)


cor.test(x, y, method = "pearson") # Это неправильное действие! Так делать нельзя!








## Пермутационный метод

set.seed(12345)

male <- rnorm(100, 130, 5)
female <- rnorm(100, 129,5)

t.test(male, female)




SE_m <- sd(male) / sqrt(length(male))
SE_f <- sd(female) / sqrt(length(female))

t_initial <- (mean(male) - mean(female))/sqrt(SE_m^2 + SE_f^2)


f <- female
m <- male

num_perm <- sample(1:100,1)
order_m <- sample (1:100, num_perm)
order_f <- sample (1:100, num_perm)

f[order_f] <- male[order_f]
m[order_m] <- female[order_f]

SE_m <- sd(m) / sqrt(length(m))
SE_f <- sd(f) / sqrt(length(f))

t_p <- (mean(m) - mean(f))/sqrt(SE_m^2 + SE_f^2)


Nperm=10000

tperm <- rep(NA, Nperm)

set.seed(12345)

for (i in 1:(Nperm-1))
{
  BOX <- c(male,female)
  ord <-sample(1:200, 200)
  f <- BOX[ord[1:100]]
  m <- BOX [ord[101:200]]
  SE_m <- sd(m) / sqrt(length(m))
  SE_f <- sd(f) / sqrt(length(f))
  tperm[i]=(mean(m) - mean(f))/sqrt(SE_m^2 + SE_f^2)
}


head(tperm)
tail(tperm)

tperm [Nperm] <- t_initial

tdf <- data.frame(t = tperm)

ggplot(tdf, aes(x =t)) + geom_histogram(fill="blue", color = "black") + geom_vline(xintercept = c(t_initial, -t_initial))


p_value <- mean(tperm <= -t_initial |tperm >= t_initial )



# Пермутационная оценка значимости корреляции
library(coin)

library(MASS)

set.seed(1234567)

mu <- c(10, 20) #Вектор средних значений

Sigma <- matrix(.7, nrow=2, ncol=2)

diag(Sigma) <- c(1, 3)

# Sigma --- ковариационная матрица

dat <- as.data.frame(mvrnorm(n=100, mu=mu, Sigma=Sigma))

qplot(dat$V1, dat$V2)

cor.test(dat$V1, dat$V2, method = "spearman")

spearman_test( V1 ~ V2, data = dat, distribution = approximate(B=99999))



# Пермутационная оценка значимости мантеловской корреляции

mant <- mantel(dist_com, dist_chem, method="pearson", permutations = 9999)
mant


# Частная мантеловская корреляция

# Матрица координат описаний
geo <- read.table("data/Coordinates.txt",header = TRUE, sep = "\t")

# Матрица расстояний между точками
dist_geo <- vegdist(geo[, -1], method = "euclidean")


mantel_partial <- mantel.partial(dist_com, dist_chem, dist_geo, method = "pearson", permutations = 9999)
mantel_partial



# Модельные матрицы

com <- read.csv("data/mussel_beds.csv",
                sep=';', header = T)

ascam <- read.csv("data/ASCAM.csv",
                  sep=';', header = T)

library(dplyr)

log_com <- com %>% filter(Bank == "Vor2") %>% select(-c(1:3)) %>% decostand(.,method = "log")


log_ascam <- ascam %>% filter(Bank == "Vor2") %>% select(-c(1:2)) %>% decostand(.,method = "log")

ord_log_com <- metaMDS(log_com, autotransform = F)

mds_com <- as.data.frame(scores(ord_log_com, display = "sites"))

mds_com$Year <- com %>% filter(Bank == "Vor2") %>% select(Year)

str(mds_com)

ggplot(mds_com, aes(x = NMDS1, y = NMDS2)) + geom_path() + geom_text(label = as.character(mds_com$Year) )



dist_com <- vegdist(log_com, method = "bray")

dist_ascam <- vegdist(log_ascam, method = "euclidean")


mantel(dist_com, dist_ascam)









## Задание
# 1. Выясните есть ли многолетний градиент в динамике размерной струтуры и структуры сообщества на банке Vor4.
# 2. Оцените связь между размерной структурой мидий и структурой сообщества.



## Градиентная модельная матрица
gradient_model <- vegdist(com$Year[com$Bank == "Vor2"], method="euclidian")
gradient_model

## Тестируем гипотезу о наличии градиента с помощью теста Мантела
dist_vor2_com <- vegdist(  , method = "bray")
dist_vor2_ascam <- vegdist(  , method = "euclidean")

### 1) Наличие градиента в структуре сообщества
mantel(dist_com, gradient_model)



### 2) Наличие градиента в размерной структуре мидий
mantel( dist_ascam, gradient_model)

## Прослеживается ли связь между размерной структурой мидий и структурой сообщества?

### Не самое правильное решение
mantel(dist_vor2_com, dist_vor2_ascam)

### Более корректное решение
mantel.partial(dist_ascam , dist_com , gradient_model )





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
mantel(dist_ascam, cycl_model)

## Более корректная оценка
mantel.partial(dist_ascam, cycl_model, gradient_model)



str(varechem)


# Функция `bioenv()`из пакета `vegan`


# Количество комбинаций
2^ncol(varechem)-1





BioEnv <- bioenv(varespec, varechem, method = "spearman", index = "bray")

BioEnv



plot(veg_ord)

plot(envfit(veg_ord ~ N + P + Al + Mn + Baresoil, data = varechem  ))


# Оценка достоверности результатов BIO-ENV
# ЗАПУСКАТЬ КОД МЕЖДУ ДВУМЯ ЛИНИЯМИ ТОЛЬКО ЕСЛИ НЕ ЖАЛКО ВРЕМЕНИ!!!
#------------------------------------
perm_binv <- c(1:100)
for (i in 1:99)
{
  perm_num <- sample(1:nrow(varespec))
  perm_com <- varespec[perm_num,]
  perm_i <- bioenv(perm_com, varechem)

  manteltop <- length(perm_i$models)
  est <- c(1:manteltop)
  for (j in 1:ncol(varechem)) est[j]<-perm_i$models[[j]]$est
  perm_binv[i] <- max(est)
  cat("итерация ", i, "\n")
}

manteltop <- length(BioEnv$models)


estim <- c(1:manteltop)
for (j in 1:ncol(varechem)) estim[j]<-BioEnv$models[[j]]$est
perm_binv[100,1]<-max(estim)


perm_binv <- data.frame(perm_i=perm_binv)

p=length(perm_binv[perm_binv[,1]>=perm_binv[100,1],1])/nrow(perm_binv)

# png("BIOENV.png")

hist <- ggplot(perm_binv, aes(x=perm_i))
hist + geom_histogram (bin=0.1, fill="blue", colour="black")+geom_vline(xintercept=perm_binv[100,1], linetype=2) + theme_bw() + xlab("Мантеловские корреляции, \nполученные при пермутациях \nпроцедуры BIO-ENV ") + annotate("text", x=0.8, y=20, label=(paste("P=", p, sep=" ")))

# dev.off()
#------------------------------------




