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

head(varechem)

sum(is.na(varechem))
sum(is.na(varespec))


# Код для построения ординаци в осях nMDS

library(ggplot2)
veg_ord <- metaMDS(varespec)
ordiplot(veg_ord, display = "sites")

veg_MDS <- as.data.frame(veg_ord$points)

ggplot(veg_MDS, aes(x = MDS1, y = MDS2, fill = varechem$Al))  + geom_point(shape=21, size =4)+ scale_fill_gradient(low = "cyan", high = "darkblue") + theme_bw() + theme(legend.position = "bottom") + labs(fill = "Aluminium concentration") + scale_fill_gradient(high = "red", low = "yellow")




# Применяем функцию envfit()
env_fit <- envfit(veg_ord, varechem)

env_fit

# Визуализация результатов
ordiplot(veg_ord, display = "site")
plot(env_fit)


# Анализ связи с переменными c помощью функции `ordisurf()`
env_fit2 <- envfit(veg_ord ~ N, data = varechem)
plot(veg_ord, display = "site")
plot(env_fit2, col = "red")

ordisurf(veg_ord, varechem$N,
         add = TRUE, col="blue")
ordisurf(veg_ord, varechem$Mn,
         add = TRUE, col="green")
env_fit3 <- envfit(veg_ord ~ Mn, data = varechem)
plot(env_fit3, col = "yellow")

# Задание: Отразите связь ординации растительности со значениями концентрации гумуса.





# Вычисление мантеловской корреляции
dist_com <- vegdist(varespec)
dist_chem <- vegdist(varechem, method = "euclidean")

x <- as.vector(dist_com)
y <- as.vector(dist_chem)

plot(x, y)



R <- round(cor(x, y, method = "spearman"), 3)


xy <- data.frame (x, y)
mant <- ggplot(xy, aes(x=x, y=y))
mant + geom_point(size=3) + xlab ("Biological dissimilarity") + ylab ("Chemical dissimilarity") + annotate("text", x=0.25, y=0.35, label=paste("Rspearmen =", R, sep=" ")) + theme_bw() + geom_smooth(method = "lm", se = FALSE)



mant <- mantel(dist_com, dist_chem, method="spearman", permutations = 999)
mant


# Частная мантеловская корреляция

# Матрица координат описаний
geo <- read.table("Coordinates.txt",header = TRUE, sep = "\t")

# Матрица расстояний между точками
dist_geo <- vegdist(geo[, -1], method = "euclidean")


mantel_partial <- mantel.partial(dist_com, dist_chem, dist_geo, method = "pearson", permutations = 9999)
mantel_partial

ncol(varechem)



2^ncol(varechem) - 1




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




## Модельные матрицы и ANOSIM
m <- vegdist(as.numeric(com$Bank), method = "euclidean")
mm <- m
mm[m > 0] <- 1
mm[m == 0] <- 0
mantel(vegdist(log_com), mm, method = "pearson")








# Функция `bioenv()`из пакета `vegan`

BioEnv <- bioenv(varespec, varechem, method = "spearman", index = "bray")
BioEnv




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

