# Анализ связи между наборами данных
# Вадим Хайов
# Многомерные методы на R

# Пакеты
library(vegan)
library(ggplot2)

# Загружаем данные
data(varespec)
data(varechem)

# Код для построения ординаци в осях nMDS






# Применяем функцию envfit()
env_fit <- envfit(veg_ord, varechem)
env_fit

# Визуализация результатов
plot(veg_ord, display = "site")
plot(env_fit)


# Анализ связи с переменными c помощью функции `ordisurf()`
env_fit2 <- envfit(veg_ord ~ N, data = varechem)
plot(veg_ord, display = "site")
plot(env_fit2, col = "red")
ordisurf(veg_ord, varechem$N,
         add = TRUE, col="blue")
ordisurf(veg_ord, varechem$Mn,
         add = TRUE, col="green")


# Задание: Отразите связь ординации растительности со значениями концентрации гумуса.





# Вычисление мантеловской корреляции
dist_com <- vegdist(varespec)
dist_chem <- vegdist(varechem, method = "euclidean")

x <- as.vector(dist_com)
y <- as.vector(dist_chem)

R <- round(cor(x, y, method="spearman"), 3)


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

2^ncol(varechem) - 1



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

png("BIOENV.png")

hist <- ggplot(perm_binv, aes(x=perm_i))
hist + geom_histogram (bin=0.1, fill="blue", colour="black")+geom_vline(xintercept=perm_binv[100,1], linetype=2) + theme_bw() + xlab("Мантеловские корреляции, \nполученные при пермутациях \nпроцедуры BIO-ENV ") + annotate("text", x=0.8, y=20, label=(paste("P=", p, sep=" ")))

dev.off()
#------------------------------------

