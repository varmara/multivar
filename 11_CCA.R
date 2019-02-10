#Канонический корреспондентный анализ
######################################

#Вадим Хайтов, Марина Варфоломеева


data(mite)

library(vegan)
data(mite)
data(mite.env)
data(mite.xy)
head(mite[ , 1:6], 4)




library(ggplot2)
library(gridExtra)

th <- theme(legend.text = element_text(size = 13), legend.title = element_text(size = 14), legend.position = "bottom", text = element_text(size = 10))
th_narrow <- th + theme(legend.key.width = unit(3, "mm"))

theme_set(theme_bw(base_size = 18))
update_geom_defaults("point", list(shape = 19))
p <- ggplot(mite.xy, aes(x = x, y = y)) + geom_point()
p + coord_fixed() + th

#Функция для рсования градиентов

gginterp <- function(x, y, envir, nx, ny){
  # функция интерполирует значения переменных среды
  require(akima)
  require(reshape2)
  # Интерполяция
  fld <- interp.new(x = x, y = y, z = envir, xo=seq(min(x), max(x), length = nx), yo=seq(min(y), max(y), length = ny))
  # Перевод результатов в "длинную" форму
  df_env <- melt(fld$z, na.rm = TRUE)
  colnames(df_env) <- c("x", "y", "envir")
  # Замена по порядковым номерам настоящими значениями переменных среды
  df_env$x <- fld$x[df_env$x]
  df_env$y <- fld$y[df_env$y]
  return(df_env)
}


ggmapinterp <- function(x, y, envir, nx, ny){
  # функция рисует график интерполированной переменной
  df_env <- gginterp(x, y, envir, nx, ny)
  require(ggplot2)
  p <- ggplot(data = df_env, aes(x = x, y = y, z = envir)) +
    geom_tile(aes(fill = envir)) # + stat_contour(colour = "gray40")
  return(p)
}

# модификация темы для всех графиков
th <- theme(legend.text = element_text(size = 13), legend.title = element_text(size = 14), legend.position = "bottom", text = element_text(size = 10))
th_narrow <- th + theme(legend.key.width = unit(3, "mm"))

# Для легенды
p0 <- ggmapinterp(x = mite.xy$x, y = mite.xy$y, envir = mite.env$SubsDens, nx = 50, ny = 200) + geom_point(data = mite.xy, aes(x = x, y = y, z = NULL, shape = mite.env$Substrate)) +
  scale_fill_continuous(name = "Плотность субстрата", low = "white", high = "green", guide = "none") +
  scale_shape_manual(name = "Субстрат", values=1:7, labels = c("Сфагн1", "Сфагн2", "Сфагн3", "Сфагн4", "Раст. остатки", "Голый торф", "Поверхность" )) + th

# Плотность субстрата
p1 <- ggmapinterp(x = mite.xy$x, y = mite.xy$y, envir = mite.env$SubsDens, nx = 50, ny = 200) + geom_point(data = mite.xy, aes(x = x, y = y, z = NULL, shape = mite.env$Substrate)) + scale_fill_continuous(low = "white", high = "green") +   scale_shape_manual(name = "Субстрат", values=1:7, labels = c("Сфагн1", "Сфагн2", "Сфагн3", "Сфагн4", "Раст. остатки", "Голый торф", "Поверхность" ), guide = "none") + th + ggtitle("Плотность\nсубстрата") + guides(fill = FALSE, shape = F)

# Содержание воды
p2 <- ggmapinterp(x = mite.xy$x, y = mite.xy$y, envir = mite.env$WatrCont, nx = 50, ny = 200) + geom_point(data = mite.xy, aes(x = x, y = y, z = NULL, shape = mite.env$Substrate)) + scale_fill_continuous( low = "white", high = "blue") +   scale_shape_manual(name = "Субстрат", values=1:7, labels = c("Сфагн1", "Сфагн2", "Сфагн3", "Сфагн4", "Раст. остатки", "Голый торф", "Поверхность" ), guide = "none") + th + ggtitle("Содержание\nводы") + guides(fill = FALSE, shape = F)

# Количество кустарников
p3 <- ggmapinterp(x = mite.xy$x, y = mite.xy$y, envir = mite.env$Shrub, nx = 50, ny = 200) +  geom_point(data = mite.xy, aes(x = x, y = y, z = NULL, shape = mite.env$Substrate)) +   scale_fill_continuous(low = "white", high = "darkgreen", breaks = c(1, 2, 3), labels = c("Нет", "Мало", "Много")) +   scale_shape_manual(name = "Субстрат", values=1:7, labels = c("Сфагн1", "Сфагн2", "Сфагн3", "Сфагн4", "Раст. остатки", "Голый торф", "Поверхность" ), guide = "none") + th + ggtitle("Покытие \nкустарники") + guides(fill = FALSE, shape = F)


p4 <- ggmapinterp(x = mite.xy$x, y = mite.xy$y, envir = mite.env$Topo, nx = 50, ny = 200) +  geom_point(data = mite.xy, aes(x = x, y = y, z = NULL, shape = mite.env$Substrate)) + scale_fill_continuous(low = "white", high = "orange", breaks = c(1, 2), labels = c("Ровно", "Кочка")) +  scale_shape_manual(name = "Субстрат", values=1:7, labels = c("Сфагн1", "Сфагн2", "Сфагн3", "Сфагн4", "Раст. остатки", "Голый торф", "Поверхность" ), guide = "none") + th + ggtitle("Топографические \nхарактеристики") + guides(fill = FALSE, shape = F)


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p0 + theme(legend.position = "bottom", legend.key = element_blank()))

library(gridExtra)
grid.arrange(arrangeGrob(p1, p2, p3, p4, nrow = 1), mylegend, nrow = 2, heights = c(10, 1))




# Здесь будет Ваш код для решения Задания по построению ограниченной ординации CCA


# Что нужно проверить до начала анализа?

mite_cca <- cca( )

str(mite.env)


vif.cca()

=======
mite_cca <- cca( )

mite_cca

summary(mite_cca)

################################

f_ij <- mite #Частота встречи данного вида в данной пробе, то есть это первичные даные!


Ft <- sum(mite) #Общее количество найденных животных



f_i <- apply(mite, 1, FUN = sum) #Общее количество особей в каждой пробе

p_i <- f_i/Ft #Вектор вероятностей встретить какую-либо особь в данной пробе



f_j <- apply(mite, 2, FUN = sum) #Общее количество особей в каждом виде

p_j <- f_j/Ft #Вектор вероятностей встретить особь данного вида


###########################


Q <- (f_ij*Ft - f_i %*% t(f_j))/(Ft*sqrt(f_i %*% t(f_j))) #Матрица вкладов, вычисленная через частоты

Q <- as.matrix(Q)

sum(Q^2)


U <- svd(Q)$u
D <- diag(svd(Q)$d)
V <- svd(Q)$v


Q1 <- U %*% D %*% t(V)


round(sum(Q1 - Q))

sum(round(diag(D), 5) != 0)


X <- model.matrix( ~ SubsDens + WatrCont + Substrate + Topo, data =  mite.env)


ncol(X)

nlevels(mite.env$Substrate)

nlevels(mite.env$Topo)




betas <- solve(t(X) %*% diag(p_i) %*% X) %*% (t(X) %*% diag(p_i)^(1/2) %*% Q)

#Матрица предсказанных значенй
Q_pred <- diag(p_i)^(1/2) %*% X %*% betas

U_pred <- svd(Q_pred)$u
D_pred <- diag(svd(Q_pred)$d)
V_pred <- svd(Q_pred)$v



sum(round(diag(D_pred), 5) != 0)

sum(D_pred^2)


mite_cca


Q_resid <- Q - Q_pred

U_res <- svd(Q_resid)$u
D_res <- diag(svd(Q_resid)$d)
V_res <- svd(Q_resid)$v

sum(round(diag(D_res), 5) != 0)

sum(D_res^2)

mite_cca



sum(D_res^2) + sum(D_pred^2)


constr_CA_samples <- diag(p_i^(-1/2))%*% U_pred

ggplot(as.data.frame(constr_CA_samples), aes(x=V1, y=V2)) +   geom_text(label = rownames(mite)) + labs(x = "CCA1", y = "CCA2") + theme_bw() + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2) + ggtitle("Результаты, полученные вручную")




plot(mite_cca, display = "lc", tpe = "t", scaling = 1)




constr_CA_species <- diag(p_j^(-1/2))%*% V_pred

ggplot(as.data.frame(constr_CA_species), aes(x=V1, y=V2)) + geom_text(label = colnames(mite)) + labs(x = "CCA1", y = "CCA2") + theme_bw() + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2) + ggtitle("Результаты, полученные вручную")



summary(mite_cca)

scores(mite_cca, display = "species", choices = 1:5)

spenvcor(mite_cca)


#Триплоты

plot(mite_cca, scaling = 1, main = "scaling 1")



plot(mite_cca, scaling = 2, main = "scaling 2")





plot(mite_cca, scaling = 2,  display = c("sp", "cn"),  main = "biplot cca, scaling 2")

plot(mite_cca, scaling = 1,  display = c("lc", "cn"),  main = "biplot cca, scaling 1")

# Проверка значимости ординации
anova(mite_cca)

anova(mite_cca, by="term")

anova(mite_cca, by="mar")

anova(mite_cca, by="axis")


# Компоненты изменчивости при построении модели с двумя наборами предикторов (возможно только для RDA)
mod <- varpart(mite, ~ SubsDens + WatrCont + Substrate + Topo, ~ x + y, data = cbind(mite.env, mite.xy))

showvarparts(2)


mod

plot(mod)

