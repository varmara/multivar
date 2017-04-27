#Канонический корреспондентный анализ
######################################

#Вадим Хайтов, Марина Варфоломеева


data(mite)

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


# Компоненты изменчивости при построении модели с двумя наборами предикторов
mod <- varpart(mite, ~ SubsDens + WatrCont + Substrate + Topo, ~ x + y, data = cbind(mite.env, mite.xy))

mod

plot(mod)

