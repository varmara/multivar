# title: "Анализ избыточности (Redundancy analysis, RDA)"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Марина Варфоломеева, Вадим Хайтов


## Пример: генетика бабочек Euphydryas editha
# Частоты разных аллелей фосфоглюкоизомеразы и данные о факторах среды для 16 колоний бабочек _Euphydryas editha_ в Калифорнии и Орегоне
library(ade4)
data(butterfly)
s.label(butterfly$xy, contour = butterfly$contour, inc = FALSE)


## Структура данных
str(butterfly, max.level = 2, give.attr = FALSE, vec.len = 2)


## Создадим переменные с более короткими названиями для удобства
# частоты аллелей
gen <- butterfly$genet
head(gen, 3)
# переменные среды и географические координаты
env_geo <- cbind(butterfly$envir, butterfly$xy)
head(env_geo, 3)


## RDA в vegan
library(vegan)
bf_rda <- rda(gen ~ Altitude + Precipitation + Temp_Max + Temp_Min, data = env_geo)
summary(bf_rda)
## Собственные векторы, нагрузки переменных = “species scores”
scores(bf_rda, display = "species", choices = 1:5)

## Корреляции между откликами и предикторами
spenvcor(bf_rda)


## Визуализация ординации
## Триплот корреляций (scaling = 2): Какие переменные среды важнее всего?
plot(bf_rda, scaling = 2)
## Триплот расстояний (scaling = 1)
plot(bf_rda, scaling = 1)


# Проверка значимости ординации

## Общий тест
anova(bf_rda)
## Тест факторов, type I эффекты
anova(bf_rda, by = "term")
## Тест факторов, type III эффекты
anova(bf_rda, by = "mar")
## Тест значимости осей, ограниченных факторами
anova(bf_rda, by = "axis")



# Выбор оптимальной модели
m1 <-rda(gen ~ Altitude + Precipitation + Temp_Max + Temp_Min, data = env_geo)
m0 <- rda(gen ~ 1, data = env_geo)
m <- ordistep(m0, scope = formula(m1))
m$anova



# Частный анализ избыточности и компоненты объясненной инерции
## Делаем частный RDA: зависимость генетической структуры от среды с учетом географического положения
bf_prda_1 <- rda(gen ~ Altitude + Precipitation + Temp_Max + Temp_Min + Condition(x + y), data = env_geo)
anova(bf_prda_1) ## Пермутационный тест
## График ординации
plot(bf_prda_1, main = "Partial RDA")


## Задание
# Проверьте значимость частного RDA, описывающего зависимость генетической структуры от среды с учетом географического положения





# Компоненты объясненной изменчивости
showvarparts(2)



bf_prda_2 <- rda(gen ~ x + y + Condition(Altitude + Precipitation + Temp_Max + Temp_Min), data = env_geo)
bf_rda_full <- rda(gen ~ x + y + Altitude + Precipitation + Temp_Max + Temp_Min, data = env_geo)

# ## Задание: Найдите компоненты инерции
# 1. изменчивость, потенциально объяснимую средой и географией
# 2. изменчивость, связанную только со средой, но не с географией
# 3. изменчивость, связанную только с географией, но не со средой
# 4. изменчивость, объясненную одновременно средой и географией
### Подсказка
# Смотрите на результаты разных RDA


## Компоненты изменчивости - сводим результаты вместе
comp <- data.frame(Inertia = c(I_env, I_geo, I_env_geo, I_total))
rownames(comp) <- c('Только среда', 'Только география', 'Среда и география вместе', 'Общая объяснимая инерция')
comp$Proportion <- comp$Inertia/sum(comp$Inertia[1:3]) * 100
colnames(comp) <- c('Инерция', '%')
comp
