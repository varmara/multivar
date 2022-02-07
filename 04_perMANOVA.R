# title: "Тестирование гипотез на основе многомерных данных: PERMANOVA"
# subtitle: "Анализ и визуализация многомерных данных с использованием R"
# author: Марина Варфоломеева, Вадим Хайтов

# ## Пример: Поведение песчанок в тесте открытое поле #################
# **Гипотеза:** Разные виды песчанок демонстрируют различия поведения в тесте "Открытое поле"
# Виды:
# * Карликовая песчанка (_Gerbillus gerbillus_)
# * Монгольская песчанка (_Meriones unguiculatus_)
# * Жирнохвостая песчанка (_Pachyuromys duprasi_)
# Оценка поведения песчанок трех видов по семи признакам
# * Время до выхода в квадрат открытого поля
# * Количество актов мочеиспускания
# * Количество актов дефекации
# * Количество пересеченных квадратов
# * Число вертикальных стоек
# * Количество актов смещенной активности
# * Время проведенное в центре квадрата открытого поля

pesch <- read.csv("data/pesch.csv", header = TRUE, sep = ";")
head(pesch)

# ## Задание 1 -------------------------------------
# - Какую меру различия можно использовать с этими данными?
# - Как можно преобразовать данные?
# - Постройте ординацию объектов в осях MDS и раскрасьте
# точки в соответствии с видами


# ## PERMANOVA ###################################
library(vegan)
permanova_pesch <- adonis(log_pesch[3:9] ~ log_pesch$Species, method = "euclidean")
permanova_pesch


# ## Условия применимости PERMANOVA ##############
# ## Проверка равенства внутригрупповых дисперсий
dist_pesch <- vegdist(log_pesch[,3:ncol(pesch)], method  = "euclidian")
PCO_pesch <- betadisper(dist_pesch, log_pesch$Species)
plot(PCO_pesch)
anova(PCO_pesch)
boxplot(PCO_pesch)


# # Более подробная интерпретация результатов perMANOVA


# ## Функция для попарных perMANOVA ###############
pairwise_permanova <- function(dat, group, strata = NULL, ...){
  pair <- combn(unique(as.character(group)), 2)
  ncomb <- ncol(pair)
  res <- rep(NA, ncomb)
  for (i in 1:ncomb) {
    filter <- group %in% pair[, i]
    if(is.null(strata)){
      posthoc <- adonis(dat[filter, ] ~ group[filter], ...)$aov.tab$Pr[1]
    } else {
      posthoc <- adonis(dat[filter, ] ~ group[filter],
                        strata = strata[filter], ...)$aov.tab$Pr[1]
    }
    res[i] <- posthoc
    names(res)[i] <- paste(pair[, i], collapse = " vs. ")
  }
  return(res)
}


# ## Результаты попарных сравнений

p_vals <- pairwise_permanova(
  dat = log_pesch[, -c(1:2)], group = log_pesch$Species,
  method = "euclidean", permutations=9999)
p_vals

# >- Это все? Пишем статью?

p.adjust(p_vals, method = "bonferroni")

p.adjust(p_vals, method = "holm")


# ## Многофакторный дизайн в PERMANOVA ###########

# Выясним, влияет ли пол и вид песчанок на поведение.
# Отфильтруем исходные данные (в случае с жирнохвостыми
# песчанками были изучены только самки)
log_pesch2 <- log_pesch[log_pesch$Species != "zhirnokhvost", ]

twofact_pesch <- adonis(log_pesch2[,3:ncol(pesch)] ~ Gender * Species,
                        data = log_pesch2, method = "euclidian")
twofact_pesch

# ## Здесь возможен иерархический дизайн
# Различается ли поведение самцов и самок у этих видов песчанок?
nested_pesch <- adonis(log_pesch2[, 3:ncol(pesch)] ~ Gender,
                       data = log_pesch2, strata = log_pesch2$Species,
                       method = "euclidian")
nested_pesch


# ## Задание 2 -------------------------------------
#
# + Создайте датафрейм из файла `simulated_data.csv` (Это
# данные симулированные по алгоритму, приведенному в справке
# по функции `adonis()`)
# + В этом датафрейме записано обилие двух видов на
# экспериментальных площадках двух типов: без добавления и с
# добавлением NO3, по 6 повторностей в каждом эксперименте.
# Эксперименты были независимо проведены на 3 полях.
# + Оцените, зависит ли структура совместного поселения этих
# двух видов от концентрации NO3.

