#  ---
#  title: "Distance-based Redundancy Analysis, dbRDA"
#  subtitle: "Анализ и визуализация многомерных данных с использованием R"
#  author:
#    - Марина Варфоломеева
#    - Вадим Хайтов
#  company: 'Каф. Зоологии беспозвоночных, СПбГУ'
#  ---

#  # Пример: Растительность в сосновых лесах, где пасутся олени ==========
#  Фрагмент из датасета (Henry et al. 1995).
#  - `varespec` --- покрытие 44 видов на 24 участках в северной Финляндии и на
#  Кольском полуострове.
#  - `varechem` --- 14 переменных, описывающих условия среды.

#  Какие факторы среды определяют облик растительного сообщества?

# Знакомство с данными --------------------------------------------
library("ggplot2")
theme_set(theme_minimal(base_size = 18))
library("cowplot")
library("vegan")

data("varespec")
data("varechem")
head(varespec, 2)
sum(is.na(varespec))


# Задание 1 ----------------------------------------------------------
# Сделайте PCA данных о составе сообщества
# Сколько изменчивости объясняют первые две главные компоненты?
# Нарисуйте график ординации







## Экологически-осмысленные трансформации -------------------------

#  Степенные трансформации

# Квадратный корень
mod_pca_sqrt <- rda(sqrt(varespec))
biplot(mod_pca_sqrt, scaling = 2, main = "квадратный корень")

# Логарифм
mod_pca_log <- rda(log(varespec + 1))
biplot(mod_pca_log, scaling = 2, main = "логарифм")

# Трансформации для перехода к другим расстояниям

#  Хордальное расстояние
mod_pca_chord <- rda(decostand(varespec, method = "normalize"))
biplot(mod_pca_chord, scaling = 2, main = "хордальное расст.")

#  Расстояние Хеллингера
mod_pca_hell <- rda(decostand(varespec, method = "hellinger"))
biplot(mod_pca_hell, scaling = 2, main = "расст. Хеллингера")



# # Principal Coordinate Analysis, PCoA -----------------------------------

d_eucl <- vegdist(varespec, method = "euclidean")
mod_pcoa_eucl <- cmdscale(d_eucl, k = nrow(varespec) - 1, eig = TRUE)
ordiplot(mod_pcoa_eucl, type = "t")

# Хордальное р.

varespec_norm <- decostand(varespec, "normalize")
d_chord <- vegdist(varespec_norm, method = "euclidean")
mod_pcoa_chord <- cmdscale(d_chord, k = nrow(varespec) - 1, eig = TRUE)
ordiplot(mod_pcoa_chord, type = "t")

# Р. Хеллингера
varespec_hel <- decostand(varespec, "hellinger")
d_hel <- vegdist(varespec_hel, method = "euclidean")
mod_pcoa_hel <- cmdscale(d_hel, k = nrow(varespec) - 1, eig = TRUE)
ordiplot(mod_pcoa_hel, type = "t")

# Коэф. Брея-Куртиса
d_bray <- vegdist(varespec,  method = "bray")
mod_pcoa_bray <- cmdscale(d_bray,  k = nrow(varespec) - 1, eig = TRUE)
ordiplot(mod_pcoa_bray, type = "t")


# Задание 2 ---------------------------------------------------------------

# Получите собственные числа из анализа PCoA по матрице коэффициентов
# Брея-Куртиса.




# # Поправки в `vegan::cmdscale()` --------------------------------------------

#  Поправка Сailliez --- единственная в `cmdscale()`
d_bray <- vegdist(varespec,  method = "bray")
mod_pcoa_bray_cai <- cmdscale(d_bray,  k = nrow(varespec) - 1, eig = TRUE,
                              add = TRUE) # <<-
eigenvals(mod_pcoa_bray_cai)

# График
ordiplot(scores(mod_pcoa_bray_cai, choices = c(1, 2)), type = "t")
abline(h = 0, lty = 1, lwd = 0.5)
abline(v = 0, lty = 1, lwd = 0.5)

# Взвешенные средние видов
varespec_wa <- wascores(mod_pcoa_bray_cai$points[, 1:2], varespec)
text(varespec_wa, rownames(varespec_wa), cex = 0.7, col = "red")


# Задание 3 ---------------------------------------------------------------

# Нанесите на график ординации проекции переменных среды при помощи envfit
# (Используйте только переменные, значимо связанные со структурой сообществ)





# Поправки в `ape::pcoa()` --------------------------------------------------
library(ape) # здесь есть обе поправки к PCoA

d_bray <- vegdist(varespec,  method = "bray")

# Без поправки
mod_pcoa_raw <- pcoa(d_bray)
mod_pcoa_raw$values$Eigenvalues
biplot.pcoa(mod_pcoa_raw, varespec)

# Сailliez
mod_pcoa_cai <- pcoa(d_bray, correction = "cailliez")
mod_pcoa_cai$values$Corr_eig
biplot.pcoa(mod_pcoa_cai, varespec)

#  Lingoes
mod_pcoa_lin <- pcoa(d_bray, correction = "lingoes")
mod_pcoa_lin$values$Corr_eig
biplot.pcoa(mod_pcoa_lin, varespec)



#  Дальше --- прямая ординация ===============================================

#  tbRDA --------------------------------------------------------------------


# Функция ordiggplot ----
# Рисует график ординации из vegan в ggplot
ordiggplot <- function(mod, lab_size = 5, lab_var_size = 6,
                       line_size = 0.5, point_size = 2,
                      plot_sites = TRUE, plot_species = TRUE,
                      plot_centroids = TRUE, plot_biplot = TRUE,
                      plot_factorbiplot = TRUE, ...){
  mod_dat <- scores(mod, tidy = TRUE, ...)
  ax_names <- colnames(mod_dat)[1:2]
  names(mod_dat)[1:2] <- c("X", "Y")
  mod_eig <- round(eigenvals(mod) / mod$tot.chi * 100, 2)
  ar <- arrow(angle = 10, length = unit(2, "mm"), type = "closed")
  gg <- ggplot() +
    geom_hline(yintercept = 0, colour = "grey70", size = 0.25) +
    geom_vline(xintercept = 0, colour = "grey70", size = 0.25)
  if(any(mod_dat$score == "sites" & plot_sites)) {
      gg <- gg +
      geom_point(data = filter(mod_dat, score == "sites"),
                 aes(x = X, y = Y), size = point_size) +
      geom_text(data = filter(mod_dat, score == "sites"),
                aes(x = X, y = Y, label = label),
                size = lab_size, hjust = -0.7,
                colour = "grey40")
  }
  if(any(mod_dat$score == "species" & plot_species)) {
      gg <- gg +
        geom_segment(data = filter(mod_dat, score == "species"),
                   aes(x = 0, y = 0, xend = X, yend = Y),
                   size = line_size, colour = "orangered", arrow = ar) +
        geom_text(data = filter(mod_dat, score == "species"),
                  aes(x = X, y = Y, label = label),
                  size = lab_size, hjust = 1.3, vjust = 0.4,
                  colour = "orangered")
  }
  if(any(mod_dat$score == "centroids" & plot_centroids)) {
    gg <- gg + geom_point(data = filter(mod_dat, score == "centroids"),
               aes(x = X, y = Y),
               shape = 13, size = 3,
               colour = "grey20") +
      geom_text(data = filter(mod_dat, score == "centroids"),
                aes(x = X, y = Y, label = label),
                size = lab_var_size, hjust = -0.2,
                colour = "grey20")
  }
  if(any(mod_dat$score == "factorbiplot" & plot_factorbiplot)) {
    gg <- gg + geom_point(data = filter(mod_dat, score == "factorbiplot"),
                          aes(x = X, y = Y),
                          shape = 19, size = 0.5,
                          colour = "blue") +
      geom_text(data = filter(mod_dat, score == "factorbiplot"),
                aes(x = X, y = Y, label = label),
                size = lab_var_size, hjust = -0.2,
                colour = "blue")
  }
  if(any(mod_dat$score == "biplot" & plot_biplot)) {
    gg <- gg + geom_segment(data = filter(mod_dat, score == "biplot"),
                 aes(x = 0, y = 0, xend = X, yend = Y),
                 size = line_size, colour = "blue",  arrow = ar) +
      geom_text(data = filter(mod_dat, score == "biplot"),
                aes(x = X, y = Y, label = label),
                size = lab_var_size, hjust = -0.2,
                colour = "blue")
  }
  gg + coord_cartesian() +
      labs(x = paste0(ax_names[1], " (", mod_eig[1], "%)"),
           y = paste0(ax_names[2], " (", mod_eig[2], "%)"))
  }


#  ## tbRDA с расстоянием Хеллингера
mod_tbrda <- rda(decostand(varespec, method = 'hellinger') ~
                   Mn + Baresoil + N, data = varechem)

eigenvals(mod_tbrda)/sum(eigenvals(mod_tbrda))

# ordiggplot(mod_tbrda, scaling = 1)
ordiggplot(mod_tbrda, scaling = 2) + aes(colour = varechem$Mn)

anova(mod_tbrda, by = "mar")


#  dbRDA ------------------------------------------------------------

mod_dbrda_lingoes <- capscale(varespec ~ Mn + Baresoil + N,
                              distance = "bray", data = varechem, add = TRUE)

eigenvals(mod_dbrda_lingoes)/sum(eigenvals(mod_dbrda_lingoes))

# ordiggplot(mod_dbrda_lingoes, scaling = 1)
ordiggplot(mod_dbrda_lingoes, scaling = 2) + aes(colour = varechem$Mn)

anova(mod_dbrda_lingoes, by = "mar")


# Задание 4 ------------------------------------------------------------------
# Рыбы в реке Ду, Франция
#  Данные из Borcard et al., 2011, исходно из PhD диссертации Verneaux (1973).
#  - `doubs$fish` --- Обилие 27 видов рыб в 30 точках на реке Ду
#  - `doubs$env` --- 11 характеристик условий среды
#  - `doubs$xy` --- Координаты точек

#  Проанализируйте, какие факторы среды определяют облик сообществ рыб в реке
#  Ду, если удалить влияние географической близости точек сбора.

data("doubs", package = "ade4")
help("doubs", package = "ade4")





