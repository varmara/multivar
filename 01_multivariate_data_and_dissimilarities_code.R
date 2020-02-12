#Введение в методы многомерной статистики
# В. М. Хайтов

#Устанавливаем пакет {vegan}

#install.packages("vegan")


# Читаем данные

abund <- read.table("data/dolg_abundance.txt", skip = 1, header = TRUE, sep = ";")
hydrol <- read.table("data/dolg_hydrology.txt", skip = 1, header = TRUE, sep = ";")

 # Код для пересчета данных в относительные величины






library(vegan)

# Проводим подготовку данных в vegan







# Вычисляем матрицу расстояний между объектами в пакете vegan
# Заполняем пропущенные куски кода

dist_init <- vegdist( , method =  )

dist_stand <- vegdist( , method =  )

dist_log <- vegdist( , method =  )

dist_rel <- vegdist(, method = )




# Представляем матрицу расстояний в развернутом виде
# Пользовательская функция для разворачиваия матрицы коэффициетов различия

unfolding <- function(x, method = "euclidean") {
  n <- nrow(x)
  N <- (n^2 - n)/2
  unfold <- data.frame(i = 1:N, Object_j = NA, Object_k = NA, Distance = NA)
  pos <- 0
  for(i in 1:(n-1)) for(j in (i+1):n) {
    pos <- pos + 1
    unfold$Object_j[pos] <- i
    unfold$Object_k[pos] <- j
  }
  unfold$Distance <- as.vector(vegdist(x, method = method))
  unfold
}



disatances <- data.frame(Init =  ,
                         Stand = ,
                         Log = ,
                         Rel = )





# Код для построения частотного распределения расстояний
library(ggplot2)




## Код для вычисления матрицы расстояния, основанной на разных типах коэффициентов различий









# Пользовательская функция для Висконсинской полярной ординации


polarord <- function(x,...) {
  dist <- unfolding(x)
  polar_distance <- max(dist$Distance)
  Object_polus1 <- dist$Object_j[dist$Distance == max(dist$Distance)]
  Object_polus2 <- dist$Object_k[dist$Distance == max(dist$Distance)]
  Polar_coord <- data.frame(Object = 1:nrow(x), Coord = NA)
  for(i in 1:nrow(x)) {

    AC <- dist$Distance[dist$Object_j == Object_polus1 & dist$Object_k == Object_polus2]
    if (i != Object_polus1 & i != Object_polus2) BC <- dist$Distance[(dist$Object_j == i & dist$Object_k == Object_polus1)|(dist$Object_j == Object_polus1 & dist$Object_k == i)]
    if (i != Object_polus1 & i != Object_polus2) AB <- dist$Distance[(dist$Object_j == i & dist$Object_k == Object_polus2)|(dist$Object_j == Object_polus2 & dist$Object_k == i)]
    if (i != Object_polus1 & i != Object_polus2) Polar_coord$Coord[i] <- (BC^2 + AC^2 - AB^2)/(2 * AC)

  }
  Polar_coord$Coord[Object_polus1] <- 0
  Polar_coord$Coord[Object_polus2] <- polar_distance
  Polar_coord
}



# Самостоятельная работа #######

# Проанализируйте структуру многомерных данных в датасете "Polychaetes_species.csv", применяя простейше методы анализа.

# Постройте полярную ординацию проб.

# Оцените наличие связи между значениям координат в полярной ординации и факторами среды, приведенными в датасете "Polychaeta_env.csv"




