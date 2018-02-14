#Введение в методы многомерной статистики
# В. М. Хайтов

#Устанавливаем пакет {vegan}

#install.packages("vegan")


# Читаем данные
protein <- read.table("data/Protein.txt", header = TRUE, sep = "\t")
head(protein)

 # Код для пересчета данных в относительные величины





# protein_stand$Country <- protein$Country

row.names(protein_stand) <- protein$Country






library(vegan)




# Вычисляем матрицу расстояний между объектами в пакете vegan
# Заполняем пропущенные куски кода

bray <- vegdist( , method =  )

euclid <- vegdist( , method =  )

# Представляем матрицу расстояний в развернутом виде

euclid_unfold <-    (euclid)

bray_unfold  <-    (bray)


#Визуализируем соотношение эвклидова расстояния и коэффициента Брея-Куртиса

Dist <-    (Euclid = euclid_unfold, Bray = bray_unfold)

library(ggplot2)

ggplot(Dist, aes(x = , y = )) +



# Код для построения частотного распределения расстояний





