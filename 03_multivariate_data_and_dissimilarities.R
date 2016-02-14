#Введение в методы многомерной статистики
# В. М. Хайтов

#Устанавливаем пакет {vegan}

#install.packages("vegan")


# Читаем данные
protein <- read.table("data/Protein.txt", header = TRUE, sep = "\t")
head(protein)

# Код для пересчета данных в относительные величины



# Код для сравнения Эвклидовых расстояний и коэффициентов Брея-Кёиса






# Код для построения частотного распределения значений коэффициента Брея-Кётиса


# Ординация стран в пространстве MDS


# Эвклидово расстояние

row.names(protein) <- protein$Country

mds1 <- metaMDS(protein [,-1], distance = "euclidean", autotransform = FALSE)

plot(mds1,type = "n", main = "Euclidean distance")

# points(mds1,  display = "sites")

text(mds1, display = "sites", cex = 1)


# Коэффициент Брея-Кётиса

mds2 <- metaMDS(protein [,-1], distance = "bray", autotransform = FALSE)

plot(mds2,type = "n", main = "Bray-Curtis")

# points(mds2,  display = "sites")

text(mds2, display = "sites", cex = 1)
