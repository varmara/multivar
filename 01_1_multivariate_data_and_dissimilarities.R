#Введение в методы многомерной статистики
# В. М. Хайтов

#Устанавливаем пакет {vegan}

#install.packages("vegan")


# Читаем данные
protein <- read.table("Protein.txt", header = TRUE, sep = "\t")
head(protein)

 # Код для пересчета данных в относительные величины
sums <- apply(protein[ , -1], MARGIN = 1, FUN = sum)

protein_stand <- protein[,-1] / sums
row.names(protein_stand) <- protein$Country

colnames(protein_stand)
# protein_stand$Country <- protein$Country

row.names(protein_stand) <- protein$Country






library(vegan)

vegdist(protein_stand)

nrow(protein_stand)

(25^2 - 25)/2


bray <- vegdist(protein_stand, method = "bray" )

euclid <- vegdist(protein_stand, method = "euclidean" )


euclid <- as.vector(euclid)

bray <- as.vector(bray)


Dist <- data.frame(Euclid = euclid, Bray = bray)

library(ggplot2)

ggplot(Dist, aes(x = Euclid, y = Bray)) + geom_point()



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
