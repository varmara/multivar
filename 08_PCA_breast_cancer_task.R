# Данные из Machine Learning Repository
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Diagnostic)

# Результаты тонкоигольной аспирационной пункционной биопсии. Описание ядер клеток

# Переменные:
# 1) id - идентификационный номер пациента
# 2) diagnosis - диагноз (M = malignant, B = benigh)
# 3-12) среднее, 13-22) - стандартное отклонение и 23-32) - худшее значение следующих признаков:
# a) radius (mean of distances from center to points on the perimeter)
# b) texture (standard deviation of gray-scale values)
# c) perimeter
# d) area
# e) smoothness (local variation in radius lengths)
# f) compactness (perimeter^2 / area - 1.0)
# g) concavity (severity of concave portions of the contour)
# h) concave points (number of concave portions of the contour)
# i) symmetry
# j) fractal dimension ("coastline approximation" - 1)
#

# # Задание:
# - Проведите анализ главных компонент. Какую долю общей изменчивости объясняют первые две главные компоненты?
# - Постройте график ординации объектов в пространстве первых двух компонент и раскрасьте точки в зависимости от диагноза.
# - При помощи таблицы или графика факторных нагрузок определите, какие признаки вносят вклад в изменчивость вдоль первых двух главных компонент.
# - Вдоль какой из компонент более выражено разделение облаков точек?

# Открываем данные и создаем названия признаков
brc <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", header = F)
features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")
names(brc) <- c("id", "diagnosis", paste0(features,"_mean"), paste0(features,"_se"), paste0(features,"_worst"))


