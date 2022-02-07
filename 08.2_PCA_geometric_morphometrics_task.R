# Геометрическая морфометрия тела рыб Cyprindon pecosensis.
# - coords - координаты лендмарок, выравненные при помощи gpa
# - CS - размер центроида
# - Sex - пол ("F","M")
# - Pop - популяция ("Marsh","Sinkhole")
# Collyer, M.L., D.J. Sekora, and D.C. Adams. 2015. A method for analysis of phenotypic change for phenotypes described by high-dimensional data. Heredity. 115: 357-365.

# Задание:
# Сделайте PCA по выравненным координатам лендмарок.
# Сколько изменчивости объясняют первые две главных компоненты?
# Нарисуйте график главных компонент:
# - раскрасьте точки в зависимости от пола и популяции рыб,
# - приведите графики изменения формы вдоль главных компонент.

library(geomorph)
data("pupfish")
