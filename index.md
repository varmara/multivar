---
layout: page
title: Анализ и визуализация многомерных данных с использованием R
tagline: Презентации к курсу
description: Презентации к курсу Анализ и визуализация многомерных данных с использованием R
---

## О курсе

### Цель и задачи курса

Цель курса --- научить решать с точки зрения статистики различные типы исследовательских задач в биологии (сравнение и группировка объектов; различение и разделение групп; определение места объекта/группы в ранее описанной системе). 
После освоения материалов курса студенты смогут критически оценивать корректность применения методов многомерной статистики в научных статьях, смогут самостоятельно анализировать данные с использованием скриптов на языке статистического программирования R, представлять в наглядном виде результаты анализов, интерпретировать их и представлять в виде научного отчета.

### Расписание и контакты

#### Расписание:

9:00--10:45 по понедельникам, начиная с 15 февраля + два дополнительных четверга 9:00--10:45 18 и 25 февраля

#### Office hours: 

11:00--14:00 по понедельникам

#### Инструкторы: 

- Варфоломеева Марина Александровна -  marina.nikolaeva[]gmail.com
- Хайтов Вадим Михайлович - polydora[]rambler.ru
- Тамберг Юта Юрьевна - yutamberg[]gmail.com

### Задания, дедлайны и оценки

(1) Короткие тесты (10 минут) в начале каждой лекции, кроме самой первой --- 30 баллов. Каждый тест из 5 вопросов на каждом занятии, кроме первого, т.е. всего 55 вопросов. Каждый правильный ответ приносит 0.55 балла.

(2) Домашние задания по анализу данных --- 30 баллов. Срок выполнения --- 2 недели (прислать по e-mail). За каждое домашнее задание можно получить 5 баллов. За сданное в срок задание один дополнительный балл. Минимальный набор файлов в решенном задании --- это отчет и код.

- [Критерии оценки домашних заданий](pages/evaluation_criteria.html)
- [Пример решения домашнего задания](example-report.zip)
- [Правила хорошего кода](pages/coding_practices.html)

(3) В конце курса будет финальный тест --- 40 баллов (всего 20 вопросов, каждый правильный ответ приносит 2 балла).

Итоговая оценка:

- Выше 60 баллов - зачет
- 60-69 баллов => «удовлетворительно»; 70-79% => «хорошо»; 80 и больше => «отлично»

### Пересдачи заданий

Пропущенные тесты можно будет решить в конце курса.
Три из уже сделанных тестов на выбор можно будет перерешать.


## Ссылки и ресурсы

- [Исходный код в RMarkdown](http://github.com/varmara/multivar-course)
- [Другие ресурсы и книги для изучения R и статистики](pages/resources.html)

<!--

- [Как и где можно найти помощь с R и статистикой](pages/more_help.html)

-->

---

## Презентации

1.Знакомство с R.

- [Презентация](pages/01_introduction_to_r.html)

2.Тестирование гипотез.

- [Презентация](pages/02_hypothesis_testing.html)

3.Знакомство с многомерными данными.

- [Презентация в html5](pages/03_multivariate_data_and_dissimilarities.html); [презентация в pdf](pages/03_multivariate_data_and_dissimilarities.pdf)
- [Код](https://raw.githubusercontent.com/varmara/multivar-course/master/03_multivariate_data_and_dissimilarities.R)
- Данные: [Protein.txt](https://raw.githubusercontent.com/varmara/multivar-course/master/data/Protein.txt)
- [Домашнее задание](https://github.com/varmara/multivar-course/raw/master/tasks/task01.zip)

4.Неметрическое многомерное шкалирование (nMDS).

- [Презентация в html5](pages/04_nMDS.html); [презентация в pdf](pages/04_nMDS.pdf); [Видео](https://www.youtube.com/playlist?list=PL_m3ZHlVDNoq574p-bpZWeRTflnMhuJPK)
- [Код](https://raw.githubusercontent.com/varmara/multivar-course/master/04_nMDS.R)
- Данные: [dolg_abundance.txt](https://raw.githubusercontent.com/varmara/multivar-course/master/data/dolg_abundance.txt); [dolg_hydrology.txt](https://raw.githubusercontent.com/varmara/multivar-course/master/data/dolg_hydrology.txt)
- [Домашнее задание](https://github.com/varmara/multivar-course/raw/master/tasks/task02.zip)


5.Анализ связи между наборами данных.

- [Презентация в html5](pages/05_Mantel_correlation-based_methods.html); [презентация в pdf](pages/05_Mantel_correlation-based_methods.pdf)
- [Код](https://raw.githubusercontent.com/varmara/multivar-course/master/05_Mantel_correlation-based_methods.R)
- Данные: [Coordinates.txt](https://raw.githubusercontent.com/varmara/multivar-course/master/data/Coordinates.txt);
- [Домашнее задание](https://github.com/varmara/multivar-course/raw/master/tasks/task03.zip)

6.Тестирование гипотез на основе многомерных данных: ANOSIM, SIMPER и тест Мантела.

- [Презентация в html5](pages/06_ANOSIM_SIMPER.html); [презентация в pdf](pages/06_ANOSIM_SIMPER.pdf)
- [Код](https://raw.githubusercontent.com/varmara/multivar-course/master/06_ANOSIM_SIMPER.R)
- Данные: [mussel_beds.csv](https://raw.githubusercontent.com/varmara/multivar-course/master/data/mussel_beds.csv); [ASCAM.csv](https://raw.githubusercontent.com/varmara/multivar-course/master/data/ASCAM.csv)
- [Домашнее задание](https://github.com/varmara/multivar-course/raw/master/tasks/task04.zip)

7.Тестирование гипотез на основе многомерных данных: PERMANOVA. <!--
- [Презентация в html5](pages/07_perMANOVA.html); [презентация в pdf](pages/07_perMANOVA.pdf)
- [Код](https://raw.githubusercontent.com/varmara/multivar-course/master/07_perMANOVA.R)
- Данные: [pesch.csv](https://raw.githubusercontent.com/varmara/multivar-course/master/data/pesch.csv); [simulated_data.csv](https://raw.githubusercontent.com/varmara/multivar-course/master/data/simulated_data.csv)
- [Домашнее задание](https://github.com/varmara/multivar-course/raw/master/tasks/task05.zip)
-->

8.Кластерный анализ.

<!--
- [Презентация в html5](pages/08_cluster_analysis.html); [презентация в pdf](08_cluster_analysis.pdf)
- [Код](https://raw.githubusercontent.com/varmara/multivar-course/master/08_cluster_analysis.R)
- Данные: [XXXXX](https://raw.githubusercontent.com/varmara/multivar-course/master/data/XXXXX); [XXXXX](https://raw.githubusercontent.com/varmara/multivar-course/master/data/XXXXX)
- [Домашнее задание](https://github.com/varmara/multivar-course/raw/master/tasks/task06.zip)
-->

9.Основы матричной алгебры.

<!--
- [Презентация в html5](pages/09_introduction_to_matrix_algebra.html); [презентация в pdf](pages/09_introduction_to_matrix_algebra.pdf)
- [Код](https://raw.githubusercontent.com/varmara/multivar-course/master/09_introduction_to_matrix_algebra.R)
- Данные: [XXXXX](https://raw.githubusercontent.com/varmara/multivar-course/master/data/XXXXX); [XXXXX](https://raw.githubusercontent.com/varmara/multivar-course/master/data/XXXXX)
-->

10.Анализ главных компонент (Principal Component Analysis, PCA).

<!--
- [Презентация в html5](pages/10_PCA.html); [презентация в pdf](pages/10_PCA.pdf)
- [Код](https://raw.githubusercontent.com/varmara/multivar-course/master/10_PCA.R)
- Данные: [XXXXX](https://raw.githubusercontent.com/varmara/multivar-course/master/data/XXXXX); [XXXXX](https://raw.githubusercontent.com/varmara/multivar-course/master/data/XXXXX)
-->

11.Специальные случаи применения анализа главных компонент. Геометрическая морфометрия.

<!--
- [Презентация в html5](pages/11_PCA_geometric_morphometrics.html); [презентация в pdf](pages/11_PCA_geometric_morphometrics.pdf)
- [Код](https://raw.githubusercontent.com/varmara/multivar-course/master/11_PCA_geometric_morphometrics.R)
- Данные: [XXXXX](https://raw.githubusercontent.com/varmara/multivar-course/master/data/XXXXX); [XXXXX](https://raw.githubusercontent.com/varmara/multivar-course/master/data/XXXXX)
- [Домашнее задание](https://github.com/varmara/multivar-course/raw/master/tasks/task06.zip)
-->

12.Корреспондентный анализ (Corespondence Analysis, CA) и анализ главных компонент.

<!--
- [Презентация в html5](pages/12_CA_vs_PCA.html); [презентация в pdf](pages/12_CA_vs_PCA.pdf)
- [Код](https://raw.githubusercontent.com/varmara/multivar-course/master/12_CA_vs_PCA.R)
- Данные: [XXXXX](https://raw.githubusercontent.com/varmara/multivar-course/master/data/XXXXX); [XXXXX](https://raw.githubusercontent.com/varmara/multivar-course/master/data/XXXXX)
- [Домашнее задание](https://github.com/varmara/multivar-course/raw/master/tasks/task07.zip)
-->

13.Анализ избыточности (Redundancy analysis, RDA).

<!--
- [Презентация в html5](pages/13_RDA.html); [презентация в pdf](pages/13_RDA.pdf)
- [Код](https://raw.githubusercontent.com/varmara/multivar-course/master/13_RDA.R)
- Данные: [XXXXX](https://raw.githubusercontent.com/varmara/multivar-course/master/data/XXXXX); [XXXXX](https://raw.githubusercontent.com/varmara/multivar-course/master/data/XXXXX)
- [Домашнее задание](https://github.com/varmara/multivar-course/raw/master/tasks/task08.zip)
-->

14.Канонический корреспондентный анализ (Canonical Correspondence Analysis, CCA).

<!--
- [Презентация в html5](pages/14_CCA.html); [презентация в pdf](pages/14_CCA.pdf)
- [Код](https://raw.githubusercontent.com/varmara/multivar-course/master/14_CCA.R)
- Данные: [XXXXX](https://raw.githubusercontent.com/varmara/multivar-course/master/data/XXXXX); [XXXXX](https://raw.githubusercontent.com/varmara/multivar-course/master/data/XXXXX)
- [Домашнее задание](https://github.com/varmara/multivar-course/raw/master/tasks/task09.zip)
-->


