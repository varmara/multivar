library(readxl)
library(tidyr)
library(dplyr)
library(tibble)
library(ggplot2)

# install.packages("devtools")
# devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan)

dat <- read.delim("data/Krapivin_2017_Medulis-symb.tab", skip = 36)
str(dat)

colnames(dat) <- gsub("[.]", replacement = "", colnames(dat))

dat <- rename(dat, L = 'MedulisLshellmm', Age = 'Medulisagea')

dat$Site <- factor(dat$Site, levels = c(2, 3, 1), labels = c("G","L","S"))

f_remove <- c("Nematoda", "Microsetella", "Copepoda", "Chironomidae", "Halacaridae", "Jaeraspp", "Ostrac")
f_sp <- c("Urastomaspp", "Renicolaspp", "Himasthlaspp", "Metacercaria", "Gymnophallusspp", "Alg", "Other")
f_vars <- c("Zone", "Site", "L", "Age")

dat$Other <- rowSums(dat[, f_remove])
dat$Total  <- rowSums(dat[, f_sp])

dat <- dat[dat$Total != 0 & dat$Age %in% 3:8, ]

# Данные для анализа
spec <- dat[, f_sp]
env <- dat[, f_vars]

with(env, table(Site, Zone))


# nMDS ==================
ord_mus <- metaMDS(spec, dist = "bray", autotransform = FALSE)
stressplot(ord_mus)
ord_mus$stress


# Простой график
ordiplot(ord_mus)

# Палитры
pal_col <- c("red", "green", "steelblue")
pal_sh <- c(1, 2, 0)

# Украшенный график
ordiplot(ord_mus, type = "n")
points(ord_mus, col = pal_col[env$Zone], pch = pal_sh[env$Site])

# Украшенный график с центроидами видов
ordiplot(ord_mus, type = "n")
points(ord_mus, col = pal_col[env$Zone], pch = pal_sh[env$Site])
text(ord_mus, display = "spec", cex=0.9, col="grey20")

# # Многоугольники ===========
# ordiplot(ord_mus, type = "n")
# ordihull(ord_mus, env$Zone, col = pal_col, lwd = 3)
# ordispider(ord_mus, env$Zone, col = pal_col, label = TRUE)
#
# # Эллипсы =================
# ordiplot(ord_mus, type = "n")
# points(ord_mus, col = pal_col[env$Zone], pch = pal_sh[env$Site])
# ordiellipse(ord_mus, env$Zone, col = pal_col, kind = "sd", lwd = 3)

# Envfit & Ordisurf =====================
ef <- envfit(ord_mus, env[, c("Zone", "Site", "L", "Age")])

# График с векторами и центроидами (envfit)
ordiplot(ord_mus, type = "n")
points(ord_mus, col = pal_col[env$Zone], pch = pal_sh[env$Site])
plot(ef)

ef

# Графики с поверхностью (ordisurf)
par(mfrow = c(1, 2))
os_L <- ordisurf(ord_mus, env$L, method = "REML")
os_Age <- ordisurf(ord_mus, env$Age, method = "REML")
par(mfrow = c(1, 1))

summary(os_L)
summary(os_Age)


# Графики в ggplot2 ==============================

# Данные для графика ординации -------------------
ord_mus_pt <- data.frame(env, scores(ord_mus, display = "sites"))
head(ord_mus_pt)

ord_mus_sp <- data.frame(scores(ord_mus, display = "species"))
ord_mus_sp$Species <- rownames(ord_mus_sp)
head(ord_mus_sp)

# Ординация --------------------------------------
gg_ord_mus <- ggplot() +
  geom_point(data = ord_mus_pt,
             aes(x = NMDS1, y = NMDS2,
                 colour = Zone,
                 shape = Site,
                 size = L),
             alpha = 0.5)
gg_ord_mus
gg_ord_mus + facet_wrap(~Site)



# Ординация и центроиды видов (т.е. переменных) ---------
gg_ord_mus_sp <- gg_ord_mus +
  geom_text(data = ord_mus_sp,
            aes(x = NMDS1, y = NMDS2,
                label = Species))
gg_ord_mus_sp



autoplot(ef)

# График envfit ----------------------------------
ord_mus_ef <- fortify(ef)

gg_ord_mus +
  geom_segment(data = ord_mus_ef[ord_mus_ef$Type == "Vector", ],
               aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm"))) +
  geom_text(data = ord_mus_ef[ord_mus_ef$Type == "Vector", ],
            aes(x = NMDS1, y = NMDS2, label = Label,
                hjust = 1.1, vjust = 1)) +
  geom_text(data = ord_mus_ef[ord_mus_ef$Type == "Centroid", ],
            aes(x = NMDS1, y = NMDS2, label = Label,
                hjust = 1.1, vjust = 1))


# График ordisurf --------------------------------

fortify.ordisurf <- function(model) {
  # Fortifies an object of class ordisurf to produce a dataframe of contour coordinates for subsequent plotting in ggplot.
  xy <- expand.grid(x = model$grid$x, y = model$grid$y)
  xyz <- cbind(xy, c(model$grid$z))
  names(xyz) <- c("x", "y", "z")
  return(na.omit(xyz))
}

ord_mus_os <- fortify.ordisurf(os_Age)
head(ord_mus_os)

ggplot(data = ord_mus_os, aes(x = x, y = y, z = z)) +
  stat_contour(aes(colour = ..level..))


# Подписать контуры можно вручную
labs_contour <- data.frame(
  x = c(-0.7, -0.4, -0.1, 0.2, 0.6, 0.6, 0.6, 0.6, 1.25),
  y = c(1, 1.1, 1.2, 1.3, 1.1, 0.6, 0.25, -0.2, 1.5),
  lab = c(4.2, 4.4, 4.6, 4.8, 5.0, 5.2, 5.4, 5.6, 5.4),
  z = NA)
ggplot(data = ord_mus_os, aes(x = x, y = y, z = z)) +
  stat_contour(aes(colour = ..level..)) +
  geom_text(data = labs_contour, aes(x = x, y = y, label = lab))


# в ggvegan пока нет функции, но есть пакет ggordiplots
# https://oliviarata.wordpress.com/2014/07/17/ordinations-in-ggplot2-v2-ordisurf/
# devtools::install_github("jfq3/ggordiplots")
# Виньетка
# https://john-quensen.com/wp-content/uploads/2017/12/Ordiplots_with_ggplot.pdf

# Эллипсы ----------------------------------------
# tidies ellipses for ggplot2
ordielipse_by <- function(coord, groups, kind = "se", conf = 0.95, ...){
  ellipses <- ordiellipse_coord(coord, groups, kind = kind, conf = conf, ...)
  elps <- lapply(ellipses, function(x)x$xy)
  tms <- sapply(elps, nrow)
  nms <- mapply(rep, x = names(elps), times = tms, SIMPLIFY = FALSE)
  nms <- Reduce(c, nms)
  elps <- Reduce(rbind, elps)
  orde <- data.frame(nms, as.data.frame(elps))
  colnames(orde) <- c("groups", "X", "Y")
  return(orde)
}

# Modified from vegan package. Computes ellipses
ordiellipse_coord <- function (coord, groups, kind = c("sd", "se"), conf, show.groups,  ...)
{
  kind <- match.arg(kind)
  ## ordiellipse only works with 2D data (2 columns)
  coord <- as.matrix(coord)
  if (ncol(coord) > 2)
    coord <- coord[ , 1:2, drop = FALSE]
  if (ncol(coord) < 2)
    stop("ordiellipse needs two dimensions")
  w <- rep(1, nrow(coord))
  if (!missing(show.groups)) {
    take <- groups %in% show.groups
    coord <- coord[take, , drop = FALSE]
    groups <- groups[take]
    w <- w[take]
  }
  out <- seq(along = groups)
  inds <- names(table(groups))
  res <- list()
  ## Remove NA scores
  kk <- complete.cases(coord)
  for (is in inds) {
    gr <- out[groups == is & kk]
    if (length(gr) > 2) {
      X <- coord[gr, ]
      W <- w[gr]
      mat <- cov.wt(X, W)
      if (kind == "se")
        mat$cov <- mat$cov/mat$n.obs
      if (missing(conf))
        t <- 1
      else t <- sqrt(qchisq(conf, 2))
      xy <- vegan:::veganCovEllipse(mat$cov, mat$center, t)
      mat$scale <- t
      mat$xy <- xy
      res[[is]] <- mat
    }
  }
  class(res) <- "ordiellipse_coord"
  invisible(res)
}


ellipse_coord <- ordielipse_by(ord_mus$points, groups = env$Zone, kind = "se", conf = 0.95)

gg_ord_mus_sp + geom_path(data = ellipse_coord,
                          aes(x = X, y = Y,
                              colour = groups), size = 1.5)
