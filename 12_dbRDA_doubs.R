library(vegan)

source("https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/NumEcolR2/triplot.rda.R")

`%nin%` <- Negate(`%in%`)

ord_eig <- function(mod){
  round(eigenvals(mod) / mod$tot.chi * 100, 2)
}

n_common <- function(x, y) {
  sum(x > 0 & y > 0)
}

par(cex = 0.8)

# cleanplot.pca
source('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/scripts/NumEcolR1/cleanplot.pca.R')


data("doubs", package = "ade4")
help("doubs", package = "ade4")

f_rows <- rowSums(doubs$fish) > 0
env <- doubs$env[f_rows, ]
spe <- doubs$fish[f_rows, ]

mod_pca <- rda(spe)
cleanplot.pca(mod_pca)

# Эффект подковы (S-образное облако в данном случае)
mod_rda <- rda(spe ~ alt + flo + pho + nit + oxy,
    data = env)
triplot.rda(mod_rda)

# Число общих видов
n_common(spe['1', ], spe['23', ])
n_common(spe['1', ], spe['25', ])
n_common(spe['23', ], spe['25', ])

n_common(spe['1', ], spe['5', ])
n_common(spe['1', ], spe['6', ])
n_common(spe['5', ], spe['6', ])

n_common(spe['1', ], spe['15', ])
n_common(spe['1', ], spe['16', ])
n_common(spe['15', ], spe['16', ])

n_common(spe['1', ], spe['30', ])
n_common(spe['1', ], spe['29', ])
n_common(spe['29', ], spe['30', ])

mod0 <- capscale(spe ~ alt + flo + pho + nit + oxy,
                 distance = "bray",
                 data = env)
mod0_eig <- ord_eig(mod0)

triplot.rda(mod0)

triplot.rda(mod0, scaling = 2, site.sc = "lc")
triplot.rda(mod0, site.sc = "wa")


# Ординация площадок
plot(mod0, display = c("lc", "cn"),
     scaling = "sites",
     main = "Sites and environment",
     xlab = paste0("CAP1 (", mod0_eig[1], "%)"),
     ylab = paste0("CAP2 (", mod0_eig[2], "%)"))

# Ординация признаков
plot(mod0, display = c("sp", "cn"),
     scaling = "species",
     main = "Species and environment",
     xlab = paste0("CAP1 (", mod0_eig[1], "%)"),
     ylab = paste0("CAP2 (", mod0_eig[2], "%)"))
