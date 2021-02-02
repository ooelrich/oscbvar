library(mvtnorm)
library(goldfish)
library(LaplacesDemon)
library(ggdist)
source("R/bvar_osa.R")
source("R/bvar_pd.R")

og_tiny <- og_medium[, c("GDPC1", "FEDFUNDS", "GDPCTPI")]
Y <- og_tiny[2:217, ]
y <- og_tiny[218, ]
Z <- og_tiny[1:216, ]
pd <- bvar_pd(Z, og_tiny[218, ], Y, 1)


vu <- pd[[4]]
sig <- as.numeric(pd[[3]][1,1] / (pd[[2]] * vu))
bvar_osa_marg(y, pd, 1)

dstudent_t(x = y[1], df = vu, mu = pd[[1]][1], sigma = sqrt(sig), log = TRUE)
