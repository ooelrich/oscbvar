library(mvtnorm)
library(goldfish)
library(LaplacesDemon)
library(ggdist)
source("R/bvar_osa.R")
source("R/bvar_pd.R")
source("R/notebook_bvar")


Y <- og_medium[2:217, ]
y <- og_medium[218, ]
Z <- og_medium[1:216, ]
pd <- bvar_pd(Z, og_medium[218, ], Y, 1)

test <- notebook_bvar(og_medium, c(1,2,3))
test2 <- notebook_bvar(og_medium, c(1,2,3,4,5,6,7))

head(test)
head(test2)

mean(test$de)
mean(test2$dens)
