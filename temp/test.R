library(devtools)
load_all()






# TESTING THE DIFFERENT NOTEBOOK GENERATORS
install_github("ooelrich/goldfish",
    auth_token = "b9ca31e6d5d20f94fb1d99429a0cd615efe5bffd")
    library(goldfish)


og_sub <- og_medium_scaled[1:100, ]

test <- nb_bvar(og_sub, c(1:3))
head(test)

test2 <- nb_stochvol(og_sub, c(1:3))
head(test2)

test3 <- nb_bart(og_sub, c(1:3))
head(test3)

test4 <- nb_tvpbvar(og_sub, c(1:3))
head(test4)