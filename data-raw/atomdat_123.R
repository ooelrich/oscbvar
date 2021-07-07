library(devtools)
load_all()

# Generates atomdat_1, atomdat_2, atomdat_3
# which are "atomic" datasets with predictions from all the agents
# for the response variables gdp, tcpi, and fed (1-2-3)

menu <- gen_atomic_list()
list_of_models <- menu[c(2, 6, 9, 10)]

atomdat_1 <- gen_atomic_preds(list_of_models, list(5, 60, FALSE, 1))
atomdat_2 <- gen_atomic_preds(list_of_models, list(5, 60, FALSE, 2))
atomdat_3 <- gen_atomic_preds(list_of_models, list(5, 60, FALSE, 3))

save(atomdat_1, file = "data/atomdat_1.RData")
save(atomdat_2, file = "data/atomdat_2.RData")
save(atomdat_3, file = "data/atomdat_3.RData")