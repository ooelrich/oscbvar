library(devtools)

load_all()


############################################
### Working with the aggregation process ###
############################################

atom_dat <- gen_atomic_preds(window_length = 60, rolling = FALSE, start_t = 5,
                bvar_3 = TRUE, bvar_7 = TRUE,
                bvar_3_o2 = TRUE, bvar_7_o2 = TRUE,
                svbvar_3 = FALSE, svbvar_7 = FALSE,
                svbvar_3_o2 = FALSE, svbvar_7_o2 = FALSE,
                bart_7 = FALSE, tvpsvbvar_3 = FALSE,
                bvar_3_basic = TRUE, bvar_7_basic = TRUE)


load_all()
sotw <- data.frame(t = c(1:nrow(macrodata)), macrodata)
testie <- caliper_relevance(atom_dat, 161, 5, sotw)

testie
