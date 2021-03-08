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



RAL_gen <- function(atomic_df, start_t, rel_method) {

    RAL_df <- data.frame(matrix(ncol = 3, nrow = 0))
    x <- c("RAL", "method", "t")
    colnames(RAL_df) <- x


    return(RAL_df)
}



load_all()



# Individual tests

test2 <- nb_bvar(og_medium_scaled[, 1:7], start_t = 2,lags = 1,
                overall_tightness = 0.2, include_intercept = TRUE)

test3 <- nb_svbvar(macrodata[, 1:3])
test4 <- nb_svbvar(macrodata[, 1:3], lags = 2)

mean(test3$lpdens[-c(1:100)])
mean(test4$lpdens[-c(1:100)])

test5 <- nb_bart(macrodata[, 1:3])
head(test5)


test6 <- nb_tvpsvbvar(macrodata[, 1:3])

test7 <- nb_bvar_flat_Jeff(macrodata[, 1:3])
mean(test7$lpdens[-c(1:96)])

test8 <- nb_bvar_flat_Jeff(macrodata[, 1:7])
mean(test8$lpdens[-c(1:96)])
