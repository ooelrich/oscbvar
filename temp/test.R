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

gen_baseline <- function(atomic_df, start_t) {

    baseline_df <- gen_atomic_df()
    T <- max(atomic_df$t)

    # Giving all models equal weight
    df_all <- data.table::data.table(atomic_df)
    df_equal_wt <- df_all[start_t:T,
        .(pmean = mean(pmean), lpdens = log(mean(exp(lpdens))), method = "equal_wt", t),
        by = .(t)][, 2:5]
    baseline_df <- rbind(baseline_df, df_equal_wt)

    # Optimal prediction pools (Geweke & Amisano)
    df_gewisano <- gen_gewisano(data = atomic_df, start_t)
    baseline_df <- rbind(baseline_df, df_gewisano)

    return(baseline_df)
}

agga <- gen_baseline(atomic_df = atom_dat, start_t = 161)

gen_gewisano(atom_dat, start_t = 161)

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