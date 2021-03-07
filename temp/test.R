library(devtools)

load_all()

gen_atomic_preds(window_length = 60, rolling = FALSE, start_t = 5,
                bvar_3 = TRUE, bvar_7 = TRUE,
                bvar_3_o2 = TRUE, bvar_7_o2 = TRUE,
                svbvar_3 = FALSE, svbvar_7 = FALSE,
                bart_7 = FALSE, tvpsvbvar_3 = FALSE,
                bvar_3_basic = FALSE, bvar_7_basic = FALSE)

test2 <- nb_bvar(og_medium_scaled[, 1:7], start_t = 2,lags = 1,
                overall_tightness = 0.2, include_intercept = TRUE)

test3 <- nb_svbvar(macrodata[, 1:3])
test4 <- nb_svbvar(macrodata[, 1:3], lags = 2)

mean(test3$lpdens[-c(1:100)])
mean(test4$lpdens[-c(1:100)])

test5 <- nb_bart(macrodata[, 1:3])
head(test5)


test6 <- nb_tvpsvbvar(macrodata[, 1:3])
