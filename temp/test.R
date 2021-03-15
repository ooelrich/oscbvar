library(devtools)

load_all()


############################################
### Working with the aggregation process ###
############################################
# quick to generate atomic data to use to test out the other functions
atom_dat <- gen_atomic_preds(window_length = 60, rolling = FALSE, start_t = 5,
                bvar_3 = TRUE, bvar_7 = TRUE,
                bvar_3_o2 = TRUE, bvar_7_o2 = TRUE,
                svbvar_3 = FALSE, svbvar_7 = FALSE,
                svbvar_3_o2 = FALSE, svbvar_7_o2 = FALSE,
                bart_7 = FALSE, tvpsvbvar_3 = FALSE,
                bvar_3_basic = TRUE, bvar_7_basic = TRUE)

# let's try it all now!
df_all <- gen_all_data(start_t = 5, start_agg = 161, rolling = FALSE, 
                         window_length = 60, tol = 15, woc = "full",
                         bvar_3 = TRUE, bvar_7 = TRUE,
                         bvar_3_o2 = TRUE, bvar_7_o2 = TRUE,
                         svbvar_3 = TRUE, svbvar_7 = TRUE,
                         svbvar_3_o2 = TRUE, svbvar_7_o2 = TRUE,
                         bart_7 = TRUE, tvpsvbvar_3 = TRUE,
                         bvar_3_basic = FALSE, bvar_7_basic = FALSE)
