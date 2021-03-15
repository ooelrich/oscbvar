library(devtools)

load_all()

menu <- gen_atomic_list()
first_four <- menu[1:4]

############################################
### Working with the aggregation process ###
############################################
# quick to generate atomic data to use to test out the other functions
atom_dat <- gen_atomic_preds(lom, window_length = 60, rolling = FALSE, start_t = 5)


first_half <- gen_all_data(list_of_models, start_t = 5, start_agg = 161,
                           rolling = FALSE, window_length = 60, tol = 15,
                           woc = "full")

 


test_bart <-  nb_bart(macrodata)
