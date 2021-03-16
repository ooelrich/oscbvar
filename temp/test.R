library(devtools)

load_all()

menu <- gen_atomic_list()
list_of_models <- menu[1:4]


first_half <- gen_all_data(list_of_models, start_t = 5, start_agg = 161,
                           rolling = FALSE, window_length = 60, tol = 15,
                           woc = "full")
                           