library(devtools)

load_all()

menu <- gen_atomic_list()
list_of_models <- menu[1:4]

first_half <- gen_all_data(list_of_models, woc = "wha-evva")