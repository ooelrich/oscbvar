# should create a single pdf (or maybe a folder) with some visualisation

# 1 is winter
ggplot(bikes, aes(y = cnt, x = season)) + geom_col()

# 1 is "best" weather, 4 is "worst"
ggplot(bikes, aes(y = cnt, x = weathersit)) + geom_col()

ggplot(bikes, aes(y = cnt, x = holiday)) + geom_col()

# Here is where we save the stuff after having processed it
# usethis::use_data()

# "känns som temp" har sämre förklaringsgrad än temperatur!!
# (inte så stor skillnad)
summary(lm(bikes$cnt ~ bikes$temp))
summary(lm(bikes$cnt ~ bikes$atemp))
