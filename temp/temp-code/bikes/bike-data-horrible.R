bikes_h <- read.csv("data-raw/bikesharing/hour.csv")
bikes_d <- read.csv("data-raw/bikesharing/day.csv")

# Let's reverse engineer how the daily version is derived from the
# hourly version

# temperature is the average temperature over the whole day
# (a really cold night would probably not influence the renting
# behaviour if the day is sunny and warm)
mean(bikes_h[1:24, "temp"])
bikes_d[1, "temp"]

# We have a categorical weather variable
# The aggregate appears to have been caluculated as a simple mean
# Clearly it's not the mode, as can be seen when looking at the first
# day
table(bikes_h[1:24, "weathersit"])
bikes_d[1, "weathersit"]
mean(bikes_h[1:24, "weathersit"])

# For days with missing hours the temp is still the mean of temp
# so it's not actually the daily average anymore
# This is machinelearning for sure...
# (Note that it cuts to six decimals for the days when there is no
# missing data as well)
mean(bikes_h[25:47, "temp"])
bikes_d[2, "temp"]
