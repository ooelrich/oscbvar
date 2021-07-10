# step 1, get svbvar up and running
# then the bart
# then try doing some aggregate methods
# is all good ppl

covariates <- bikes_d[
    ,
    .(
        yr,
        mnth,
        workingday,
        weather_1,
        weather_2,
        temp,
        hum,
        windspeed,
        sandy1,
        sandy2,
        cnt_l
    )
]

test <- bikes_svbvar(covariates)