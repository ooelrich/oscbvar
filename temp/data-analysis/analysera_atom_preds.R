library(data.table)

atom1dt <- data.table(atomdat_1)
atom2dt <- data.table(atomdat_2)
atom3dt <- data.table(atomdat_3)


#######################################################
### Summary of the predictive ability of the agents ###
#######################################################

summ_stat <- function(df) {
    df[,
        .(
            lpdens_mean = mean(lpdens), 
            prederr_mean = mean(sqrt((pmean - ytrue)^2))
        ),
        by = .(method)
    ][order(lpdens_mean), ]
}

lapply(list(atom1dt[atom1dt$t > 72, ], atom1dt), summ_stat)
lapply(list(atom2dt[atom2dt$t > 72, ], atom2dt), summ_stat)
lapply(list(atom3dt[atom3dt$t > 72, ], atom3dt), summ_stat)