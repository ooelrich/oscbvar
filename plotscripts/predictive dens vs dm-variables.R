

# All the pooling variables vs predictive ability for different outcomes
# Is this actually something interesting to look at at all???

load("plotscripts/plt-data/pooling_vars.Rdata")

dump2 <- pooling_vars[-c(1:60), ]
ussurv <- c(NA, dump2$USSURV1055[1:153])
gdp <- c(NA, dump2$GDPC1[1:153])
gdptpi <- c(NA, dump2$GDPCTPI[1:153])
fedfunds <- c(NA, dump2$FEDFUNDS[1:153])

dump1 <- atomdat_1
dump1$ussurv <- rep(ussurv, 4)
dump1$gdp <- rep(gdp, 4)
dump1$gdptpi <- rep(gdptpi, 4)
dump1$fedfunds <- rep(fedfunds, 4)
dump1 <- na.omit(dump1)
dump1$dataset <- "pred_gdp"

dump2 <- atomdat_2
dump2$ussurv <- rep(ussurv, 4)
dump2$gdp <- rep(gdp, 4)
dump2$gdptpi <- rep(gdptpi, 4)
dump2$fedfunds <- rep(fedfunds, 4)
dump2 <- na.omit(dump2)
dump2$dataset <- "pred_gdptpi"

dump3 <- atomdat_3
dump3$ussurv <- rep(ussurv, 4)
dump3$gdp <- rep(gdp, 4)
dump3$gdptpi <- rep(gdptpi, 4)
dump3$fedfunds <- rep(fedfunds, 4)
dump3 <- na.omit(dump3)
dump3$dataset <- "pred_fedfunds"

dump <- rbind(dump1, dump2, dump3)

melty_dump <- reshape2::melt(
    dump,
    na.rm = FALSE,
    value.name = "Pooling_var",
    id = c("pmean", "lpdens", "method", "t", "ytrue", "dataset")
)

melty_dump$variable <- as.character(melty_dump$variable)


ds <- unique(melty_dump$dataset)
resp <- unique(melty_dump$variable)
for (i in ds) {
    for (j in resp) {
        df <- melty_dump %>% 
            filter(dataset == i, variable == j)
        myplt <- plotfun(df, i, j)     
        ggsave(sprintf("temp/lpdens (%s) vs %s.pdf", i, j), myplt)       
    }
}


plotfun <- function(df, i, j) {
    a <- ggplot(df, aes(x = Pooling_var, y = lpdens)) +  
        geom_point() +
        labs(title = sprintf("lpdens (%s) vs %s", i, j)) +
        facet_wrap(~method)
    return(a)
}