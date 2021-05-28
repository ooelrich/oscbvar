library(devtools)
library(ggplot2)
load_all()

# Looking at the atomic predictive means compared to the actual outcome
# Nr 3 will surprise you
# Used in a vignette!

pfig1 <- 
    ggplot(data = atomdat_1, aes(y = pmean, x = t)) +
    geom_line() + 
    geom_line(aes(y = ytrue, x = t), col = "red") +
    facet_wrap(~method) +
    labs(title = "predictive mean and truth (red), GDPC1")

pfig2 <- 
    ggplot(data = atomdat_2, aes(y = pmean, x = t)) +
    geom_line() + 
    geom_line(aes(y = ytrue, x = t), col = "red") +
    facet_wrap(~method) +
    labs(title = "predictive mean and truth (red), GDPCTPI")


pfig3 <- 
    ggplot(data = atomdat_3, aes(y = pmean, x = t)) +
    geom_line() + 
    geom_line(aes(y = ytrue, x = t), col = "red") +
    facet_wrap(~method) +
    labs(title = "predictive mean and truth (red), FEDFUNDS")


ggsave("vignettes/pmean_resp1.pdf", pfig1)
ggsave("vignettes/pmean_resp2.pdf", pfig2)
ggsave("vignettes/pmean_resp3.pdf", pfig3)