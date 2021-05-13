# Doing the old classics evaluation
lpdens_agg <- ggplot(aggpred_3, aes(y = lpdens, x = t, col = method)) +
    geom_line() +
    labs(title = "predictive density for aggregate methods")
ggsave("figs/lpdens_agg_fin_3.pdf", lpdens_agg)

aggpred_3[, .(lpdens_sum = sum(lpdens)), by = .(method)][order(lpdens_sum), ]