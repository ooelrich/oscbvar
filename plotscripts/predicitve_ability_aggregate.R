### VERSION FOR GDP

aggpred_data <- gen_agg_preds(
    atomdat_1,
    start_agg = 173,
    sotw = data.frame(pooling_vars[, c  (1:4, 9)]),
    baseline = TRUE,
    caliper = TRUE,
    mahala  = FALSE,
    cw = 4,
    mvc = 10
)

dftutt <- rbind(atomdat_1[atomdat_1$t > 172, ], aggpred_data)

aggpreds <- ggplot(dftutt, aes(y = lpdens, x = t, col = method)) +
    geom_line() +
    theme_classic() + 
    labs(
        title = "Predictive ability of caliper v baseline versions",
        subtitle = "GDP. Agent predictions greyed out",
        x = "Time",
        y = "Log predictive density",
        color = "Method"
    ) 
    
final <- aggpreds +
    gghighlight(
        method %in% c("caliper_propto", "gewisano", "equal_wt"),
        use_direct_label = FALSE
    )


ggsave("temp/plots to save/gdp_final.pdf", final)

### Version for GDPTPCI

aggpred_data <- gen_agg_preds(
    atomdat_2,
    start_agg = 173,
    sotw = data.frame(pooling_vars[, c  (1:4, 9)]),
    baseline = TRUE,
    caliper = TRUE,
    mahala  = FALSE,
    cw = 1,
    mvc = 10
)

dftutt <- rbind(atomdat_2[atomdat_2$t > 172, ], aggpred_data)

aggpreds <- ggplot(dftutt, aes(y = lpdens, x = t, col = method)) +
    geom_line() +
    theme_classic() + 
    labs(
        title = "Predictive ability of caliper v baseline versions",
        subtitle = "GDPCTPI. Agent predictions greyed out",
        x = "Time",
        y = "Log predictive density",
        color = "Method"
    ) 
    
final <- aggpreds +
    gghighlight(
        method %in% c("caliper_propto", "gewisano", "equal_wt"),
        use_direct_label = FALSE
    )
    

 ggsave("temp/plots to save/gdpctpi_final.pdf", final)



### Version for FEDFUNDS

aggpred_data <- gen_agg_preds(
    atomdat_3,
    start_agg = 173,
    sotw = data.frame(pooling_vars[, c  (1:4, 9)]),
    baseline = TRUE,
    caliper = TRUE,
    mahala  = FALSE,
    cw = 5,
    mvc = 10
)

dftutt <- rbind(atomdat_3[atomdat_3$t > 172, ], aggpred_data)

aggpreds <- ggplot(dftutt, aes(y = lpdens, x = t, col = method)) +
    geom_line() +
    theme_classic() + 
    labs(
        title = "Predictive ability of caliper v baseline versions",
        subtitle = "FEDFUNDS. Agent predictions greyed out",
        x = "Time",
        y = "Log predictive density",
        color = "Method"
    ) 
    
final <- aggpreds +
    gghighlight(
        method %in% c("caliper_propto", "gewisano", "equal_wt"),
        use_direct_label = FALSE
    )


ggsave("temp/fedfunds_final.pdf", final)



