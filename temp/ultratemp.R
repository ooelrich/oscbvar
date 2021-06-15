
df_fed <- rbind(atomdat_3[atomdat_3$t > 173, ], aggpred_data)
# This should be done with a lookup table
df_fed$cat <- 1
for (i in seq_len(nrow(df_fed))){
    if (df_fed$method[i] == "equal_wt") {
        df_fed$cat[i] <- 2
    }
    if (df_fed$method[i] == "gewisano") {
        df_fed$cat[i] <- 3
    }
    if (df_fed$method[i] == "caliper_propto") {
        df_fed$cat[i] <- 4
    }
}
df_fed$cat <- factor(df_fed$cat)

aggpreds <- ggplot(
        df_fed,
        aes(y = lpdens, x = t, group = method, col = cat)) +
    geom_line(position = position_dodge(width = 0.5), size = 0.7) +
    theme_classic() + 
    labs(
        y = "Log predictive density",
        color = "Method"
    ) +
    xlim(174, 215) +
    theme(aspect.ratio = 7/16)

# Make data frame for the labels
df_fed <- data.table(df_fed)
df_lab <- df_fed[t == 214] # select the last point, (t, elpd) will be (x,y)
df_lab[7, 2] <- df_lab[7, 2] - 0.1  # Move caliper version down slighlty
df_lab[6, 2] <- df_lab[6, 2] + 0.05 # Move linear pool up a smidge

# Better names
df_lab[1, 3] <- "BVAR"
df_lab[2, 3] <- "SVBVAR"
df_lab[3, 3] <- "BART"
df_lab[4, 3] <- "TVPSV"
df_lab[5, 3] <- "Equal weight"
df_lab[6, 3] <- "Linear pool"
df_lab[7, 3] <- "Local pool"

final <- final + geom_text(
    df_lab,
    mapping = aes(x = t, y = lpdens, label = method, color = cat),
    nudge_x = 2.3,
    size = 2
) + theme(legend.position = "none")


my_colors <- c("grey", RColorBrewer::brewer.pal(3, "Dark2"))
names(my_colors) <- levels(factor(c(levels(df_fed$cat), levels(df_lab$cat)))) 
my_scale <- scale_color_manual(name = "cat", values = my_colors)
final <- final + my_scale  

# Finally, fixing the x-axis
final <- final + scale_x_continuous(
    name = "",
    breaks = c(178, 190, 202, 214),
    labels = c("2016Q1", "2017Q1", "2018Q1", "2019Q1")
)

ggsave("temp/fedfunds_final.pdf", final)
