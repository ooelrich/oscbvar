load_all()

# Should only look at the subset of data that we are actually using,
# so ignoring the first 65 (or more maybe) observations.

# THIS SHOULD BE A FUNCTION IN THE PACKAGE

max_tol <- 100
macr <- pooling_vars[62:214, c(2:4, 9)] # Selecting decision maker vars
T <- nrow(macr)

dist_mat <- matrix(NA, nrow = T, ncol = T)
for (i in seq_len(T)) {
    for (j in seq_len(T)) {
        dist_mat[i, j] <- sum((macr[i, ] - macr[j, ])^2)
    }
}

perc_incl <- c()
for (i in seq_len(max_tol)) {
    perc_incl[i] <- sum(as.numeric(dist_mat < i), na.rm = TRUE) / (T * (T - 1))
}

x <- 1:max_tol
plot(
    x,
    perc_incl,
    type = "l",
    xlab = "tolerance",
    ylab = "percent included",
    main = "Percentage of observations within the caliper on average"
)

# It might seem a little strange to sum over the whole distance matrix 
# (which basically means backwards and forwards in time...). However, 
# due to the nature of distance, the matrix is symmetric, so taking the 
# average over only the lower diagonal is the same as taking the average 
# over the whole thing (note that we are not counting the diagonal when 
# calculating the average).

# would be useful with a table that shows the exact distribution for 
# each tolerance as well.

# Now for the actual graph then

width_vec <- c()
no_incl <- c()
time <- c()
cw <- c(0, 1, 5, 20, 100)
k <- 0
for (j in 1:length(cw)) {
    width <- cw[j]
    for (i in 102:153) {
        k <- k + 1
        time[k] <- i
        width_vec[k] <- width
        rel_vec <- dist_mat[1:(i - 1), i]
        no_incl[k] <- sum(as.numeric(rel_vec < width)) / (i - 1)
    }
}

dfgg <- data.frame(as.factor(width_vec), no_incl, time)


plt <- ggplot(
    dfgg,
    aes(
        x = time,
        y = no_incl,
        group = factor(width_vec),
        col = factor(width_vec)
    )
)
plt <- plt  + geom_line()
plt <- plt + theme_classic()
plt <- plt + labs(
    title = "Percentage of past observations within caliper",
    subtitle = "Grouped by caliper width",
    x = "Observation nr",
    y = "Percentage past observations in caliper",
    color = "Caliper width"
    #,caption = "Data source: ToothGrowth"
)
plt <- plt + theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0, face = "italic"),
    axis.title.x = element_text(hjust = 0.5, vjust = 0,
        margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(hjust = 0.5, vjust = 0,
        margin = margin(t = 0, r = 15, b = 0, l = 0)),
)
plt

ggsave("temp/percplot.pdf", plt)