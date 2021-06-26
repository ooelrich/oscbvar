library(ggplot2)

x <- c(1,2,3,4)
alpha <- c(3, 5, 3, 9)
df <- data.frame(x, alpha)
ggplot(df, aes(x = x, y = alpha)) + geom_col()