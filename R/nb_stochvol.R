#' Generates a notebook for a simple stochastic volatility model
#' 
#' Uses Gregors prior. Generates a notebook for the decision-maker to use. 
#' Uses a rolling window of default length 60 to estimate the model.
#' 
#' @param data Dataset from which to generate the notebook
#' @param model Which columns in the dataset should be included as covariates.
#' @param window_length Length of estimation window. Defaults to 60.
#' @param rolling Whether to use a rolling estimation window or not.
#' 
#' @import stochvol

nb_stochvol <- function(data, model, window_length, rolling = FALSE) {
    df <- data.frame(matrix(ncol = 2, nrow = 0))
    x <- c("pred_mean", "dens")
    colnames(df) <- x

    for (i in (window_length + 2):218) {
        
        if (rolling == TRUE) {
            j <- i - window_length - 1
        } else {
            j <- 1
        }

        Y <- data[(j + 1):(i - 1), 1]
        Z <- data[j:(i - 2), model]
        z <- data[i - 1, model]
        y <- data[i, 1]

        sv_draws <- svsample(Y,
                           designmatrix = Z)
        pred_draws <- predict(sv_draws, 1, t(z))

        pred_mean <- mean(pred_draws$y[[1]])
        pred_sd <- sd(pred_draws$y[[1]])
        pdens <- dnorm(y, pred_mean, pred_sd, log = TRUE)

        df[(i - window_length - 1), "pred_mean"] <- pred_mean
        df[(i - window_length - 1), "dens"] <- pdens

    }
    return(df)
}