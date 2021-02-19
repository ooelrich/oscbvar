#' Generates a notebook for a BVAR model v2
#' 
#' Uses Jeffreys' prior. Generates a notebook for the decision-maker to use. 
#' Uses a rolling window of default length 60 to estimate the model.
#' 
#' @param data Dataset from which to generate the notebook
#' @param model Which columns in the dataset should be included
#' @param window_length Minimum length of the estimation window.
#' @param rolling Whether to use a rolling estimation window or not.

nb_bvar <- function(data, model, window_length = 60, rolling = FALSE) {

    df <- data.frame(matrix(ncol = 2, nrow = 0))
    x <- c("pred_mean", "dens")
    colnames(df) <- x


    for (i in (window_length + 2):218) {
        
        if (rolling == TRUE) {
            j <- i - window_length - 1
        } else {
            j <- 1
        }

        Y <- data[(j + 1):(i - 1), model]
        Z <- data[j:(i - 2), model]
        z <- data[i - 1, model]
        y <- data[i, 1]
        pdist <- bvar_pd(Z, z, Y, 1)
        pdens <- bvar_osa_marg(y, pdist, 1, TRUE)

        df[(i - window_length - 1), "pred_mean"][[1]] <- pdist[[1]][1,1]
        df[(i - window_length - 1), "dens"] <- pdens

    }
    return(df)
}