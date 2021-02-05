#' The relevance adjusted goldfish
#' 
#' Calculates weights for which previous log scores to use based on the 
#' relevance adjusted goldfish.
#' 
#' @param t Decides which observation represents the present (in the notebook)
#' @param notebook Notebook containing all predictions
#' @param tol Determines how similar observations need to be to be included

relevance_goldfish <- function(t, notebook, tol) {
    index <- c()
    cov_vec <- og_medium[t-1, ]
    for (i in 1:(t-2)) {
        if (sum((cov_vec - notebook[i, "cov"][[1]])^2) < tol) {
            index <- c(index, i)
        }
    }
    return(index)
}