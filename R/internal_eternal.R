#'  Generates an empty data frame to store atomic predictions
#' 
#' @keywords internal
#' 
gen_atomic_df <- function() {
    df <- data.frame(matrix(ncol = 4, nrow = 0))
    x <- c("pmean", "lpdens", "method", "t")
    colnames(df) <- x
    return(df)
}
