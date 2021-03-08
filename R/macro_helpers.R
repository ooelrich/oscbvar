#' @title Generate atomic data frame 
#' 
#' @description
#' Generates an empty data frame to store atomic predictions.
#' 
#' @details 
#' Separate from the actual notebook functions to ensure that each function
#' stores its predictions in identical data frames, since otherwise the merging
#' won't work.
#' 
#' @keywords internal
 
gen_atomic_df <- function() {
    df <- data.frame(matrix(ncol = 4, nrow = 0))
    x <- c("pmean", "lpdens", "method", "t")
    colnames(df) <- x
    return(df)
}

#' @title Generate Z matrix
#' 
#' @description Generates a Z (design) matrix for a VAR model.
#' 
#' @details 
#' Given a data frame, a starting point, a number of lags, and whether to
#' include an intercept or not, creates a design matrix with correctly names
#' columns.
#' 
#' @param data Data frame (Y).
#' @param start_t Which observation is considered as t = 1. Needs to be greater 
#'   than the number of lags.
#' @param lags Order of the VAR.
#' @param include_intercept Boolean. Determines is an intercept should be used
#'   or not.

gen_Z <- function(data, start_t, lags, include_intercept) {

    intercept <- rep(1, nrow(data) - start_t + 1)
    Z <- data.frame(intercept)
    for (i in seq_len(lags)) {
        tmp <- data[(start_t - i):(nrow(data) - i), ]
        colnames(tmp) <- paste0(colnames(tmp), sprintf("_lag_%i", i))
        Z <- cbind(Z, tmp)
    }
    if (include_intercept == FALSE) {
        Z <- Z[, -1]
    }
    return(as.matrix(Z))
}


#' @title Omega_0 for the Minnesota prior
#' 
#' @description Generates an Omega_0 according to a (the) Minnesota prior.
#' 
#' @details 
#' Prior variance for the intercept (if included) is set to 10 000. Cross series
#' tightness is set to 1.
#' 
#' @param overall_tightness Overall tightness.
#' @param lag_decay Lag decay.
#' @param m Dimensionality of each y-vector (or columns in Y).
#' @param include_intercept Whether or not the model includes an intercept.
#' 
#' @keywords internal

omega_minnesota <- function(lags, overall_tightness, lag_decay, m, include_intercept) {
    om_vec <- c()
    if (include_intercept == TRUE){
        om_vec <- c(100^2)
    }
    for (i in seq_len(lags)) {
        om_vec <- c(om_vec, rep((overall_tightness / i)^2, m))
    }
    return(diag(om_vec))
}

#' @title Generate GewSano Weights
#' 
#' @description
#' Generates weights according to Geweke & Amisano 2011/2012, also known as
#' linear predicition pools.
#' 
#' @param data Data set of atomic predictions.
#' @param start_t Which timepoint to start generating weights for. Obs! This is
#'   based on the variable t in the data, not the row number!

gen_gewisano <- function(data, start_t) {

    #data <- atom_dat # debug stuff
    #start_t <- 100

    fun_to_opt <- function(x, dataopt) {
        -sum(log(exp(dataopt) %*% x))
    }

    data <- data.table::data.table(data)
    df_fat <- data.table::dcast(data, t ~ method, value.var = "lpdens")
    k <- ncol(df_fat) - 1
    start_val <- rep((1 / k), k)
    t <- seq(start_t, max(data$t))
    wts <- matrix(ncol = k, nrow = length(t))

    j <- 0
    for (i in start_t:(max(data$t))) {
        j <- j + 1
        vecop <- as.matrix(df_fat[t <= i, 2:(k + 1)])
        opt_sol <- pracma::fmincon(start_val, fun_to_opt, Aeq = matrix(1, 1, k),
                beq = 1, lb = rep(0, k), ub = rep(1, k), dataopt = vecop)
        wts[j, ] <- opt_sol$par
    }

    lpdens <- log(rowSums(exp(df_fat[t >= start_t, 2:(k + 1)]) * wts))
    df_pmean <- data.table::dcast(data, t ~ method, value.var = "pmean")
    pmean <- rowSums(df_pmean[t >= start_t, 2:(k + 1)] * wts)
    
    method <- rep("gewisano", length(t))
    df_gew_res <- data.frame(pmean, lpdens, method, t)
    df_gew <- gen_atomic_df()
    df_gew <- rbind(df_gew, df_gew_res)

    return(df_gew)
}