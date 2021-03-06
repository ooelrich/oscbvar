% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/macro_nb_generators.R
\name{nb_svbvar}
\alias{nb_svbvar}
\title{Notebook generator: Stochastic BVAR}
\usage{
nb_svbvar(
  data = oscbvar::macrodata[, 1:7],
  agc = list(5, 60, TRUE, 1),
  lags = 1,
  include_intercept = TRUE
)
}
\arguments{
\item{data}{Dataset from which to generate the notebook. Should 
include only the variables used by the model. Defaults to the
macroeconomic data set from this package using all variables.}

\item{agc}{List of atomic prediction generation controllers. The 
first element of the list gives the starting time (ie what  
observation is considered as t = 1), the second element is the 
minimum window length used for estimation, and the third one is a 
boolean indicating if the estimation window is rolling or not, the 
forth indicates which variable is the response variable and it 
defaults to 1 (GDP).}

\item{lags}{The order of the VAR model. Defaults to 1.}

\item{include_intercept}{Should the design matrix include an
intercept? Defaults to TRUE.}
}
\description{
Generates a notebook for BVAR models with stochastic volatility.
}
\details{
Uses the stochvol package with default priors.
}
