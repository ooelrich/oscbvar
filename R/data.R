#' @title Medium data set from OG et al 2020, rescaled to unit variance
#'
#' @description The final observation corresponds to 2019Q1. You do the rest of
#'   the math.
#' 
#' @format A data frame with 218 rows and 7 variables:
#' \describe{
#'   \item{GDPC1}{Real GDP. Transformed by 400 x diff-log. Quarterly.}
#'   \item{GDPCTPI}{GDP deflator. Transformed by 400 x diff-log. Quarterly.}
#'   \item{FEDFUNDS}{FED funds rate. Quarterly.}
#'   \item{PCECC96}{Real consumption. Transformed by 400 x diff-log. Quarterly.}
#'   \item{GPDIC1}{Real investment. Transformed by 400 x diff-log. Quarterly.}
#'   \item{HOANBS}{Hours worked. Transformed by 400 x diff-log. Quarterly.}
#'   \item{AHETPIx}{Real compensation/hour. Transformed by 400 x diff-log. Quarterly.}
#' }
"macrodata"

#' @title Predictions from the tvpsvbvar model with variable 3 as response.
#'
#' @description Bla bla bla.
#' 
#' @format A data frame with 154 rows and 5 variables:
#' \describe{
#'   \item{pmean}{Predictive mean.}
#'   \item{lpdens}{Log predictive density.}
#'   \item{method}{Name of the model used.}
#'   \item{t}{Time point}
#'   \item{ytrue}{True value of the response value.}
#' }
"tvpsvbvar_resp3"