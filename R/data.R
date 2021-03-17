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