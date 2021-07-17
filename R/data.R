#' @title Medium data set from OG et al 2020, rescaled to unit variance
#'
#' @description Starts at 1964Q4, ends at 2019Q1..
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


#' @title Additional decision maker data.
#'
#' @description Some data about how hyped americans are about "the
#'   economy." Starts at X and ends at Y.
#' 
#' @format A data frame with 293 rows and 2 variables:
#' \describe{
#'   \item{USSURV1055}{This is the one we use.}
#'   \item{USSURV1459}{This is the one we do not use.}
#' }
"dmdat"


#' @title Agent predictions for response 1
#'
#' @description Can be generated from oscbvar::macrodata, but is included here
#'   since that takes a lot of time. Agent predictions of GDP.
#' 
#' @format A data frame with 616 rows and 5 columns.
#' \describe{
#'   \item{pmean}{Predictive mean.}
#'   \item{lpdens}{Log predictive density.}
#'   \item{method}{Aggregation method.}
#'   \item{t}{Time.}
#'   \item{ytrue}{True value of the response, not really implemented.}
#' }
"atomdat_1"


#' @title Agent predictions for response 2
#'
#' @description Can be generated from oscbvar::macrodata, but is included here
#'   since that takes a lot of time. Agent predictions of GDPTCPI.
#' 
#' @format A data frame with 616 rows and 5 columns.
#' \describe{
#'   \item{pmean}{Predictive mean.}
#'   \item{lpdens}{Log predictive density.}
#'   \item{method}{Aggregation method.}
#'   \item{t}{Time.}
#'   \item{ytrue}{True value of the response, not really implemented.}
#' }
"atomdat_2"


#' @title Agent predictions for response 3
#'
#' @description Can be generated from oscbvar::macrodata, but is included here
#'   since that takes a lot of time. Agent predictions of FEDFUNDS.
#' 
#' @format A data frame with 616 rows and 5 columns.
#' \describe{
#'   \item{pmean}{Predictive mean.}
#'   \item{lpdens}{Log predictive density.}
#'   \item{method}{Aggregation method.}
#'   \item{t}{Time.}
#'   \item{ytrue}{True value of the response, not really implemented.}
#' }
"atomdat_3"

#' @title Bikeshare data, daily
#' 
#' @description Data on bikeshare, daily version, with a bunch of covariates.
#' 
#' @format A data frame with 730 rows and 13 columns
#' \describe{
#'  \item{cnt}{Count of total rental bikes (used).}
#'  \item{t}{Time.}
#'  \item{yr}{Year.}
#'  \item{mnth}{Month.}
#'  \item{workingday}{Weekday but not a holiday.}
#'  \item{temp}{Normalized temperature in celsius (how is this in 
#'      celsius...).}
#'  \item{hum}{Normalized humidity. I can't see how this is normalized.}
#'  \item{windspeed}{"Normalized" windspeed.}
#'  \item{sandy1}{Hurricane day 1.}
#'  \item{sandy2}{Hurricane day 2.}
#'  \item{weather_1}{1: clear, few clouds, partly cloudy. Baseline
#'      is light snow, light rain and thunderstorm and scattered clouds.}
#'  \item{weather_2}{2: mist and cloudy, mist and broken clouds.}
#'  \item{cnt_l}{Lagged cnt.}
#' }
"bikes_d"


#' @title Bikeshare data, daily
#' 
#' @description Data on bikeshare, daily version, with a bunch of covariates.
#' 
#' @format A data frame with 730 rows and 13 columns
#' \describe{
#'  \item{logcnt}{Log of cnt.}
#'  \item{t}{Time.}
#'  \item{yr}{Year.}
#'  \item{mnth}{Month.}
#'  \item{workingday}{Weekday but not a holiday.}
#'  \item{temp}{Normalized temperature in celsius (how is this in 
#'      celsius...).}
#'  \item{hum}{Normalized humidity. I can't see how this is normalized.}
#'  \item{windspeed}{"Normalized" windspeed.}
#'  \item{sandy1}{Hurricane day 1.}
#'  \item{sandy2}{Hurricane day 2.}
#'  \item{weather_1}{1: clear, few clouds, partly cloudy. Baseline
#'      is light snow, light rain and thunderstorm and scattered clouds.}
#'  \item{weather_2}{2: mist and cloudy, mist and broken clouds.}
#'  \item{logcnt_l}{Lagged log count.}
#' }
"bikes_d_log"