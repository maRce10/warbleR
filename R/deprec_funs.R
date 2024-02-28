##############################################################################################################
#' deprecated functions and function names
#'
#' @keywords internal
#' @details functions that have been deprecated or moved to other packages.
#' @export

ffDTW <- ff_DTW <- function(...) warning2(x = "This function has been deprecated. DTW distances between fundamental frequency contours can be calculated using `freq_DTW`")

freq_ts <- ff_ts <- function(...) warning2(x = "This function has been deprecated. Fundamental frequency contours can be calculated using `freq_ts`")

dfts <- df_ts <- function(...) warning2(x = "This function has been deprecated. Dominant frequency contours can be calculated using `freq_ts`")

sp.en.ts <- entropy_ts <- function(...) warning2(x = "This function has been deprecated. Spectral entropy contours can be calculated using `freq_ts`")

quer_ml <- function(...) warning2(x = "This function has been removed temporarily due to API changes at Macaulay Library")

xcorr.graph <- function(...) warning2(x = "This function has been deprecated as it was not compatible with changes to improve performance in 'xcorr()' (use corrplot package instead)")

manualoc <- manual_loc <- function(...) warning2(x = "This function has been deprecated. Try Raven Software (Cornell Lab of Ornithology) or Audacity to manually annotate. The Rraven package (imp_raven) can be used for importing Raven annotations: try `install.packages('Rraven')`")

find_annotations <- function(...) warning2(x = "This function has been removed due to API changes at audioblast.org")

optimize_auto_detec <- function(...) warning2(x = "This function has been deprecated. Look at the package ohun for signal detection tools")
