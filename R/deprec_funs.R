##############################################################################################################
#' deprecated functions and function names
#'
#' @keywords internal
#' @details functions that have been deprecated or moved to other packages.
#' @export

imp.raven <- function(...) write(file = "", x = "This function has been moved to the Rraven package (imp_raven) \n try `install.packages('Rraven')`")

imp.syrinx <- function(...) write(file = "", x = "This function has been moved to the Rraven package (imp_syrinx) \n try `install.packages('Rraven')`")

quer_ml <- function(...) write(file = "", x = "This function has been removed temporarily due to API changes at Macaulay Library")

xcorr.graph <- function(...) write(file = "", x = "This function has been depracated as it was not compatible with changes to improve performance in 'xcorr()' (use corrplot package instead)")
