#' Example data frame of a selection table including all signals of interests.
#' 
#' A data frame containing the start, end, low and high frequency of 
#' \emph{Phaethornis longirostris} (Long-billed Hermit) songs from the 
#' example sound files included in this package. Similar than 'lbh_selec_table'.
#' but contains selections for all songs in the recordings.
#' 
#' @format A data frame with 15 rows and 7 variables: \describe{ 
#'  \item{sound.files}{recording names}
#'  \item{channel}{channel in which signal is found}
#'  \item{selec}{selection numbers within recording}
#'  \item{start}{start times of selected signal}
#'  \item{end}{end times of selected signal}
#'  \item{bottom.freq}{lower limit of frequency range}
#'  \item{top.freq}{upper limit of frequency range}
#' }
#' 
#' @usage data(lbh_selec_reference)
#' 
#' @source Marcelo Araya-Salas, warbleR 
#' 
#' @description \code{lbh_selec_model} is a data frame containing the start, end, low and high frequency all songs in the recordings. Mostly to be used as an example in  \code{\link{diagnose_detection}}.
#' 
"lbh_selec_reference" 
