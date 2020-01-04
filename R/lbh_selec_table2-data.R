#' Data frame of selections 
#' 
#' A data frame containing the start, end, low and high frequency of 
#' \emph{Phaethornis longirostris} (Long-billed Hermit) songs from the 
#' example sound files included in this package. SImilar than 'lbh_selec_table'.
#' but it contains 1 selection from 2 sound files (rows 1 and 2) and 2 additional 
#' selections spaning the whole duration of the same sound files.
#' 
#' @format A data frame with 11 rows and 6 variables: \describe{ 
#'  \item{sound.files}{recording names}
#'  \item{channel}{channel in which signal is found}
#'  \item{selec}{selection numbers within recording}
#'  \item{start}{start times of selected signal}
#'  \item{end}{end times of selected signal}
#'  \item{bottom.freq}{lower limit of frequency range}
#'  \item{top.freq}{upper limit of frequency range}
#' }
#' 
#' @usage data(lbh_selec_table2)
#' 
#' @source Marcelo Araya Salas, warbleR 
#' 
#' @description \code{lbh_selec_table2} is a data frame containing the start, end, low and high frequency of 4 selections. Mostly to be used as an example in  \code{\link{find_peaks}}.
#' 
"lbh_selec_table2" 
