#' Example data frame of selections (i.e. selection table).
#'
#' A data frame containing the start, end, low and high frequency of
#' \emph{Phaethornis longirostris} (Long-billed Hermit) songs from the
#' example sound files included in this package. Similar than 'lbh_selec_table'.
#' but it contains only 2 selections.
#'
#' @format A data frame with 2 rows and 7 columns: \describe{
#'  \item{sound.files}{sound file names}
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
#' @source Marcelo Araya-Salas, warbleR
#'
#' @description \code{lbh_selec_table2} is a data frame containing the start, end, low and high frequency of 2 selections. Mostly to be used as an example in  \code{\link{find_peaks}}.
#'
"lbh_selec_table2"
