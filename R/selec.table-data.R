#' Data frame of selections (i.e. selection table).
#' 
#' A data frame containing the start, end, low and hig frequency of 
#' \emph{Phaethornis longirostris} (Long-billed Hermit) songs from the 
#' example sound files included in this package.
#' 
#' @format A data frame with 11 rows and 6 variables: \describe{ 
#'  \item{sound.files}{recording names}
#'  \item{selec}{selection numbers within recording}
#'  \item{start}{start times of selected signal}
#'  \item{end}{end times of selected signal}
#'  \item{low.f}{lower limit of frequency range}
#'  \item{high.f}{upper limit of frequency range}
#'  \item{sel.comment}{selection comments}
#'  \item{rec.comment}{recording comments}
#' }
#' 
#' @usage data(selec.table)
#' 
#' @source Marcelo Araya Salas, warbleR 
"selec.table"
