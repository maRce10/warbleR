#' Matrix listing selections to be compared by \code{\link{xcorr}} 
#' 
#' A character matrix with 2 columns indicating the selections to be compared (column 1 vs column 2) by \code{\link{xcorr}}. The first column contain the ID of the selection, which is given by combining the 'sound.files' and 'selec' columns of 'X', separated by '-' (i.e. \code{paste(X$sound.files, X$selec, sep = "-")}). The selection id's refer to those on the example data "lbh_selec_table". The second column refers to the sound files in which to search for the templates.
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
#' @usage data(comp_matrix)
#' 
#' @source Marcelo Araya Salas, warbleR 
#' 
#' @description \code{comp_matrix} is a character matrix with 2 columns indicating the selections to be compared (column 1 vs column 2) by \code{\link{xcorr}}.
#'
"comp_matrix" 
