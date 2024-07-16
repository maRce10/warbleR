#' Sort columns in a more intuitive order
#'
#' \code{sort_colms} sorts selection table columns in a more intuitive order.
#' @param X Data frame containing columns for sound file (sound.files), selection
#' (selec), start and end time of signals ('start' and 'end') and low and high
#' frequency ('bottom.freq' and 'top.freq', optional). See the example data 'lbh_selec_table'.
#' @return  The same data as in the input data frame but with the most relevant information
#' for acoustic analysis located in the first columns.
#' @details The function returns the data from the input data frame with the most relevant information
#' for acoustic analysis located in the first columns. The priority order for column names is: "sound.files", "channel", "selec", "start", "end", "top.freq", and "bottom.freq".
#' @export
#' @name sort_colms
#' @examples
#' library(warbleR)
#' data("lbh_selec_table")
#'
#' # mess column order
#' lbh_selec_table <- lbh_selec_table[, sample(seq_len(ncol(lbh_selec_table)))]
#'
#' # check names
#' names(lbh_selec_table)
#'
#' lbh_selec_table <- sort_colms(X = lbh_selec_table)
#'
#' # check names again
#' names(lbh_selec_table)
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on mar-10-2018

sort_colms <- function(X) {
  if (any(duplicated(names(X)))) stop(" Duplicated column names must be fixed first")

  mtch <- match(c("sound.files", "channel", "selec", "start", "end", "top.freq", "bottom.freq"), names(X))

  mtch <- mtch[!is.na(mtch)]

  X <- X[, c(mtch, setdiff(seq_len(ncol(X)), mtch))]

  return(X)
}
