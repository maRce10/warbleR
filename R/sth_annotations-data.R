#' Example data frame of annotations from a Scale-throated hermit song (i.e. selection table).
#'
#' A data frame containing the start, end, low and high frequency of
#' \emph{Phaethornis eurynome} (Scale-throated Hermit). The correspondent sound file can be found at \url{https://xeno-canto.org/15607}. The song of this species consists of two frequency modulated elements separated by a short gap. This annotation data set includes labels for 'song' and 'element' and aims to provide example data for functions working at higher herarchical levels of organization in the acoustic signals.   
#'
#' @format A data frame with 46 rows and 9 columns: \describe{
#'  \item{sound.files}{sound file names}
#'  \item{selec}{selection numbers within recording}
#'  \item{channel}{channel in which signal is found}
#'  \item{start}{start times of selected signal}
#'  \item{end}{end times of selected signal}
#'  \item{bottom.freq}{lower limit of frequency range}
#'  \item{top.freq}{upper limit of frequency range}
#'  \item{song}{song ID label}
#'  \item{element}{element ID label}
#' }
#'
#' @usage data(sth_annotations)
#'
#' @source Marcelo Araya-Salas, warbleR
#'
#' @description \code{sth_annotations} is a data frame containing the start, end, low and high frequency and song and element labels of Scale-throated Hermit \emph{Phaethornis eurynome} songs.
#'
"sth_annotations"
