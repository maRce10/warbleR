#' Access 'Xeno-Canto' recordings and metadata
#'
#' \code{query_xc} It downloads recordings and metadata from \href{https://www.xeno-canto.org/}{Xeno-Canto}. DEPRECATED.
#' @param qword Character vector of length one indicating the genus, or genus and
#'  species, to query 'Xeno-Canto' database. For example, \emph{Phaethornis} or \emph{Phaethornis longirostris}.
#'  More complex queries can be done by using search terms that follow the
#'  xeno-canto advance query syntax. This syntax uses tags to search within a particular aspect of the recordings
#'  (e.g. country, location, sound type). Tags are of the form tag:searchterm'. For instance, 'type:song'
#'  will search for all recordings in which the sound type description contains the word 'song'.
#'  Several tags can be included in the same query. The query "phaethornis cnt:belize' will only return
#'  results for birds in the genus \emph{Phaethornis} that were recorded in Belize. Queries are case insensitive. Make sure taxonomy related tags (Genus or scientific name) are found first in multi-tag queries. See \href{https://www.xeno-canto.org/help/search}{Xeno-Canto's search help} for a full description and see examples below
#'  for queries using terms with more than one word.
#' @param download Logical argument. If \code{FALSE} only the recording file names and
#'   associated metadata are downloaded. If \code{TRUE}, recordings are also downloaded to the working
#'   directory as .mp3 files. Default is \code{FALSE}. Note that if the recording is already in the
#'   working directory (as when the downloading process has been interrupted) it will be skipped.
#'   Hence, resuming downloading processes will not start from scratch.
#' @param X Data frame with a 'Recording_ID' column and any other column listed in the file.name argument. Only the recordings listed in the data frame
#' will be download (\code{download} argument is automatically set to \code{TRUE}). This can be used to select
#' the recordings to be downloaded based on their attributes.
#' @param file.name Character vector indicating the tags (or column names) to be included in the sound file names (if download = \code{TRUE}). Several tags can be included. If \code{NULL} only the 'Xeno-Canto' recording identification number ("Recording_ID") is used. Default is c("Genus", "Specific_epithet").
#' Note that recording id is always used (whether or not is listed by users) to avoid duplicated names.
#' @param parallel Numeric. Controls whether parallel computing is applied when downloading mp3 files.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing). Applied both when getting metadata and downloading files.
#' @param path Character string containing the directory path where the sound files will be saved.
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return If X is not provided the function returns a data frame with the following recording information: recording ID, Genus, Specific epithet, Subspecies, English name, Recordist, Country, Locality, Latitude, Longitude, Vocalization type, Audio file, License, URL, Quality, Time, Date. Sound files in .mp3 format are downloaded into the working directory if download = \code{TRUE} or if X is provided; a column indicating the  names of the downloaded files is included in the output data frame.
#' @export
#' @name query_xc
#' @details This function is has been deprecated. To obtaining nature media from online repositories use the package suwo (https://github.com/maRce10/suwo).
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

query_xc <- function(qword, download = FALSE, X = NULL, file.name = c("Genus", "Specific_epithet"),
                     parallel = 1, path = NULL, pb = TRUE) {

  .Deprecated(msg = "This function has been deprecated. To obtaining nature media from online repositories use the package suwo (https://github.com/maRce10/suwo)")
    
return(NULL)
  }


####

#' alternative name for \code{\link{query_xc}}
#'
#' @keywords internal
#' @details see \code{\link{query_xc}} for documentation. \code{\link{querxc}} will be deprecated in future versions.
#' @export

querxc <- query_xc
