#' Access 'Xeno-Canto' recordings and metadata (deprecated)
#'
#' \code{query_xc} has been deprecated. Use the package
#' \href{https://docs.ropensci.org/suwo/}{suwo} instead: \code{suwo::query_xenocanto()}
#' retrieves 'Xeno-Canto' metadata and \code{suwo::download_media()} downloads the recordings.
#' @param ... Ignored. Kept so that existing code calling \code{query_xc()} gets an informative warning instead of an error.
#' @return \code{NULL} (invisibly).
#' @export
#' @name query_xc
#' @keywords internal
#' @details This function has been deprecated as access to 'Xeno-Canto' (and other online nature media repositories such as Macaulay Library, iNaturalist, GBIF and WikiAves) is now provided by the package \href{https://docs.ropensci.org/suwo/}{suwo}. Note that 'Xeno-Canto' queries now require an API key (see \code{?suwo::query_xenocanto}). For example:
#' \preformatted{
#' # install.packages("suwo")
#' library(suwo)
#'
#' # search for metadata
#' p_anth <- query_xenocanto(species = "Phaethornis anthophilus")
#'
#' # download recordings
#' download_media(metadata = p_anth, path = tempdir())
#' }
#' @seealso \code{\link{map_xc}}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

query_xc <- function(...) {
  .Deprecated(
    new = "suwo::query_xenocanto",
    package = "warbleR",
    msg = "query_xc() has been deprecated. Use `query_xenocanto()` and `download_media()` from the package suwo instead (https://docs.ropensci.org/suwo/)"
  )

  return(invisible(NULL))
}


####

#' alternative name for \code{\link{query_xc}}
#'
#' @keywords internal
#' @details Deprecated. See \code{\link{query_xc}}.
#' @export

querxc <- query_xc
