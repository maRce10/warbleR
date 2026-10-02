#' Maps of 'Xeno-Canto' recordings by species (deprecated)
#'
#' \code{map_xc} has been deprecated. Use \code{suwo::map_locations()} from the package
#' \href{https://docs.ropensci.org/suwo/}{suwo} instead, which maps media records from
#' 'Xeno-Canto' and other online repositories.
#' @param ... Ignored. Kept so that existing code calling \code{map_xc()} gets an informative warning instead of an error.
#' @return \code{NULL} (invisibly).
#' @export
#' @name map_xc
#' @keywords internal
#' @details This function has been deprecated as access to online nature media repositories (including 'Xeno-Canto') is now provided by the package \href{https://docs.ropensci.org/suwo/}{suwo}. Metadata obtained with \code{suwo::query_xenocanto()} (or any other suwo query function) can be mapped with \code{suwo::map_locations()}.
#' @seealso \code{\link{query_xc}}
#' @references
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#'
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}) and Grace Smith Vidaurre

map_xc <- function(...) {
  .Deprecated(
    new = "suwo::map_locations",
    package = "warbleR",
    msg = "map_xc() has been deprecated. Use `map_locations()` from the package suwo instead (https://docs.ropensci.org/suwo/)"
  )

  return(invisible(NULL))
}
