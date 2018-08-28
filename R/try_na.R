#' Wrapper for "try" function
#' 
#' \code{try_na} silly wrapper for \code{\link[base]{try}} function that returns an NA if an error is found.
#' @usage try_na(expr, silent = TRUE, outFile)
#' @param expr An R expression to try.
#' @param silent Logical to control whether the report of error messages is suppressed. Default is \code{TRUE}. 
#' @param outFile	A connection, or a character string naming the file to print to
#' (via cat(*, file = outFile)); used only if silent is false, as by default.
#' @return Returns an `NA` if any error occurs during the evaluation of a expression.
#' If not, it will return the result of the evaluation.
#' @details This is a silly wrapper on \code{\link[base]{try}} that returns an `NA` if any error occurs during the evaluation of a expression. See \code{\link[base]{try}} for details.
#' @export
#' @name try_na
#' @examples{
#' # try a function that does not exists to produce an error
#' try_na(crazy78(12))
#' 
#' # try a real function (no error)
#' try_na(mean(1:5))
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on apr-17-2018 (MAS)

try_na <- function(expr, silent = TRUE, outFile){
  out <- try(expr = expr, silent = silent, outFile = outFile)
if (class(out) == "try-error") return(NA) else return(out)
  }
