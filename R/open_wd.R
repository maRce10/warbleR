#' Open working directory
#' 
#' \code{open_wd} opens the working directory in the default file browser.
#' @usage open_wd(path = getwd(), verbose = TRUE)
#' @param path Directory path to be opened. By default it's the working directory. 
#' 'wav.path' set by \code{\link{warbleR_options}} is ignored in this case.
#' @param verbose Logical to control whether the 'path' is printed in the console. 
#' @family data manipulation
#' @seealso \code{\link{move.imgs}} 
#' @export
#' @name open_wd
#' @details The function opens the working directory using the default file browser 
#' and prints the working directory in the R console. This function aims to simplify
#' the manipulation of sound files and other files produced by many of the \code{\link{warbleR}} function.
#' @examples
#' {
#' \donttest{open_wd()}
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#last modification on apr-16-2018 (MAS)

open_wd <- function(path = getwd(), verbose = TRUE){
  
  #check path to working directory
  if (!dir.exists(path)) stop("'path' provided does not exist") else
    path <- normalizePath(path)
  
    if (.Platform['OS.type'] == "windows"){
    shell.exec(path)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), path), ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  if (verbose)
  print(paste(path, "opened in file browser"))
}
