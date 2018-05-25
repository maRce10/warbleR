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
#' open_wd()
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on apr-16-2018 (MAS)

open_wd <- function(path = getwd(), verbose = TRUE){
  
  #check path to working directory
  if (!dir.exists(path)) stop("'path' provided does not exist") 
  
    if (.Platform['OS.type'] == "windows"){
    shell.exec(path)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), path), ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  if (verbose)
  print(paste(path, "opened in file browser"))
}
