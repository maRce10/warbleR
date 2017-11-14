#' Check if object is of class "selection.table"
#' 
#' \code{is.selection.table} Check if the object belongs to the class "selection.table"
#' @usage is.selection.table(X)
#' @param X R object. 
#' @return A logical argument indicating whether the object class is 'selection.table'
#' @seealso \code{\link{make.selection.table}}
#' @export
#' @name is.selection.table
#' @examples
#' \dontrun{
#' # First set temporary folder
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#' 
#' is.selection.table(selec.table)
#' 
#' setwd(tempdir())
#' 
#' writeWave(Phae.long1,"Phae.long1.wav")
#' writeWave(Phae.long2,"Phae.long2.wav")
#' writeWave(Phae.long3,"Phae.long3.wav")
#' writeWave(Phae.long4,"Phae.long4.wav")
#' 
#' st <- make.selection.table(selec.table)
#' 
#' is.selection.table(st)
#' 
#' class(st)
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on nov-10-2017 (MAS)

is.selection.table <- function(X) inherits(X, "selection.table")
