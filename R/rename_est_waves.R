#' Rename wave objects and associated metadata in extended selection tables
#'
#' \code{rename_est_waves} rename wave objects and associated metadata in extended selection tables
#' @export rename_est_waves
#' @param X object of class 'extended_selection_table'.
#' @param new.sound.files Character vector of length equals to the number of wave objects in the extended selection table (\code{length(attr(X, "wave.objects"))}).Specifies the new names to be used for wave objects and sound file column. Note that this will rename wave objects and associated attributes and data in 'X'.
#' @param new.selec Numeric or character vector of length equals to the number of rows in 'X' to specify the 'selec' column labels. Default is \code{NULL}. If not provided the 'selec' column is kept unchanged. Note that the combination of 'sound.files' and 'selec' columns must produce unique IDs for each selection (row).
#' @return An extended selection table with rename sound files names in data frame and attributes. The function adds columns with the previous sound file names (and 'selec' if provided).
#' @family extended selection table manipulation
#' @name rename_est_waves
#' @details This function allow users to change the names of 'sound.files' and 'selec' columns in extended selection tables. These names can become very long after manipulations used to produce extended tables.
#' @examples{
#' data("lbh.est")
#'
#' # order by sound file name
#' lbh.est <- lbh.est[order(lbh.est$sound.files),]
#'
#' # create new sound file name
#' nsf <- sapply(strsplit(lbh.est$sound.files, ".wav",fixed = TRUE), "[",1)
#'
#' slc <- vector(length = nrow(lbh.est))
#' slc[1] <- 1
#'
#' for(i in 2:length(slc))
#' if (nsf[i - 1] == nsf[i]) slc[i] <- slc[i - 1] + 1 else
#' slc[i] <- 1
#'
#' nsf <- paste(nsf, slc, sep = "_")
#'
#' # rename sound files
#' Y <- rename_est_waves(X = lbh.est, new.sound.files = nsf)
#' }
#'
#' @references
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' 
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

rename_est_waves <- function(X, new.sound.files, new.selec = NULL) {
  # check length of new.sound.files
  if (length(new.sound.files) != length(attr(X, "wave.objects"))) stop2("length of 'new.sound.files' must be equal to the number of wave objects in 'X' (length(attr(X, 'wave.objects')))")

  # order check results attributes as in X
  attributes(X)$check.results <- attributes(X)$check.results[match(paste(X$sound.files, paste(X$selec)), paste(attributes(X)$check.results$sound.files, attributes(X)$check.results$selec)), ]

  # save old name
  X$old.sound.file.name <- attributes(X)$check.results$old.sound.file.name <- X$sound.files

  # old and new names in data frame
  new_names <- data.frame(old = names(attributes(X)$wave.objects), new = new.sound.files)

  # rename columns in X
  X$sound.files <- attributes(X)$check.results$sound.files <- sapply(X$old.sound.file.name, function(i) new_names$new[new_names$old == i])

  # check and rename selec
  if (!is.null(new.selec)) {
    X$old.selec <- attributes(X)$check.results$old.selec <- X$selec
    X$selec <- attributes(X)$check.results$selec <- new.selec
  }

  # check that every row has a unique ID
  if (attributes(X)$by.song[[1]] & any(duplicated(paste(X$sound.files, X$selec)))) stop2("new.sound.files + 'selec' don't generate unique labels for each row/selection")

  if (!attributes(X)$by.song[[1]] & any(duplicated(new.sound.files))) stop2("new.sound.files don't have unique labels for each row/selection")

  # rename wave objects
  names(attributes(X)$wave.objects) <- sapply(names(attributes(X)$wave.objects), USE.NAMES = FALSE, function(x) {
    X$sound.files[X$old.sound.file.name == x][1]
  })


  return(X)
}