#' Access 'audioblast.org' annotation data base
#' 
#' \code{find_annotations} downloads sound file annotations and metadata from \href{https://audioblast.org/annotations/}{audioblast.org}.
#' @usage find_annotations(qword, parallel = 1, pb = TRUE)  
#' @param qword Character vector of length one indicating  the scientific name of the species to search for at audioblast's annotations database. For example, \emph{Phaethornis longirostris}. 
#' @param parallel Numeric. Controls whether parallel computing is applied when downloading mp3 files.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return A data frame with the annotation information.  
#' @export
#' @name find_annotations
#' @details This function queries for annotations on acoustic media in the open-access
#' online repository \href{https://audioblast.org/annotations/}{audioblast.org}.
#' @seealso \code{\link{quer_xc}} 
#' @examples
#' ann <- find_annotations(qword = "Amazilia brevirostris")
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com}) 
#last modification on dec-24-2019 (MAS)

find_annotations <- function(qword, parallel = 1, pb = TRUE) {
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(find_annotations)
  
  # get warbleR options
  opt.argms <- if(!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0
  
  # remove options not as default in call and not in function arguments
  opt.argms <- opt.argms[!sapply(opt.argms, is.null) & names(opt.argms) %in% argms]
  
  # get arguments set in the call
  call.argms <- as.list(base::match.call())[-1]
  
  # remove arguments in options that are in call
  opt.argms <- opt.argms[!names(opt.argms) %in% names(call.argms)]
  
  # set options left
  if (length(opt.argms) > 0)
    for (q in 1:length(opt.argms))
      assign(names(opt.argms)[q], opt.argms[[q]])
  
  #check internet connection
  a <- try(RCurl::getURL("https://api.audioblast.org"), silent = TRUE)
  if (is(a, "try-error")) stop("No connection to api.audioblast.org (check your internet connection!)") else rm(a)
  
  # If parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
    #search annotations 
    if (pb)
      write(file = "", x = "Obtaining annotations...")
    
    #format JSON
    # qword <- "Amazilia brevirostris"
    qword <- gsub(" ", "%20", qword)
    
    #initialize search
    q <- rjson::fromJSON(file = paste0("https://api.audioblast.org/annotations/?taxon=", qword))
    
    if (length(q) == 0) cat("No annotations were found") else {
    
      
      q <- lapply(q, function(x){
        
        x[sapply(x, is.null)] <- NA
        
        data.frame(x)
        
      })
      
      # determine all column names in all pages
      cnms <- unique(unlist(lapply(q, names)))    
      
      # add columns that are missing to each selection table
      q2 <- lapply(q, function(X)
      {
        nms <- names(X)
        if (length(nms) != length(cnms))  
          for(i in cnms[!cnms %in% nms]) {
            X <- data.frame(X,  NA, stringsAsFactors = FALSE, check.names = FALSE)
            names(X)[ncol(X)] <- i
          }
        return(X)
      })  

        # save results in a single data frame  
        results <- do.call(rbind, q2)
        
        # convert factors to characters
        indx <- sapply(results, is.factor)
        results[indx] <- lapply(results[indx], as.character)
        
        if (pb)
          write(file = "", x = paste0(nrow(results), " annotations found!"))
    
        return(results)
        } 

}
