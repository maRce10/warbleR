#' Access Macaulay Library media (meta)data
#' 
#' \code{quer_ml} queries media metadata from \href{http://macaulaylibrary.org}{Macaulay Library database}.
#' @usage quer_ml(qword, download = FALSE, X = NULL, file.name = "sciName", media.type = "a",
#' parallel = 1, path = NULL, pb = TRUE)  
#' @param qword Character vector of length one indicating the genus, or genus and
#'  species, to query Macaulay Library database. For example, \emph{Phaethornis} or \emph{Phaethornis longirostris}. More complex queries will be available in the 
#'  future using search terms that follow the Macaulay Library advance query
#'   syntax.
#' @param download Logical argument. If \code{FALSE} only the media file names and
#'   associated metadata are downloaded. If \code{TRUE}, media files are also downloaded to the working
#'   directory. Default is \code{FALSE}. Note that if the file is already found in the 
#'   working directory (as when the downloading process has been interrupted) it will be skipped. 
#'   Hence, resuming downloading processes will not start from scratch.   
#' @param X Data frame with a 'catalogId' column and any other column listed in the file.name argument. Only the media files listed in the data frame 
#' will be download (\code{download} argument is automatically set to \code{TRUE}). This can be used to select
#' the files to be downloaded based on their attributes.  
#' @param file.name Character vector indicating the tags (or column names) to be included in the sound file names (if download = \code{TRUE}). Several tags can be included. If \code{NULL} only the Macaulay Library catalog identification number ("catalogId") is used. Default is "sciName".
#' @param media.type Character vector indicating the type of media that will be searched for. Three options are available:
#' "a" (audio, default), "v" (video),  and "p" (photo).
#' Note that "catalogId" is always used (whether or not is listed by users) to avoid duplicated names.
#' @param parallel Numeric. Controls whether parallel computing is applied when downloading files.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing). Applied both when getting metadata and downloading files.
#' @param path Character string containing the directory path where the sound files will be saved. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return If X is not provided the function returns a data frame with detailing media file metadata. 
#' @export
#' @name quer_ml
#' @details This function queries for animal behavior media files at the \href{http://macaulaylibrary.org/}{Macaulay Library, Cornell Lab of Ornithology}. It can return recordings metadata
#' or download the associated files.
#'  Files are double-checked after downloading and "empty" files are re-downloaded.
#'  File downloading process can be interrupted and resume later as long as the 
#'  working directory is the same. Note that only the first 100 results can be 
#'  retreived at this time.
#'  
#'  Please read carefully \href{https://www.macaulaylibrary.org/macaulay-library-terms-of-use/}{Macaulay Library's terms of use}
#'  before using any of their data. Users are permitted to:
#' \itemize{
#'   \item View Content online.
#'   \item Print Website pages for non-commercial, personal, educational, and research uses provided that ML is properly cited as the source.
#'   \item Retain copies of specimen record data in digital form for non-commercial, personal, educational and research purposes provided that ML is properly cited as the source.
#'   \item Link to and share Website pages from third-party websites for non-commercial, personal, educational and research purposes only provided that ML is properly cited as the source.
#'   \item Share Content for non-commercial, personal, educational and research purposes provided that ML is properly cited as the source.
#'   \item Any other uses (including but not limited to commercial, promotional, or administrative uses), reproduction,
#'   alteration, modification, public performance or display, uploading or posting onto the internet, transmission, 
#'   redistribution or any other exploitation of the Website or the Content, whether in whole or in part, are prohibited 
#'   without prior written permission.
#' }
#' @seealso \code{\link{quer_xc}}
#' @examples
#' \dontrun{
#' # Set temporary working directory
#' # setwd(tempdir())
#' 
#' # search without downloading
#' df1 <- quer_ml(qword = 'Phaethornis squalidus', download = FALSE)
#' View(df1)
#'
#' # downloading files
#'quer_ml(qword = 'Phaethornis squalidus', download = TRUE)
#'
#' # check folder
#' open_wd()
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu}) 
#last modification on nov-9-2018 (MAS)

quer_ml <- function(qword, download = FALSE, X = NULL, file.name = "sciName", media.type = "a",
                   parallel = 1, path = NULL, pb = TRUE) {
  
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd))
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(quer_ml)
  
  # get warbleR options
  opt.argms <- if(!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0
  
  # rename path for sound files
  names(opt.argms)[names(opt.argms) == "wav.path"] <- "path"
  
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
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else {if (!dir.exists(path)) stop("'path' provided does not exist") else
    setwd(path)
  }  
  
  #check internet connection
  a <- try(RCurl::getURL("https://search.macaulaylibrary.org"), silent = TRUE)
  if (class(a)== "try_error") stop("No connection to Macaulay Library (check your internet connection!)")
  
  if (a == "Could not connect to the database")  stop("Macaulay Library website is apparently down")
  
  # If parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")

  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  file.name <- gsub(" ", "", file.name)

  if (is.null(X) & !is.null(file.name))
  {

    if (any(!(file.name %in%
             c("catalogId", "age", "behaviors", "sex", "locationLine2", "location", "licenseType", "thumbnailUrl", "previewUrl", "largeUrl", "mediaUrl", "userId", "mediaType", "rating", "userDisplayName", "assetId", "sciName", "width", "height","speciesCode", "eBirdChecklistId", "valid","comments", "obsComments", "specimenUrl", "userProfileUrl", "locationLine1", "obsDttm", "collected", "eBirdChecklistUrl", "ratingCount", "recorder", "microphone", "accessories", "specimenIds", "homeArchive", "stimulus", "source","commonName" )))) stop("File name tags don't match column names in the output of this function (see documentation)")
  }

  if (is.null(X))
  {
    
    if (media.type == "a") {
      fls <- "recording(s)"
    fl.xtn <- ".mp3"} else 
      if (media.type == "p") {
        fls <- "photo(s)"
      fl.xtn <- ".jpg"} else
        if (media.type == "v") {
          fls <- "video(s)" 
        fl.xtn <- ".mp4"}
    
    #search recs in ML
    if (pb)
      write(file = "", x = paste("Obtaining", gsub("(s)","", fls, fixed = TRUE), "list..."))
    
    #format JSON
    qword <- gsub("_|-| ", "%20", qword)
  
    base.srch.pth <- paste0("https://search.macaulaylibrary.org/api/v1/search?mediaType=", media.type, "&q=")
    
    #initialize search
    q <- rjson::fromJSON(file = paste0(base.srch.pth, qword))
    
    count <- q$results$count
    
    if (q$results$count == 0) {cat(paste("No", fls, "were found")) 
      download <- FALSE
      }else {
     
    # no more than 100 at the time currently  
    if (q$results$count > 100) q$results$count <- 100
      
    if (q$results$count > 30)   q <- rjson::fromJSON(file = paste0(base.srch.pth, qword, "&count=", q$results$count))
       
    # no more than 100 at the time currently  
    if (q$results$count > 100) q$results$count <- 100  
    
      ### loop over pages
      # set clusters for windows OS
      if (Sys.info()[1] == "Windows" & parallel > 1)
        cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
        
        f <- pbapply::pblapply(X = 1:q$results$count, cl = cl, FUN = function(y)
      {
          itm <- q$results$content[[y]]
          itm <- itm[which(names(itm) != "subjectData")]
          
          itm <- lapply(itm, function(x) if(!is.atomic(x) | is.null(x)) return(NA) else
            return(x))
          
          itm <- as.data.frame(itm)
          
          return(itm)
          }
      ) 
        
    # save results in a single data frame  
    results <- do.call(rbind, f)
    
    # remove subspecies info from sciName
    results$sciName <- sapply(as.character(results$sciName), function(x) paste(strsplit(x, " ")[[1]][1:2], collapse = " "), USE.NAMES = FALSE)
    
    #remove duplicates
    results <- results[!duplicated(results$catalogId), ]
    
    if (pb)
      if(count == q$results$count)
      write(file = "", x = paste(q$results$count, fls, "found!")) else    write(file = "", x = paste(count, fls, " found, but only the first 100 can be retreived at this time"))
    } 
  }
else {
    #stop if X is not a data frame
    if (class(X) != "data.frame") stop("X is not a data frame")

    #stop if the basic columns are not found
    if (!is.null(file.name))
    {if (any(!c(file.name, "catalogId") %in% colnames(X)))
      stop(paste(paste(c(file.name, "catalogId")[!c(file.name, "catalogId") %in% names(X)], collapse=", "), "column(s) not found in data frame"))} else
        if (!"catalogId" %in% colnames(X))
          stop("catalogId column not found in data frame")

    download <- TRUE
    results <- X
  }
  
  #download recordings
  if (download) {
    if (any(file.name == "catalogId")) file.name <- file.name[-which(file.name == "catalogId")]

    if (!is.null(file.name))  {  if (length(which(names(results) %in% file.name)) > 1)
      fn <- apply(results[ ,which(names(results) %in% file.name)], 1 , paste , collapse = "-" ) else
        fn <- results[ ,which(names(results) %in% file.name)]
      results$sound.files <- paste(paste(fn, paste0("ML", results$catalogId), sep = "-"), fl.xtn, sep = "")
    } else
      results$sound.files <- paste0("ML", results$catalogId, fl.xtn)

    results$sound.files <- gsub(" |/", "-", results$sound.files)

    xcFUN <-  function(W, x){
      if (!file.exists(W$sound.files[x]))
        download.file(url = as.character(W$mediaUrl[x]), destfile = W$sound.files[x],
                      quiet = TRUE,  mode = "wb", cacheOK = TRUE,
                      extra = getOption("download.file.extra"))
      return (NULL)
    }

    # set clusters for windows OS
    if (pb)
      write(file = "", x = "Downloading files...")
    if (Sys.info()[1] == "Windows" & parallel > 1)
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel

       a1 <- pbapply::pblapply(X = 1:nrow(results), cl = cl, FUN = function(x)
  {
      xcFUN(W = results, x)
  })

if (pb) write(file = "", x ="double-checking downloaded files")

   #check if some files have no data
    fl <- list.files(pattern = fl.xtn)
    size0 <- fl[file.size(fl) == 0]

    #if so redo those files
    if (length(size0) > 1)
  {  Y <- results[results$sound.files %in% size0, ]
     unlink(size0)

     # set clusters for windows OS
     if (Sys.info()[1] == "Windows" & parallel > 1)
       cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel


    a1 <- pbapply::pblapply(X = 1:nrow(Y), cl = cl, FUN = function(x)
  {
      xcFUN(Y, x)
  })

     }
  }
  
 if (is.null(X)) if (as.numeric(q$results$count) > 0) {
   
   results <- results[, na.omit(match(c("sciName","commonName" , "catalogId", "sound.files", "locationLine2", "location", "behaviors", "sex", "recorder", "microphone", "accessories", "licenseType", "age", "thumbnailUrl", "previewUrl", "largeUrl", "mediaUrl", "userId", "mediaType", "rating", "userDisplayName", "assetId", "width", "height","speciesCode", "eBirdChecklistId", "valid","comments", "obsComments", "specimenUrl", "userProfileUrl", "locationLine1", "obsDttm", "collected", "eBirdChecklistUrl", "ratingCount", "specimenIds", "homeArchive", "stimulus", "source"), names(results)))]
   
   if (media.type != "a") names(results)[names(results) == "sound.files"] <- "media.files"
   
   return(droplevels(results))
   }
  
   }
