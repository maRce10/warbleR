#' Access Macaulay Library media metadata
#' 
#' \code{quer_ml} downloads recordings and metadata from Macaulay Library database (\url{http://macaulaylibrary.org/}).
#' @usage quer_ml(qword, download = FALSE, X = NULL, file.name = "sciName", 
#' parallel = 1, path = NULL, pb = TRUE)  
#' @param qword Character vector of length one indicating the genus, or genus and
#'  species, to query Macaulay Library database. For example, \emph{Phaethornis} or \emph{Phaethornis longirostris}. More complex queries will be available in the 
#'  future using search terms that follow the Macaulay Library advance query
#'   syntax.
#' @param download Logical argument. If \code{FALSE} only the recording file names and
#'   associated metadata are downloaded. If \code{TRUE}, recordings are also downloaded to the working
#'   directory as .mp3 files. Default is \code{FALSE}. Note that if the recording is already in the 
#'   working directory (as when the downloading process has been interrupted) it will be skipped. 
#'   Hence, resuming downloading processes will not start from scratch.   
#' @param X Data frame with a 'catalogId' column and any other column listed in the file.name argument. Only the recordings listed in the data frame 
#' will be download (\code{download} argument is automatically set to \code{TRUE}). This can be used to select
#' the recordings to be downloaded based on their attributes.  
#' @param file.name Character vector indicating the tags (or column names) to be included in the sound file names (if download = \code{TRUE}). Several tags can be included. If \code{NULL} only the Macaulay Library catalog identification number ("catalogId") is used. Default is "sciName".
#' Note that "catalogId" is always used (whether or not is listed by users) to avoid duplicated names.
#' @param parallel Numeric. Controls whether parallel computing is applied when downloading mp3 files.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing). Applied both when getting metadata and downloading files.
#' @param path Character string containing the directory path where the sound files will be saved. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return If X is not provided the function returns a data frame with the following recording information: recording ID, Genus, Specific epithet, Subspecies, English name, Recordist, Country, Locality, Latitude, Longitude, Vocalization type, Audio file, License, URL, Quality, Time, Date. Sound files in .mp3 format are downloaded into the working directory if download = \code{TRUE} or if X is provided; a column indicating the  names of the downloaded files is included in the output data frame.  
#' @export
#' @name quer_ml
#' @details This function queries for animal sound recordings at the Macaulay Library, Cornell Lab of Ornithology (\url{http://macaulaylibrary.org/}). It can return recordings metadata
#' or download the associated sound files.
#'  Files are double-checked after downloading and "empty" files are re-downloaded.
#'  File downloading process can be interrupted and resume later as long as the 
#'  working directory is the same. Note that only the first 100 results can be 
#'  retreived at this time.
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
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu}) 
#last modification on nov-9-2018 (MAS)

quer_ml <- function(qword, download = FALSE, X = NULL, file.name = "sciName", 
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
  opt.argms <- .Options$warbleR
  
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
    
    #search recs in ML
    if (pb & download)
      write(file = "", x = "Obtaining recording list...")
    
    #format JSON
    qword <- gsub("_|-| ", "%20", qword)
  
    #initialize search
    q <- rjson::fromJSON(file = paste0("https://search.macaulaylibrary.org/api/v1/search?mediaType=a&q=", qword))
    
    
    count <- q$results$count
    
    if (q$results$count == 0) {cat("No recordings were found") 
      download <- FALSE
      }else {
     
    # no more than 100 at the time currently  
    if (q$results$count > 100) q$results$count <- 100
      
    if (q$results$count > 30)   q <- rjson::fromJSON(file = paste0("https://search.macaulaylibrary.org/api/v1/search?mediaType=a&q=", qword, "&count=", q$results$count))
       
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
      write(file = "", x = paste(q$results$count, " recordings found!", sep="")) else        write(file = "", x = paste(count, " recordings found, but only the first 100 can be retreived at this time", sep=""))
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
      results$sound.files <- paste(paste(fn, paste0("ML", results$catalogId), sep = "-"), ".mp3", sep = "")
    } else
      results$sound.files <- paste0("ML", results$catalogId, ".mp3")

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
    fl <- list.files(pattern = ".mp3$")
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
   
   results <- results[,match(c("sciName","commonName" , "catalogId", "locationLine2", "location", "behaviors", "sex", "recorder", "microphone", "accessories", "licenseType", "age", "thumbnailUrl", "previewUrl", "largeUrl", "mediaUrl", "userId", "mediaType", "rating", "userDisplayName", "assetId", "width", "height","speciesCode", "eBirdChecklistId", "valid","comments", "obsComments", "specimenUrl", "userProfileUrl", "locationLine1", "obsDttm", "collected", "eBirdChecklistUrl", "ratingCount", "specimenIds", "homeArchive", "stimulus", "source"), names(results))]
   
   return(droplevels(results))
   }
  
   }
