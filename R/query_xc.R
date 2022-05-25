#' Access 'Xeno-Canto' recordings and metadata
#' 
#' \code{query_xc} downloads recordings and metadata from \href{https://www.xeno-canto.org/}{Xeno-Canto}.
#' @usage query_xc(qword, download = FALSE, X = NULL, file.name = c("Genus", "Specific_epithet"), 
#' parallel = 1, path = NULL, pb = TRUE)  
#' @param qword Character vector of length one indicating the genus, or genus and
#'  species, to query 'Xeno-Canto' database. For example, \emph{Phaethornis} or \emph{Phaethornis longirostris}. 
#'  More complex queries can be done by using search terms that follow the 
#'  xeno-canto advance query syntax.This syntax uses tags to search within a particular aspect of the recordings 
#'  (e.g. country, location, sound type). Tags are of the form tag:searchterm'. For instance, 'type:song' 
#'  will search for all recordings in which the sound type description contains the word 'song'. 
#'  Several tags can be included in the same query. The query "phaethornis cnt:belize' will only return 
#'  results for birds in the genus \emph{Phaethornis} that were recorded in  Belize. 
#'  See \href{https://www.xeno-canto.org/help/search}{Xeno-Canto's search help} for a full description and see examples below 
#'  for queries using terms with more than one word.
#' @param download Logical argument. If \code{FALSE} only the recording file names and
#'   associated metadata are downloaded. If \code{TRUE}, recordings are also downloaded to the working
#'   directory as .mp3 files. Default is \code{FALSE}. Note that if the recording is already in the 
#'   working directory (as when the downloading process has been interrupted) it will be skipped. 
#'   Hence, resuming downloading processes will not start from scratch.   
#' @param X Data frame with a 'Recording_ID' column and any other column listed in the file.name argument. Only the recordings listed in the data frame 
#' will be download (\code{download} argument is automatically set to \code{TRUE}). This can be used to select
#' the recordings to be downloaded based on their attributes.  
#' @param file.name Character vector indicating the tags (or column names) to be included in the sound file names (if download = \code{TRUE}). Several tags can be included. If \code{NULL} only the 'Xeno-Canto' recording identification number ("Recording_ID") is used. Default is c("Genus", "Specific_epithet").
#' Note that recording id is always used (whether or not is listed by users) to avoid duplicated names.
#' @param parallel Numeric. Controls whether parallel computing is applied when downloading mp3 files.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing). Applied both when getting metadata and downloading files.
#' @param path Character string containing the directory path where the sound files will be saved. 
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return If X is not provided the function returns a data frame with the following recording information: recording ID, Genus, Specific epithet, Subspecies, English name, Recordist, Country, Locality, Latitude, Longitude, Vocalization type, Audio file, License, URL, Quality, Time, Date. Sound files in .mp3 format are downloaded into the working directory if download = \code{TRUE} or if X is provided; a column indicating the  names of the downloaded files is included in the output data frame.  
#' @export
#' @name query_xc
#' @details This function queries for avian vocalization recordings in the open-access
#' online repository \href{https://www.xeno-canto.org/}{Xeno-Canto}. It can return recordings metadata
#' or download the associated sound files. Complex queries can be done by using search terms that follow the 
#'  xeno-canto advance query syntax (check "qword" argument description). 
#'  Files are double-checked after downloading and "empty" files are re-downloaded. 
#'  File downloading process can be interrupted and resume later as long as the working directory is the same.
#'  Maps of recording coordinates can be produced using 
#' \code{\link{map_xc}}.
#' @seealso \code{\link{map_xc}}, 
#' \href{https://marce10.github.io/2016/12/22/Download_a_single_recording_for_each_species_in_a_site_from_Xeno-Canto.html}{blog post on accessing Xeno-Canto recordings} 
#' @examples
#' \dontrun{
#' # search without downloading
#' df1 <- query_xc(qword = 'Phaethornis anthophilus', download = FALSE)
#' View(df1)
#'
#' # downloading files
#'query_xc(qword = 'Phaethornis anthophilus', download = TRUE, path = tempdir())
#'
#' # check this folder
#' tempdir()
#' 
#' ## search using xeno-canto advance query ###
#' orth.pap <- query_xc(qword = 'gen:orthonyx cnt:papua loc:tari', download = FALSE)
#'  
#' # download file using the output data frame as input
#' query_xc(X = orth.pap, path = tempdir())
#' 
#' # use quotes for queries with more than 1 word (e.g. Costa Rica),note that the 
#' # single quotes are used for the whole 'qword' and double quotes for the 2-word term inside
#' #Phaeochroa genus in Costa Rica 
#' phae.cr <- query_xc(qword = 'gen:phaeochroa cnt:"costa rica"', download = FALSE)
#' 
#' # several terms can be searched for in the same field
#' # search for all female songs in sound type
#' femsong <- query_xc(qword = 'type:song type:female', download = FALSE)
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}) 
#last modification on nov-16-2016 (MAS)

query_xc <- function(qword, download = FALSE, X = NULL, file.name = c("Genus", "Specific_epithet"), 
                   parallel = 1, path = NULL, pb = TRUE) {
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(query_xc)
  
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
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else 
    if (!dir.exists(path)) 
      stop("'path' provided does not exist") else
        path <- normalizePath(path)
  
  #check internet connection
  a <- try(RCurl::getURL("www.xeno-canto.org"), silent = TRUE)
  if (is(a, "try-error")) stop("No connection to xeno-canto.org (check your internet connection!)")
  
  if (a == "Could not connect to the database")  stop("xeno-canto.org website is apparently down")
  
  # If parallel is not numeric
  if (!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if (any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")

  # fix file name column names
  if (!is.null(file.name)){
  file.name <- gsub(" ", "_", file.name) 
  file.name <- tolower(file.name) 
  }
  
  if (is.null(X) & !is.null(file.name))  
  {
    
    if (any(!(file.name %in%
             c("recording_id", "genus", "specific_epithet", "subspecies", "english_name", "recordist"   , 
               "country", "locality", "latitude", "longitude", "vocalization_type", "audio_file", "license",
               "url", "quality", "time", "date")))) stop("File name tags don't match column names in the output of this function (see documentation)")
  }
  
  
  if (is.null(X))
  {
    
    #search recs in xeno-canto (results are returned in pages with 500 recordings each)
    if (pb & download)
      write(file = "", x = "Obtaining recording list...")
    
    #format JSON
    qword <- gsub(" ", "%20", qword)
    
    #initialize search
    q <- rjson::fromJSON(file = paste0("https://www.xeno-canto.org/api/2/recordings?query=", qword))
    
    if (as.numeric(q$numRecordings) == 0) cat("No recordings were found") else {
      
      nms <- c("id", "gen", "sp", "ssp", "en", "rec", "cnt", "loc", "lat", "lng", "type", "file", "lic", "url", "q", "time", "date")
      
      ### loop over pages
      # set clusters for windows OS
      if (Sys.info()[1] == "Windows" & parallel > 1)
        cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
        
        f <- pblapply_wrblr_int(pbar = pb, X = 1:q$numPages, cl = cl, FUN = function(y)
      {
        #search for each page
        a <- rjson::fromJSON(file = paste0("https://www.xeno-canto.org/api/2/recordings?query=", qword, "&page=", y))  
        
        #put together as data frame
        d <-lapply(1:length(a$recordings), function(z) data.frame(t(unlist(a$recordings[[z]]))))
        
        d2 <- lapply(d,  function(x) 
        {
          if (!all(nms %in% names(x))){ 
            dif <- setdiff(nms, names(x))
            mis <- rep(NA, length(dif))
            names(mis) <- dif
            return(cbind(x, t(mis)))
          }
          return(x)
        })
        
        # determine all column names in all pages
        cnms <- unique(unlist(lapply(d2, names)))    
        
        # add columns that are missing to each selection table
        d3 <- lapply(d2, function(X)
        {
          nms <- names(X)
          if (length(nms) != length(cnms))  
            for(i in cnms[!cnms %in% nms]) {
              X <- data.frame(X,  NA, stringsAsFactors = FALSE, check.names = FALSE)
              names(X)[ncol(X)] <- i
            }
          return(X)
        })
        
        e <- do.call(rbind, d3)
        
        return(e)
      }
      ) 
        
        # determine all column names in all pages
        cnms <- unique(unlist(lapply(f, names)))    
        
        # add columns that are missing to each selection table
        f2 <- lapply(f, function(X)
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
      results <- do.call(rbind, f2)
      
      # convert factors to characters
      indx <- sapply(results, is.factor)
      results[indx] <- lapply(results[indx], as.character)
      
      #order columns
    results <- results[ ,order(match(names(results), nms))]
    
    names(results)[match(c("id", "gen", "sp", "ssp", "en", "rec", "cnt", "loc", "lat", "lng", "alt", "type", "file", "lic", "url", "q", "length", "time", "date", "uploaded", "rmk", "bird.seen", "playback.used"), names(results))] <- c("Recording_ID", "Genus", "Specific_epithet", "Subspecies", "English_name", "Recordist", 
                        "Country", "Locality", "Latitude", "Longitude", "Altitude", "Vocalization_type", "Audio_file", "License",
                        "Url", "Quality", "Length", "Time", "Date", "Uploaded", "Remarks", "Bird_seen","Playback_used")[which(c("id", "gen", "sp", "ssp", "en", "rec", "cnt", "loc", "lat", "lng", "alt", "type", "file", "lic", "url", "q", "length", "time", "date", "uploaded", "rmk", "bird.seen", "playback.used") %in% names(results))]
  
    # rename also columns
    names(results) <- gsub("also", "Other_species", names(results))
    # rename
    names(results) <- gsub("sono.", "Spectrogram_", names(results))
    
    #remove duplicates
    results <- results[!duplicated(results$Recording_ID), ]
    
    if (pb)
      write(file = "", x = paste0(nrow(results), " recording(s) found!"))
    } 
  } else { 
    #stop if X is not a data frame
    if (!is(X, "data.frame")) stop("X is not a data frame")
    
    #stop if the basic columns are not found
    if (!is.null(file.name))
    {if (any(!c(file.name, "recording_id") %in% tolower(colnames(X)))) 
      stop(paste(paste(c(file.name, "recording_id")[!c(file.name, "recording_id") %in% tolower(colnames(X))], collapse=", "), "column(s) not found in data frame"))} else
        if (!"Recording_ID" %in% colnames(X)) 
          stop("Recording_ID column not found in data frame")
    
    download <- TRUE
    results <- X  
  }
  
  #download recordings
  if (download) {
    if (any(file.name == "recording_id")) file.name <- file.name[-which(file.name == "recording_id")]
    
    if (!is.null(file.name))  {  if (length(which(tolower(names(results)) %in% file.name)) > 1)
      fn <- apply(results[,which(tolower(names(results)) %in% file.name)], 1 , paste , collapse = "-" ) else 
        fn <- results[,which(tolower(names(results)) %in% file.name)]
      results$sound.files <- paste(paste(fn, results$Recording_ID, sep = "-"), ".mp3", sep = "")     
    } else
      results$sound.files <- paste0(results$Recording_ID, ".mp3")   
    
    xcFUN <-  function(results, x){
      if (!file.exists(results$sound.files[x]))
        download.file(
          url = paste("https://xeno-canto.org/", results$Recording_ID[x], "/download", sep = ""),
          destfile = file.path(path, results$sound.files[x]),
          quiet = TRUE,  mode = "wb", cacheOK = TRUE,
                      extra = getOption("download.file.extra"))
      return (NULL)
    }

    # set clusters for windows OS
    if (pb)
      write(file = "", x = "Downloading files...")
    if (Sys.info()[1] == "Windows" & parallel > 1)
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
    
       a1 <- pblapply_wrblr_int(pbar = pb, X = 1:nrow(results), cl = cl, FUN = function(x) 
  { 
      xcFUN(results, x) 
  }) 
  
if (pb) write(file = "", x ="double-checking downloaded files")
   
   #check if some files have no data
    fl <- list.files(path = path, pattern = ".mp3$")
    size0 <- fl[file.size(file.path(path, fl)) == 0]
   
    #if so redo those files
    if (length(size0) > 0)
  {  Y <- results[results$sound.files %in% size0, ]
     unlink(size0)
     
     # set clusters for windows OS
     if (Sys.info()[1] == "Windows" & parallel > 1)
       cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
     
     
    a1 <- pblapply_wrblr_int(pbar = pb, X = 1:nrow(Y), cl = cl, FUN = function(x) 
  { 
      try(xcFUN(Y, x), silent = TRUE) 
  }) 
     
     }
    
    
  }
 if (is.null(X)) if (as.numeric(q$numRecordings) > 0) {
   
   # convert lat long to numbers
   results$Latitude <- as.numeric(results$Latitude)
   results$Longitude <- as.numeric(results$Longitude)
   
   return(droplevels(results))
   }
  
   }


##############################################################################################################
#' alternative name for \code{\link{query_xc}}
#'
#' @keywords internal
#' @details see \code{\link{query_xc}} for documentation. \code{\link{querxc}} will be deprecated in future versions.
#' @export

querxc <- query_xc
