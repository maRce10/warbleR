#' Access Xeno-Canto recordings and metadata
#' 
#' \code{querxc} downloads recordings and metadata from Xeno-Canto (\url{http://www.xeno-canto.org/}).
#' @usage querxc(qword, download = FALSE, X = NULL, file.name = c("Genus", "Specific_epithet"), 
#' parallel = 1)  
#' @param qword Character vector of length one indicating the genus, or genus and
#'   species, to query Xeno-Canto database. For example, \emph{Phaethornis} or \emph{Phaethornis longirostris}. 
#'   (\url{http://www.xeno-canto.org/}).
#' @param download Logical argument. Downloads recording file names and
#'   associated metadata if \code{FALSE}. If \code{TRUE}, recordings are also downloaded to working
#'   directory as .mp3 files. Default is \code{FALSE}. Note that if the recording is already in the 
#'   working directory (as when the downloading process has been interrupted) it will be skipped. 
#'   Hence, resuming downloading processes will not start from scratch.   
#' @param X Data frame with a Recording_ID column and any other column listed in the file.name argument. Only the recordings listed in the data frame 
#' will be download (\code{download} argument is automatically set to \code{TRUE}). This can be used to select
#' the recordings to be downloaded based on their attributes.  
#' @param file.name Character vector indicating the tags (or column names) to be included in the sound file names (if download = \code{TRUE}). Several tags can be included. If \code{NULL} only the Xeno-Canto recording identification number ("Recording_ID") is used. Default is c("Genus", "Specific_epithet").
#' Note that recording id is always used (whether or not is listed by users) to avoid duplicated names.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' Not available in Windows OS.
#' @return If X is not provided the function returns a data frame with the following recording information: recording ID, Genus, Specific epithet, Subspecies, English name, Recordist, Country, Locality, Latitude, Longitude, Vocalization type, Audio file, License, URL, Quality,Time, Date. Sound files in .mp3 format are downloaded into the working directory if download = \code{TRUE} or if X is provided; a column indicating the  names of the downloaded files (sound file) is included in the output data frame.  
#' @export
#' @name querxc
#' @details This function queries for avian vocalization recordings in the open-access
#' online repository Xeno-Canto (\url{http://www.xeno-canto.org/}). It can return recordings metadata
#' or download the associated sound files.  
#' @examples
#' \dontrun{
#' # First create empty folder
#' setwd(tempdir())
#' 
#' #search without downloading
#' df1 <- querxc(qword = "Phaethornis anthophilus", download = FALSE)
#' View(df1)
#'
#' #downloading files
#' querxc(qword = "Phaethornis anthophilus", download = TRUE)

#' #check this folder!!
#' getwd()
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu}) 
#last modification on jul-24-2016 (MAS)

querxc <- function(qword, download = FALSE, X = NULL, file.name = c("Genus", "Specific_epithet"), 
                   parallel = 1) {
 
   # If parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
file.name <- gsub(" ", "_", file.name) 
   file.name <- tolower(file.name) 
  
if(is.null(X) & !is.null(file.name))  
{

   if(any(!(file.name %in%
  c("recording_id", "genus", "specific_epithet", "subspecies", "english_name", "recordist"   , 
  "country", "locality", "latitude", "longitude", "vocalization_type", "audio_file",        "license",
  "url", "quality", "time", "date")))) stop("File name tags don't match column names in the output of this function (see documentation)")
}


  if(is.null(X))
  {
  #check internet connection
  a <- try(RCurl::getURL("www.xeno-canto.org"), silent = TRUE)
  if(substr(a[1],0,5) == "Error") stop("No connection to xeno-canto.org (check your internet connection!)")

  if(a == "Could not connect to the database")  stop("xeno-canto.org website is apparently down")
  
  #search recs in xeno-canto (results are returned in pages with 500 recordings each)
  message("Obtaining recording list...")
  if(sapply(strsplit(qword, " "), length) == 2)
  query <- rjson::fromJSON(, paste("http://www.xeno-canto.org/api/recordings.php?species_nr=&query=", #run search
                            strsplit(qword, " ")[[1]][1],"%20",strsplit(qword, " ")[[1]][2], sep="")) else
  query <- rjson::fromJSON(, paste("http://www.xeno-canto.org/api/recordings.php?species_nr=&query=", #run search
                            qword, sep=""))
  
  n.recs <- query$numRecordings
  if(n.recs == 0) {
    stop("No recordings found!")
  }
  n.pages <- query$numPages
  recs <- query$recordings
  if(n.pages > 1)
  for(i in c(2:n.pages)){
    if(sapply(strsplit(qword, " "), length) == 2)
      query <- rjson::fromJSON(, paste("http://www.xeno-canto.org/api/recordings.php?species_nr=&query=", #run search
                                strsplit(qword, " ")[[1]][1],"%20",strsplit(qword, " ")[[1]][2], "&page=", i, sep="")) else                                  
    query <- rjson::fromJSON(, paste("http://www.xeno-canto.org/api/recordings.php?species_nr=&query=", #run search
                              qword, "&page=", i, sep=""))
    recs <- c(recs, query$recordings)
  }
  
  message("Processing recording information:")
  
  results <- as.data.frame(t(sapply(matrix(c(1:n.recs), ncol=1), function(x){

    rec <- recs[[x]]
    return(c(
      rec$id,
      rec$gen,
      rec$sp,
      rec$ssp,
      rec$en,
      rec$rec,
      rec$cnt,
      rec$loc,
      ifelse(is.null(rec$lat) == FALSE, 
             rec$lat, ""),
      ifelse(is.null(rec$lng) == FALSE, 
             rec$lng, ""),
      rec$type,
      rec$file,
      rec$lic,
      rec$url,
      rec$q,
      rec$time,
      rec$date
      ))
    ####
  })))

  names(results) <- c("Recording_ID", "Genus", "Specific_epithet", "Subspecies", "English_name", "Recordist", "Country", "Locality", "Latitude", "Longitude", "Vocalization_type", "Audio_file", "License", "URL", "Quality","Time", "Date")

  #adjust ouput in case search has 2 words instead of 1
  if(sapply(strsplit(qword, " "), length) == 2) 
    results <- results[results$Specific_epithet == strsplit(qword, " ")[[1]][2], ] else
      if(length(which(results$Genus == qword))>0) results <- results[results$Genus == qword, ]

  #remove duplicates
results <- results[!duplicated(results$Recording_ID), ]

message(paste( nrow(results), " recordings found!", sep=""))  

} else { 
  #stop if X is not a data frame
  if(class(X) != "data.frame") stop("X is not a data frame")

  #stop if the basic columns are not found
  if(!is.null(file.name))
  {if(any(!c(file.name, "recording_id") %in% tolower(colnames(X)))) 
    stop(paste(paste(c(file.name, "recording_id")[!c(file.name, "recording_id") %in% tolower(colnames(X))], collapse=", "), "column(s) not found in data frame"))} else
    if(!"recording_id" %in% colnames(X)) 
    stop("Recording_ID column not found in data frame")
  
    download <- TRUE
results <- X  
}

  #download recordings
  if(download) {
    if(any(file.name == "recording_id")) file.name <- file.name[-which(file.name == "recording_id")]
    
    if(!is.null(file.name))  {  if(length(which(tolower(names(results)) %in% file.name)) > 1)
      fn <- apply(results[,which(tolower(names(results)) %in% file.name)], 1 , paste , collapse = "-" ) else 
        fn <- results[,which(tolower(names(results)) %in% file.name)]
          results$sound.files <- paste(paste(fn, results$Recording_ID, sep = "-"), ".mp3", sep = "")     
      } else
results$sound.files <- paste(results$Recording_ID, ".mp3", sep = "")   

      
      xcFUN <-  function(results, x){
      if(!file.exists(results$sound.files[x]))
        download.file(url = paste("http://xeno-canto.org/download.php?XC=", results$Recording_ID[x], sep=""), destfile = results$sound.files[x],
                      quiet = TRUE,  mode = "wb", cacheOK = TRUE,
                      extra = getOption("download.file.extra"))
      return (NULL)
    }
        message("Downloading sound files...")

      
  if(parallel > 1) {if(Sys.info()[1] == "Windows") 
    {
    
    x <- NULL #only to avoid non-declared objects
    
    cl <- parallel::makeCluster(parallel)
    
    doParallel::registerDoParallel(cl)
    
    a1 <- parallel::parLapply(cl, 1:nrow(results), function(x)
    {
      xcFUN(results, x) 
    })
    
    parallel::stopCluster(cl)
    
  } 
    
    if(Sys.info()[1] == "Linux") {    # Run parallel in other operating systems
      
      a1 <- parallel::mclapply(1:nrow(results), function(x) {
      xcFUN(results, x) 
      })
    }
    if(!any(Sys.info()[1] == c("Linux", "Windows")))
    {
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
      
       doParallel::registerDoParallel(cl)
 
      a1 <- foreach::foreach(x = 1:nrow(results)) %dopar% {
            xcFUN(results, x)
      }
      
      parallel::stopCluster(cl)
    }
    
  } else {
    a1 <- pbapply::pblapply(1:nrow(results), function(x) 
  { 
      xcFUN(results, x) 

  })
  }
  

   message("double-checking downloaded files")
   
   #check if some files have no data
    fl <- list.files(pattern = ".mp3$")
    size0 <- fl[file.size(fl) == 0]
   
    #if so redo those files
    if(length(size0) > 1)
  {  Y <- results[results$sound.files %in% size0, ]
     unlink(size0)
     
    
     
       if(parallel > 1) {if(Sys.info()[1] == "Windows") 
    {
    
    x <- NULL #only to avoid non-declared objects
    
    cl <- parallel::makeCluster(parallel)
    
    doParallel::registerDoParallel(cl)
    
    a1 <- parallel::parLapply(cl, 1:nrow(Y), function(x)
    {
      xcFUN(Y, x) 
    })
    
    parallel::stopCluster(cl)
    
  } 
    
    if(Sys.info()[1] == "Linux") {    # Run parallel in other operating systems
      
      a1 <- parallel::mclapply(1:nrow(Y), function(x) {
      xcFUN(Y, x) 
      })
    }
    if(!any(Sys.info()[1] == c("Linux", "Windows")))
    {
      cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
      
       doParallel::registerDoParallel(cl)
 
      a1 <- foreach::foreach(x = 1:nrow(Y)) %dopar% {
            xcFUN(Y, x)
      }
      
      parallel::stopCluster(cl)
    }
    
  } else {
    a1 <- pbapply::pblapply(1:nrow(Y), function(x) 
  { 
      xcFUN(Y, x) 

  })
  }
     
     
     }
    
    
  }
 if(is.null(X)) return(droplevels(results))
  }
