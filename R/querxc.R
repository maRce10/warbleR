#' Access Xeno-Canto recordings and metadata
#' 
#' \code{querxc} downloads recordings and metadata from Xeno-Canto (\url{http://www.xeno-canto.org/}).
#' @usage querxc(qword, download = FALSE, X = NULL, parallel = 1)  
#' @param qword Character vector of length one indicating the genus, or genus and
#'   species, to query Xeno-Canto database. For example, \emph{Phaethornis} or \emph{Phaethornis longirostris}. 
#'   (\url{http://www.xeno-canto.org/}).
#' @param download Logical argument. Downloads recording file names and
#'   associated metadata if \code{FALSE}. If \code{TRUE}, recordings are also downloaded to working
#'   directory as .mp3 files. Default is \code{FALSE}.
#' @param X Data frame with the same columns as the output of the function, or at least the following
#' columns: Genus, Specific_epithet and Recording_ID. Only the recordings listed in the data frame 
#' will be download (\code{download} argument is automatically set to \code{TRUE}). This can be used to select
#' the recordings to be downloaded based on their attributes.  
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (e.i. no parallel computing).
#' For windows OS the \code{parallelsugar} package should be installed.   
#' @return If X is not provided the function returns a data frame with the following recording information: recording ID, Genus, Specific epithet, Subspecies, English name, Recordist, Country, Locality, Latitude, Longitude, Vocalization type, Audio file, License, URL, Quality,Time, Date. Sound files in .mp3 format are downloaded into the working directory if download = \code{TRUE} or if X is provided.
#' @export
#' @name querxc
#' @details This function queries for avian vocalization recordings in the open-access
#' online repository Xeno-Canto (\url{http://www.xeno-canto.org/}). It can return recordings metadata
#' or can also download the associated sound files.  
#' @examples
#' \dontrun{
#' # First create empty folder
#' dir.create(file.path(getwd(),"temp"))
#' setwd(file.path(getwd(),"temp"))

#' df1 <- querxc("Phaethornis anthophilus", download = FALSE)
#' View(df1)
#' 
#' #downloading files
#' querxc("Phaethornis anthophilus", download = TRUE)

#' #check this folder!!
#' getwd()
#' 
#' # remove example directory
#' unlink(getwd(),recursive = TRUE)
#' }
#' @author Marcelo Araya-Salas (\url{http://marceloarayasalas.weebly.com/}) and Hua Zhong

querxc <- function(qword, download=FALSE, X = NULL, parallel = 1) {

  
  # If parallel is not numeric
  if(!is.numeric(parallel)) stop("'parallel' must be a numeric vector of length 1") 
  if(any(!(parallel %% 1 == 0),parallel < 1)) stop("'parallel' should be a positive integer")
  
  # If parallel was called
  if(parallel > 1)
  { options(warn = -1)
    if(all(Sys.info()[1] == "Windows",requireNamespace("parallelsugar", quietly = TRUE) == TRUE)) 
      lapp <- function(X, FUN) parallelsugar::mclapply(X, FUN, mc.cores = parallel) else
        if(Sys.info()[1] == "Windows"){ 
          message("Windows users need to install the 'parallelsugar' package for parallel computing (you are not doing it now!)")
          lapp <- pbapply::pblapply} else lapp <- function(X, FUN) parallel::mclapply(X, FUN, mc.cores = parallel)} else lapp <- pbapply::pblapply
          
          options(warn = 0)
  
  if(is.null(X))
  {
  #check internet connection
  a <- try(RCurl::getURL("www.xeno-canto.org"), silent=T)
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
  if(any(!c("Genus", "Specific_epithet", "Recording_ID") %in% colnames(X))) 
    stop(paste(paste(c("Genus", "Specific_epithet", "Recording_ID")[!c("Genus",
      "Specific_epithet", "Recording_ID") %in% colnames(X)], collapse=", "), "column(s) not found in data frame"))
  download <- TRUE
results <- X  }



  #download recordings
  if(download) {
    lapp(matrix(c(1:length(results$Genus)), ncol=1), function(x){
      gen <- results$Genus[x]
      se <- results$Specific_epithet[x]
      rid <- results$Recording_ID[x]
      if(!file.exists(file.path(getwd(), paste(gen, "-", se, "-", rid, ".mp3", sep = ""))))
        download.file(paste("http://xeno-canto.org/download.php?XC=", rid, sep=""), 
                      file.path(getwd(), paste(gen, "-", se, "-", rid, ".mp3", sep="")),
                      quiet = TRUE,  mode = "wb", cacheOK = TRUE,
                      extra = getOption("download.file.extra"))
      return (NULL)
    })
  }  
 if(is.null(X)) return(droplevels(results))
}
