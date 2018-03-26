#' Find overlapping selections 
#' 
#' \code{ovlp_sels} finds which selections overlap in time within a given sound file.
#' @usage ovlp_sels(X, index = FALSE, pb = TRUE, max.ovlp = 0, relabel = FALSE, 
#' drop = FALSE, priority = NULL, priority.col = NULL, unique.labs = TRUE, 
#' indx.row = FALSE, parallel = 1)
#' @param X 'selection.table' object or data frame with the following columns: 1) "sound.files": name of the .wav 
#' files, 2) "selec": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections. The ouptut of \code{\link{manualoc}} or \code{\link{autodetec}} can 
#' be used as the input data frame. Other data frames can be used as input, but must have at least the 4 columns mentioned above.
#' @param index Logical. Indicates if only the index of the overlapping selections would be returned.
#' Default is \code{FALSE}.
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @param max.ovlp Numeric vector of length 1 specifying the maximum overlap allowed (in seconds)
#' . Default is 0. 
#' @param relabel Logical. If \code{TRUE} then selections names (selec column) are reset.  
#' Default is \code{FALSE}.
#' @param drop Logical. If \code{TRUE}, when 2 or more selections overlap the function will remove 
#' all but one of the overlapping selection. Default is \code{FALSE}.
#' @param priority Character vector. Controls the priority criteria used for removing overlapped selections. It
#' must list the levels of the column used to determine priority (argument priority.col) in the desired 
#' priority order. Default is \code{NULL}.
#' @param priority.col Character vector of length 1 with the name of the column use to determine the priority of
#' overlapped selections. Default is \code{NULL}.
#' @param unique.labs Logical to control if labels are reused across different sound files (if \code{TRUE}, default).
#' @param indx.row Logical. If \code{TRUE} then a character column with the indices of all selections that overlapped with
#' each selection is added to the ouput data frame (if \code{index = TRUE}). For instance, if the selections in rows 1,2 
#' and 3 all overlapped with each other, the 'indx.row' value would be "1/2/3" for all. However, if selection 3 only overlaps
#'  with 2 but not with 1, then it returns, "1/2" for row 1, "1/2/3" for row 2, and "2/3" for row 3. Default is \code{FALSE}. 
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @return A data frame with the columns in X plus an additional column ('ovlp_sels') indicating 
#' which selections overlap. The ones with the same number overlap with each other. If 
#' \code{drop = TRUE} only the non-overlapping selections are return. If 2 or more selections 
#' overlap only the first one is kept.  
#' @export
#' @name ovlp_sels
#' @examples
#' {
#' #no overlap
#' ovlp_sels(X =  selec.table)
#'
#' # modified selec.table to make the first and second selection overlap
#' Y <- selec.table
#' Y$end[4] <- 1.5
#'   
#'  ovlp_sels(X =  Y)
#' 
#' # drop overlapping
#'  ovlp_sels(X =  Y, drop = TRUE)
#' 
#' # get index instead
#'  ovlp_sels(X =  Y, index = TRUE)
#' }
#' @details This function detects selections within a selection table that overlap in time. Selections must be 
#' listed in a data frame similar to \code{\link{selec.table}}.
#' @seealso  \code{\link{filtersels}} \code{\link{selec.table}}
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on mar-13-2018 (MAS)

ovlp_sels <- function(X, index = FALSE, pb = TRUE, max.ovlp = 0, relabel = FALSE, drop = FALSE,                       
                      priority = NULL, priority.col = NULL, unique.labs = TRUE, indx.row = FALSE, parallel = 1) 
  {
  
  # set pb options 
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), add = TRUE)
  
  #X must be provided
  if(is.null(X)) stop("'X' must be provided (a data frame)")
  
  #if X is not a data frame
  if(!class(X) %in% c("data.frame", "selection.table")) stop("X is not of a class 'data.frame' or 'selection table")
  
  # check column names
  if(!all(c("sound.files", "selec", 
            "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
    
  #if there are NAs in start or end stop
  if(any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if(all(class(X$end) != "numeric" & class(X$start) != "numeric")) stop("'end' and 'selec' must be numeric")
  
  #if any start higher than end stop
  if(any(X$end - X$start<0)) stop(paste("The start is higher than the end in", length(which(X$end - X$start<0)), "case(s)"))
  
 # priority
  if(!is.null(priority.col) & !is.null(priority))
  {
  #if col not found
    if(!priority.col %in% names(X)) stop(paste('priority.col', priority.col, "not found"))
  
    #all levels of priority col should be in priority
    if(!all(priority %in% unique(X[, priority.col]))) stop("Not all levels of 'priority.col' included in 'priority'") 
  }

  
 # function that runs on a data frame for a single sound file 
  ovlpFUN <- function(X, ndx.rw = indx.row) {
    #only if there is more than 1 selection for that sound file
    if(nrow(X) > 1)
    {
      # order by start time
      X <- X[order(X$start), ]
      
      #relabel
      if(relabel)
        rownames(X) <- 1:nrow(X)
      
    # determine which ones overlap
    out1 <- lapply(1:(nrow(X)), function(i) {
    sapply((i) : nrow(X), function(j) {
      # if they overlap not perfectly
      if(X$start[j] < X$end[i]) {
    if(X$end[i] - X$start[j] > max.ovlp)  out <- i else out <- 0
      } else 
        # if they have the same start and end
        if(X$start[i] == X$start[j] & X$end[i] == X$end[j]) out <- i else 0
  }
  )
    })
    
    # put it in a triangular matrix
    out2 <- as.data.frame(lapply(out1, function(x) c(rep(0, nrow(X) - length(x)), x)), col.names = 1:length(out1))


lbls <- rep(NA, nrow(out2))

###### using loop
for(w in 1:nrow(out2)){
  
  if (w == 1) lbls[w] <- max(out2) + 1 else
    if(length(which(out2[ w, ] != 0)) >= 2) {
      wh.mn <- which(out2[ w, ] != 0)
  lbls[w] <- lbls[wh.mn[- length(wh.mn)]]
      }  else   lbls[w] <- max(lbls, na.rm = TRUE) + 1 
}


# determine unique tags
unq <- table(lbls)

# add NAs to single tags
lbls[lbls == names(unq)[unq == 1]] <- NA

if(length(lbls[!is.na(lbls)]) > 0)
lbls2 <- lbls <- lbls - min(lbls, na.rm = TRUE) + 1

lbls.lvls <- unique(lbls)

lbls.lvls <- lbls.lvls[!is.na(lbls.lvls)]

if(length(lbls.lvls) > 0)
for(e in seq_len(length(lbls.lvls)))
  if (lbls.lvls[e] != lbls.lvls[1]) lbls[lbls2 == lbls.lvls[e]] <- max(lbls2[1:max(which(lbls2 == lbls.lvls[e - 1]), na.rm = TRUE)], na.rm = TRUE) + 1

# add index row
if(ndx.rw)
{  if(length(lbls.lvls) > 0)
  X$indx.row <- sapply(1:nrow(out2), function(z) paste(unique(c(which(out2[z, ] != 0), which(out2[ , z] != 0))), collapse = "/")) else X$indx.row <- NA
}
                       
    X$ovlp.sels <- lbls
  } else X$ovlp.sels <- NA # NA if only 1 row

  
   return(X)  
    }
  
  # split data into a data.frame per sound file  
sX <- split(X, X$sound.files, drop = TRUE)


# set pb options 
pbapply::pboptions(type = ifelse(pb, "timer", "none"))

# set clusters for windows OS
if (Sys.info()[1] == "Windows" & parallel > 1)
  cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel

# run loop apply function
out <- pbapply::pblapply(X = sX, cl = cl, FUN = ovlpFUN) 

out <- do.call(rbind, out)    
 
rownames(out) <- 1:nrow(out)

#unique labels
if (unique.labs)
{  # order factors
out$ovlp.sels <- factor(out$ovlp.sels, levels = as.character(stats::na.exclude(out$ovlp.sels[!duplicated(out$ovlp.sels)])))

# convert to numeric
ovlp <- out$ovlp.sels <- as.numeric(out$ovlp.sels)
}

if(index) return(which(duplicated(out$ovlp.sels, incomparables = NA))) else{

    # remove the ones overlapped  
    if(drop)
      {
      # remove based on priority
      if(!is.null(priority.col) & !is.null(priority) &  length(priority) > 1)
        {
        # remove duplicated labels
        priority <- priority[!duplicated(priority)]
        
        # create numeric vector to order resulting data frame before dropping
        ordr <- as.character(out[, priority.col])
        for(i in 1:length(priority)) {
          ordr[ordr == priority[i]] <- i 
        }
        
        # order based on priority
        out <- out[order(out$sound.files, as.numeric(ordr)),]
        
        }
    
        
    out <- out[!duplicated(out$ovlp.sels, incomparables = NA), -ncol(out)]
    
    out <- out[order(out$sound.files, out$start),]
    
    }
  
  if(length(ovlp[!is.na(ovlp)]) > 0) 
    {
    if(drop)
      cat(paste(length(ovlp[!is.na(ovlp)]),"selections overlapped,", length(which(duplicated(ovlp,  incomparables = NA))), "were removed \nSelection table has been ordered by 'sound.files' and 'start'")) else
  cat(paste(length(ovlp[!is.na(ovlp)]),"selections overlapped \nSelection table has been ordered by 'sound.files' and 'start'"))
  
    } else cat("No overlapping selections were found \nSelection table has been ordered by 'sound.files' and 'start'")
  return(out)   
}
  

}
  
