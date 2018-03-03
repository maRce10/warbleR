#' Find overlapping selections 
#' 
#' \code{ovlp_sels} finds which selections overlap in time within a given sound file.
#' @usage ovlp_sels(X, index = FALSE, pb = TRUE, max.ovlp = 0, relabel = FALSE, 
#' drop = FALSE, priority = NULL, priority.col = NULL)
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
#last modification on may-22-2017 (MAS)

ovlp_sels <- function(X, index = FALSE, pb = TRUE, max.ovlp = 0, relabel = FALSE, drop = FALSE, priority = NULL, priority.col = NULL) 
  {
  
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
  ovlpFUN <- function(X) {
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

  # determine unique overlap tag  
out3 <- apply(out2, 1, function(x) unique(x[x != 0]))
  
# put own tag to non-overlapping  
out4 <- lapply(seq_len(length(out3)), function(x) {
  if(length(out3[[x]]) == 0) y <- x else y <- out3[[x]]
return(y)
})

# determine unique tags
unq <- unique(unlist(out4))
unq <- unq[unq != 0]

# if more than 1 tag
if(length(unq) > 1)
{
# put a single tag to all the ones that overlap
  out5 <- sapply(seq_len(length(out4)), function(x) {
  sub <- out4[sapply(lapply(out4, function(y) y == x), any)]
  
  #if not share with any other selection have its own
  tab <- table(unlist(sub))
  if(length(tab) > 0)
    res <- min(as.numeric(names(tab)[tab == length(sub)])) else res <- x
return(res)
  })

  # add NA to not overlapping    
tab <- table(out5)
out5[out5 %in% as.numeric(names(tab)[tab == 1])] <- NA

X$ovlp.sels <- out5
} else X$ovlp.sels <- NA # 1 if only 1 tag for all

  } else X$ovlp.sels <- NA # NA if only 1 row
 
   return(X)  
    }
  
  # split data into a data.frame per sound file  
sX <- split(X, X$sound.files, drop = TRUE)

# run ovlpFUN over the whole X data frame
  if(pb) 
    Z <- pbapply::pblapply(seq_len(length(sX)), function(x) {

            # run function for finding overlaps
      ovlp <- ovlpFUN(sX[[x]])
      
      #create unique tags for each sound file
      ovlp$ovlp.sels[!is.na(ovlp$ovlp.sels)] <-paste(ovlp$ovlp.sels[!is.na(ovlp$ovlp.sels)], x)
      ovlp$ovlp.sels <- as.factor(ovlp$ovlp.sels)
      
      return(ovlp)
    
      }) else
      Z <- lapply(seq_len(length(sX)), function(x) {
      
        # run function for finding overlaps
        ovlp <- ovlpFUN(sX[[x]])
        
        #create unique tags for each sound file
        ovlp$ovlp.sels[!is.na(ovlp$ovlp.sels)] <-paste(ovlp$ovlp.sels[!is.na(ovlp$ovlp.sels)], x)
        ovlp$ovlp.sels <- as.factor(ovlp$ovlp.sels)
        
        return(ovlp)
        
      })        
  
out <- do.call(rbind, Z)    
 
rownames(out) <- 1:nrow(out)

# order factors
out$ovlp.sels <- factor(out$ovlp.sels, levels = as.character(stats::na.exclude(out$ovlp.sels[!duplicated(out$ovlp.sels)])))

# convert to numeric
ovlp <- out$ovlp.sels <- as.numeric(out$ovlp.sels)

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
      cat(paste(length(ovlp[!is.na(ovlp)]),"selections overlapped,", length(which(duplicated(ovlp,  incomparables = NA))), "were removed")) else
  cat(paste(length(ovlp[!is.na(ovlp)]),"selections overlapped"))
  
    } else cat("No overlapping selections were found")
  return(out)   
}
  

}
  
