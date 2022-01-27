#' Find overlapping selections 
#' 
#' \code{overlapping_sels} finds which selections overlap in time within a given sound file.
#' @usage overlapping_sels(X, index = FALSE, pb = TRUE, max.ovlp = 0, relabel = FALSE, 
#' drop = FALSE, priority = NULL, priority.col = NULL, unique.labs = NULL, 
#' indx.row = FALSE, parallel = 1, verbose = TRUE)
#' @param X 'selection_table' object or data frame with the following columns: 1) "sound.files": name of the sound 
#' files, 2) "selec": number of the selections, 3) "start": start time of selections, 4) "end": 
#' end time of selections.
#' @param index Logical. Indicates if only the index of the overlapping selections would be returned.
#' Default is \code{FALSE}.
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @param max.ovlp Numeric vector of length 1 specifying the maximum overlap allowed (in seconds)
#' . Default is 0. 
#' @param relabel Logical. If \code{TRUE} then selection names ('selec' column) are reset within each sound files.  
#' Default is \code{FALSE}.
#' @param drop Logical. If \code{TRUE}, when 2 or more selections overlap the function will remove 
#' all but one of the overlapping selection. Default is \code{FALSE}.
#' @param priority Character vector. Controls the priority criteria used for removing overlapped selections. It
#' must list the levels of the column used to determine priority (argument priority.col) in the desired 
#' priority order. Default is \code{NULL}.
#' @param priority.col Character vector of length 1 with the name of the column use to determine the priority of
#' overlapped selections. Default is \code{NULL}.
#' @param unique.labs DEPRECATED.
#' @param indx.row Logical. If \code{TRUE} then a character column with the indices of all selections that overlapped with
#' each selection is added to the ouput data frame (if \code{index = TRUE}). For instance, if the selections in rows 1,2 
#' and 3 all overlapped with each other, the 'indx.row' value would be "1/2/3" for all. However, if selection 3 only overlaps
#'  with 2 but not with 1, then it returns, "1/2" for row 1, "1/2/3" for row 2, and "2/3" for row 3. Default is \code{FALSE}. 
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param verbose Logical to control if messages are printed to the console.
#' @return A data frame with the columns in X plus an additional column ('ovlp.sels') indicating 
#' which selections overlap. The ones with the same number overlap with each other. If 
#' \code{drop = TRUE} only the non-overlapping selections are return. If 2 or more selections 
#' overlap only the first one is kept. The arguments 'priority' and 'priority.col' can be used to modified the criterium for droping overlapping selections. 
#' @export
#' @name overlapping_sels
#' @examples
#' {
#' #no overlap
#' overlapping_sels(X =  lbh_selec_table)
#'
#' # modified lbh_selec_table to make the first and second selection overlap
#' Y <- lbh_selec_table
#' Y$end[4] <- 1.5
#' 
#' overlapping_sels(X = Y)
#' 
#' # drop overlapping
#'  overlapping_sels(X =  Y, drop = TRUE)
#' 
#' # get index instead
#'  overlapping_sels(X =  Y, index = TRUE)
#' }
#' @details This function detects selections within a selection table that overlap in time. Selections must be 
#' listed in a data frame similar to \code{\link{lbh_selec_table}}. Note that row names are set to \code{1:nrow(X)}.
#' @seealso  \code{\link{filtersels}}, \code{\link{lbh_selec_table}}
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on mar-13-2018 (MAS)

overlapping_sels <- function(X, index = FALSE, pb = TRUE, max.ovlp = 0, relabel = FALSE, drop = FALSE,                       
                      priority = NULL, priority.col = NULL, unique.labs = NULL, indx.row = FALSE, parallel = 1, verbose = TRUE)  {
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(overlapping_sels)
  
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
  
  if (!is.null(unique.labs))
    write(file = "", x = crayon::silver("'unique.labs' has been deprecated"))
  
  #if X is not a data frame
  if (!any(is.data.frame(X), is_selection_table(X))) stop("X is not of a class 'data.frame', 'selection_table'")
  
  # check column names
  if (!all(c("sound.files", "selec", 
            "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
    
  #if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop("NAs found in start and/or end")  
  
  #if end or start are not numeric stop
  if (any(!is(X$end, "numeric"), !is(X$start, "numeric"))) stop("'start' and 'end' must be numeric")
  
  #if any start higher than end stop
  if (any(X$end - X$start <= 0)) stop(paste("Start is higher than or equal to end in", length(which(X$end - X$start <= 0)), "case(s)"))
  
 # priority
  if (!is.null(priority.col) & !is.null(priority))
  {
  #if col not found
    if (!priority.col %in% names(X)) stop(paste('priority.col', priority.col, "not found"))
  
    #all levels of priority col should be in priority
    if (!all(priority %in% unique(X[, priority.col]))) stop("Not all levels of 'priority.col' included in 'priority'") 
  }

  # order by start time
  X <- X[order(X$sound.files, X$start), ]
  
  # save rowname
  X$...ROWNAME... <- 1:nrow(X)
  
 # function that runs on a data frame from a single sound file 
  ovlpFUN <- function(w) {
   
    Y <- X[X$sound.files == w, ]
    
     #only if there is more than 1 selection for that sound file
    if (nrow(Y) > 1) {

      #relabel
      if (relabel)
        Y$selec <- 1:nrow(Y)
      
    # determine which ones overlap
    Y$indx.row <- sapply(1:nrow(Y), function(i) {
    
      # if any of those after i overlap
      ovlp_rows <- Y$...ROWNAME...[(Y$end - max.ovlp) > Y$start[i] & as.numeric(Y$...ROWNAME...) < as.numeric(Y$...ROWNAME...[i]) | Y$start < (Y$end[i] - max.ovlp) & as.numeric(Y$...ROWNAME...) > as.numeric(Y$...ROWNAME...[i])]
     
      if (length(ovlp_rows) > 0)
        out <- paste(sort(as.numeric(c(Y$...ROWNAME...[i], ovlp_rows))), collapse = "/") else
          out <- NA
        
      return(out)
    })
  } else 
     Y$indx.row <- NA
  
   return(Y)  
 }
  
# set clusters for windows OS
if (Sys.info()[1] == "Windows" & parallel > 1)
  cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel

# run loop apply function
ovlp_df_l <- pblapply_wrblr_int(pbar = pb, X = unique(X$sound.files), cl = cl, FUN = ovlpFUN) 

ovlp_df <- do.call(rbind, ovlp_df_l)    
 
rownames(ovlp_df) <- 1:nrow(ovlp_df)

ovlp_df$ovlp.sels <- ovlp_df$indx.row

#unique labels
ovlp_df$ovlp.sels[!is.na(ovlp_df$ovlp.sels)] <- factor(paste0(ovlp_df[!is.na(ovlp_df$ovlp.sels), "sound.files"], ovlp_df[!is.na(ovlp_df$ovlp.sels), "ovlp.sels"]), levels = unique(as.character(paste0(ovlp_df[!is.na(ovlp_df$ovlp.sels), "sound.files"], ovlp_df[!is.na(ovlp_df$ovlp.sels), "ovlp.sels"]))))

# convert to numeric
ovlp_df$ovlp.sels <- as.numeric(ovlp_df$ovlp.sels)

if (index) return(which(!is.na(ovlp_df$ovlp.sels))) else 
  {
    # remove the ones overlapped  
    if (drop)
      {
      # remove based on priority
      if (!is.null(priority.col) & !is.null(priority) &  length(priority) > 1)
        {
        # remove duplicated labels
        priority <- priority[!duplicated(priority)]
        
        # create numeric vector to order resulting data frame before dropping
        ordr <- as.character(ovlp_df[, priority.col])
        for(i in 1:length(priority)) {
          ordr[ordr == priority[i]] <- i 
        }
        
        # order based on priority
        ovlp_df <- ovlp_df[order(ovlp_df$sound.files, as.numeric(ordr)), ]
        }
        
      org.ovlp <- sum(!is.na(ovlp_df$ovlp.sels))
    ovlp_df <- ovlp_df[dups <- !duplicated(ovlp_df[, c("ovlp.sels", "sound.files")]) | is.na(ovlp_df$ovlp.sels), ]
    }
  
  if (pb & verbose)
  if (any(!is.na(ovlp_df$ovlp.sels))) 
    {
    if (drop)
      cat(paste(org.ovlp, "selections overlapped,", sum(!is.na(ovlp_df$ovlp.sels)), "were removed")) else
  cat(sum(!is.na(ovlp_df$ovlp.sels)), "selections overlapped")
  
    } else cat("No overlapping selections were found")
  
  # rename rows
  rownames(ovlp_df) <- ovlp_df$...ROWNAME...
  
  # remove ...ROWNAME...
  ovlp_df$...ROWNAME... <- NULL
  
  #set indx.row to NA when no ovlp.sels
  if (!indx.row)
   ovlp_df$indx.row <- NULL
     
  return(ovlp_df)   
  }
}
  
##############################################################################################################
#' alternative name for \code{\link{overlapping_sels}}
#'
#' @keywords internal
#' @details see \code{\link{overlapping_sels}} for documentation. \code{\link{ovlp_sels}} will be deprecated in future versions.
#' @export

ovlp_sels <- overlapping_sels
