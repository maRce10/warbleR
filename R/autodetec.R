#'
#' \code{autodetec} automatically detects the start and end of vocalizations in sound files based
#' on amplitude, duration, and frequency range attributes.
#' @usage autodetec(X = NULL, wl = 512, threshold = 15, parallel = 1, power = 1, 
#'    output = 'data.frame', thinning = 1/10, path = NULL,pb = TRUE, ssmooth = NULL, 
#'    bp = NULL, flist = NULL, hold.time, envt = NULL, msmooth = NULL, osci = NULL, 
#'    xl = NULL, picsize = NULL, res = NULL, flim = NULL, ls = NULL, sxrow = NULL, 
#'    rows = NULL, mindur = NULL, maxdur = NULL, redo = NULL, img = NULL, it = NULL, 
#'    set = NULL, smadj = NULL, pal = NULL, fast.spec = NULL)
#' @param X 'selection_table' object or a data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end). If provided the detection will be conducted only within
#' the selections in 'X'.
#' @param threshold A numeric vector of length 1 specifying the amplitude threshold for detecting
#'   signals (in \%).
#' @param envt DEPRECATED.
#' @param ssmooth A numeric vector of length 1 to smooth the amplitude envelope
#'   with a sum smooth function. Default is NULL.
#' @param msmooth DEPRECATED.
#' @param power A numeric vector of length 1 indicating a power factor applied to the amplitude envelope. Increasing power will reduce low amplitude modulations and increase high amplitude modulations, in order to reduce background noise. Default is 1 (no change).
#' @param bp Numeric vector of length 2 giving the lower and upper limits of a
#'   frequency bandpass filter (in kHz). Default is c(0, 22).
#' @param osci DEPRECATED.
#' @param wl A numeric vector of length 1 specifying the window used by internally
#' \code{\link[seewave]{ffilter}} for bandpass filtering. Default is 512.
#' @param xl DEPRECATED
#' @param picsize DEPRECATED
#' @param res DEPRECATED
#' @param flim DEPRECATED
#' @param ls DEPRECATED
#' @param sxrow DEPRECATED
#' @param rows DEPRECATED
#' @param mindur Numeric vector of length 1 giving the shortest duration (in
#'   seconds) of the signals to be detected. It removes signals below that
#'   threshold.
#' @param maxdur Numeric vector of length 1 giving the longest duration (in
#'   seconds) of the signals to be detected. It removes signals above that
#'   threshold.
#' @param redo DEPRECATED.
#' @param img DEPRECATED.
#' @param it DEPRECATED.
#' @param set DEPRECATED.
#' @param flist character vector or factor indicating the subset of files that will be analyzed. Ignored
#' if X is provided.
#' @param smadj DEPRECATED.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param pal DEPRECATED.
#' @param fast.spec DEPRECATED.
#' @param output Character string indicating if the output should be a 'data.frame' with the detections (default) or a list (of class 'autodetec.output') containing both 1) the detections and 2) the amplitude envelopes (time vs amplitude) for each sound file. The list can be input into \code{\link{lspec}} to explore detections and associated amplitude envelopes.
#' @param thinning Numeric vector of length 1 in the range 0-~1 indicating the proportional reduction of the number of
#' samples used to represent amplitude envelopes. Usually amplitude envelopes have many more samples
#' than those needed to accurately represent amplitude variation in time, which affects the size of the
#' output (usually very large R objects / files). Default is  \code{1 / 10} (a tenth of the original envelope
#' length). Use NULL to avoid any reduction. Higher sampling rates can afford higher size reduction. Reduction is conducted by interpolation using \code{\link[stats]{approx}}. 
#' @param hold.time Numeric vector of length 1. Specifies the time range at which selections will be merged (i.e. if 2 selections are separated by less than the specified hold.time they will be merged in to a single selection). Default is  \code{NULL}.
#' @return A data frame containing the start and end of each signal by
#'   sound file and selection number. If 'output = "list"' then a list including 1) a detection data frame, 2) amplitude envelopes and 3) parameters will be return. An additional column 'org.selec' is added when 'X' is provided (so detection can be traced back to the selections in 'X').
#' @export
#' @name autodetec
#' @details This function determines the start and end of signals in the sound file selections listed
#'   in the input data frame ('X'). Alternatively, if no data frame is provided, the function detects signals across
#'   each entire sound file. It can also create long spectrograms highlighting the start and of the detected
#'   signals for all sound files in the working directory (if \code{img = TRUE}). Sound files should be located in the
#'    working directory or the path to the sound files should be provided using the 'path' argument. The input
#'    data frame should have the following columns: c("sound.files","selec","start","end"). This function uses a modified version of the \code{\link[seewave]{timer}} function from seewave package to detect signals.
#'
#' @examples{
#' # Save to temporary working directory
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#' ad <- autodetec(threshold = 5, ssmooth = 300,
#' bp = c(2, 9), wl = 300, path = tempdir())
#' }
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}). Implements a
#' modified version of the timer function from seewave.
#last modification on nov-18-2020 (MAS)

autodetec <-
  function(X = NULL,
           wl = 512,
           threshold = 15,
           parallel = 1,
           power = 1,
           output = 'data.frame',
           thinning = 1/10,
           path = NULL,
           pb = TRUE,
           ssmooth = NULL,
           bp = NULL,
           flist = NULL,
           hold.time,
           envt = NULL,
           msmooth = NULL,
           osci = NULL,
           xl = NULL,
           picsize = NULL,
           res = NULL,
           flim = NULL,
           ls = NULL,
           sxrow = NULL,
           rows = NULL,
           mindur = NULL,
           maxdur = NULL,
           redo = NULL,
           img = NULL,
           it = NULL,
           set = NULL,
           smadj = NULL,
           pal = NULL,
           fast.spec = NULL
           ) {
    
    # reset working directory
    on.exit(pbapply::pboptions(type = .Options$pboptions$type))
    
    #### set arguments from options
    # get function arguments
    argms <- methods::formalArgs(autodetec)
    
    # get warbleR options
    opt.argms <-
      if (!is.null(getOption("warbleR")))
        getOption("warbleR") else
      SILLYNAME <- 0
    
    # rename path for sound files
    names(opt.argms)[names(opt.argms) == "wav.path"] <- "path"
    
    # remove options not as default in call and not in function arguments
    opt.argms <-
      opt.argms[!sapply(opt.argms, is.null) &
                  names(opt.argms) %in% argms]
    
    # get arguments set in the call
    call.argms <- as.list(base::match.call())[-1]
    
    # remove arguments in options that are in call
    opt.argms <- opt.argms[!names(opt.argms) %in% names(call.argms)]
    
    # set options left
    if (length(opt.argms) > 0)
      for (q in 1:length(opt.argms))
        assign(names(opt.argms)[q], opt.argms[[q]])
    
    #check path if not provided set to working directory
    if (is.null(path))
      path <- getwd() else
      if (!dir.exists(path))
        stop("'path' provided does not exist") else
      path <- normalizePath(path)
    
    #if bp is not vector or length!=2 stop
    if (!is.null(bp))
    {
      if (!is.vector(bp))
        stop("'bp' must be a numeric vector of length 2")  else {
        if (!length(bp) == 2)
          stop("'bp' must be a numeric vector of length 2")
      }
    }
    
    #if ssmooth is not vector or length!=1 stop
    if (!is.null(ssmooth)) {
      if (!is.vector(ssmooth))
        stop("'ssmooth' must be a numeric vector of length 1") else {
        if (!length(ssmooth) == 1)
          stop("'ssmooth' must be a numeric vector of length 1")
      }
    }
    
    #if wl is not vector or length!=1 stop
    if (is.null(wl))
      stop("'wl' must be a numeric vector of length 1") else {
      if (!is.vector(wl))
        stop("'wl' must be a numeric vector of length 1") else {
        if (!length(wl) == 1)
          stop("'wl' must be a numeric vector of length 1")
      }
    }
    
    #if threshold is not vector or length!=1 stop
    if (is.null(threshold))
      stop("'threshold' must be a numeric vector of length 1") else {
      if (!is.vector(threshold))
        stop("'threshold' must be a numeric vector of length 1") else {
        if (!length(threshold) == 1)
          stop("'threshold' must be a numeric vector of length 1")
      }
    }
    
    #if flist is not character vector
    if (!is.null(flist) &
        is.null(X) &
        any(!is.character(flist),!is.vector(flist)))
      stop("'flist' must be a character vector")
    
    #if parallel is not numeric
    if (!is.numeric(parallel))
      stop("'parallel' must be a numeric vector of length 1")
    if (any(!(parallel %% 1 == 0), parallel < 1))
      stop("'parallel' should be a positive integer")
    
    # set pb options
    pbapply::pboptions(type = ifelse(pb, "timer", "none"))
    
    #stop if power is 0
    if (power == 0)
      stop("'power' cannot equal to 0")
    
    if (!is.null(X)) {
      
      # extract selection table and envelopes
      if (is(X, "autodetec.output")) {
        
        X.class <- "autodetec.output"
      write(file = "", x = "Working on an 'autodetec.output' object")
      
      xprov <- TRUE
      
      } else 
        X.class <- "selection.table"   
      
      # if is selection table  
      if (X.class == "selection.table") { 
      
        #if files not found
      if (length(list.files(
        path = path,
        pattern = "\\.wav$",
        ignore.case = TRUE
      )) == 0)
        if (is.null(path))
          stop("No .wav files in working directory") else
            stop("No .wav files found")
      
      
      #if X is not a data frame
      if (!any(is.data.frame(X), is_selection_table(X)))
        stop("X is not of a class 'data.frame' or 'selection_table'")
      
      #check if all columns are found
      if (any(!(c(
        "sound.files", "selec", "start", "end"
      ) %in% colnames(X))))
        stop(paste(paste(
          c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec",
                                                        "start", "end") %in% colnames(X))], collapse =
            ", "
        ), "column(s) not found in data frame"))
      
      #if there are NAs in start or end stop
      if (any(is.na(c(X$end, X$start))))
        stop("NAs found in start and/or end columns")
      
      #if end or start are not numeric stop
      if (any(!is(X$end, "numeric"),!is(X$start, "numeric")))
        stop("'start' and 'end' must be numeric")
      
      #if any start higher than end stop
      if (any(X$end - X$start <= 0))
        stop(paste(
          "Start is higher than or equal to end in",
          length(which(X$end - X$start <= 0)),
          "case(s)"
        ))
      
      #return warning if not all sound files were found
      fs <-
        list.files(path = path,
                   pattern = "\\.wav$",
                   ignore.case = TRUE)
      if (length(unique(X$sound.files[(X$sound.files %in% fs)])) != length(unique(X$sound.files)))
        cat(paste(length(unique(X$sound.files)) - length(unique(X$sound.files[(X$sound.files %in% fs)])),
                  ".wav file(s) not found"))
      
      #count number of sound files in working directory and if 0 stop
      d <- which(X$sound.files %in% fs)
      if (length(d) == 0)
        stop("The .wav files are not in the working directory") else
        X <- X[d, ]
      xprov <- TRUE #to replace X if not provided
      } else {
        
        # extract selection table and envelopes as separate objects
        envelopes <- X$envelopes
        X <- X$org.selection.table
        }
    } else {
      if (!is.null(flist))
        X <- warbleR::wavdur(files = flist, path = path) else
        X <- warbleR::wavdur(path = path)
      X$start <- 0
      X$selec <- 1
      names(X)[2] <- "end"
      xprov <- FALSE #to replace X if not provided
      if (nrow(X) == 0)
        stop("Files in 'flist' not in working directory")
    
      X.class <- "selection.table"   
      }
    
    # message deprecated
    if (!is.null(smadj))
      write(file = "", x = "'smadj' has been deprecated")
 
    if (!is.null(envt))
      write(file = "", x = "'envt' has been deprecated. Only absolute envelopes can be used now")
    
    if (!is.null(msmooth))
      write(file = "", x = "'msmooth' has been deprecated. Only 'ssmooth' is available for smoothing")
    
    if (!is.null(img))
      write(file = "", x = "'img' has been deprecated. Use full_spec() to create images from autodetec() output")
    
    if (!is.null(xl))
      write(file = "", x = "'xl' has been deprecated. Use full_spec() to create images from autodetec() output")
    
    if (!is.null(picsize))
      write(file = "", x = "'picsize' has been deprecated. Use full_spec() to create images from autodetec() output")
    
    if (!is.null(flim))
      write(file = "", x = "'flim' has been deprecated. Use full_spec() to create images from autodetec() output")    
    
    if (!is.null(rows))
      write(file = "", x = "'rows' has been deprecated. Use full_spec() to create images from autodetec() output")    
    
    if (!is.null(sxrow))
      write(file = "", x = "'sxrow' has been deprecated. Use full_spec() to create images from autodetec() output")    
    
    if (!is.null(osci))
      write(file = "", x = "'osci' has been deprecated. Use full_spec() to create images from autodetec() output")    
    
    if (!is.null(res))
      write(file = "", x = "'res' has been deprecated. Use full_spec() to create images from autodetec() output")    
    
    if (!is.null(ls))
      write(file = "", x = "'ls' has been deprecated. Use full_spec() to create images from autodetec() output")    
    
    if (!is.null(redo))
      write(file = "", x = "'redo' has been deprecated. Use full_spec() to create images from autodetec() output")
    
    if (!is.null(it))
      write(file = "", x = "'it' has been deprecated. Use full_spec() to create images from autodetec() output")
    
    if (!is.null(set))
      write(file = "", x = "'set' has been deprecated. Use full_spec() to create images from autodetec() output")
    
    # if parallel was not called
    if (pb)
        cat("Detecting signals in sound files:")
    
   # function for detecting signals
    adFUN <-
      function(i,
               X,
               wl,
               bp,
               envt,
               thinning,
               threshold,
               ssmooth,
               mindur,
               maxdur,
               output,
               power,
               X.class
               )
      {
        
        # set threshold as proportion
        thres <- threshold / 100
        
        if (X.class == "selection.table") {
        
        # read wave object
        song <- warbleR::read_wave(X = X,
                                   path = path,
                                   index = i)
        
          # set sample rate and duration
          f <- song@samp.rate
          
          #filter frequnecies below 1000 Hz
          if (!is.null(bp))
            f.song <-
            seewave::ffilter(
              song,
              f = f,
              from = bp[1] * 1000,
              to = bp[2] * 1000,
              bandpass = TRUE,
              wl = wl,
              output = "Wave"
            ) else
              f.song <- song
          
          #detect songs based on amplitude (modified from seewave::timer function)
          amp_vector <- f.song@left
         
          n <- length(amp_vector)
          
          # extract envelope
          envp <-
            warbleR::envelope(
              wave = amp_vector,
              ssmooth = ssmooth
            )

          # flat edges
          # if (envp[1] < min(envp)) envp[1:min(which(envp >= min(envp)))] <- min(envp)
          #   
          # if (envp[length(envp)] < min(envp)) envp[max(which(envp >= min(envp))):length(envp)] <- min(envp)
              
          # force to be in the range 0-1
          envp <- envp - min(envp)
          envp <- envp / max(envp)
          envp <- matrix(envp, ncol = 1)

        } 
        
        # if autodetec output
        if (X.class == "autodetec.output") { # if is and autodetec.output object
        
          # extract envelopes from autodetec.output object
          if (is.null(X$org.selec))
          envp <- envelopes[envelopes$sound.files == X$sound.files[i], ] else
            envp <- envelopes[envelopes$sound.files == X$sound.files[i] & envelopes$org.selec == X$org.selec[i], ] 
          
          # convert to matrix of 1 column as the output of env()
          envp <- matrix(data = envp$amplitude, ncol = 1)
          
          # set sample rate
          f <- nrow(envp) / (X$end[i] - X$start[i])
        }
          
          if (!is.null(thinning)) {
            
            # reduce size of envelope
            app_env <-
              stats::approx(
                x = seq(0,  X$end[i] - X$start[i], length.out = nrow(envp)),
                y = envp[, 1],
                n = round(nrow(envp) * thinning),
                method = "linear"
              )$y
            
            # back into a 1 column matrix
            envp <- matrix(data = app_env, ncol = 1)
            
            n <- nrow(envp)
            f <- f * thinning
          }
        

        
          #### detection ####
          # get original number of samples
          n1 <- length(envp)
          f1 <- f * (n1 / n)
          
          # add power
          if (power != 1){
            envp <- envp ^ power
            envp <- envp / max(envp)
            }
          
          
          # get binary values if above or below threshold
          binary_treshold <- ifelse(envp <= thres, yes = 1, no = 2)
          n2 <- length(binary_treshold)
          
          cross <- sapply(2:length(binary_treshold), function(x) {
            
            if (binary_treshold[x] > binary_treshold[x - 1]) out <- "u" # u means going up
            if (binary_treshold[x] < binary_treshold[x - 1]) out <- "d" # d means going down
            if (binary_treshold[x]  == binary_treshold[x - 1])  
              if (binary_treshold[x] == 2)  out <- "a"  else# a means above
                out <- "b" # b means below
            
            return(out)
            
            })      
          
          cross <- c(if (binary_treshold[1] == 1) "b" else "a", cross)
          
          # time series
          cross_ts <- ts(cross,
             start = X$start[i],
             end =  X$end[i],
             frequency = length(cross) / ( X$end[i] - X$start[i]))
          
          starts <- time(cross_ts)[cross_ts == "u"]
          ends <- time(cross_ts)[cross_ts == "d"]
          
          # if there are both starts and ends detected
          if (length(starts) > 0 & length(ends) > 0){
            # if  start is not the first detection
                if (starts[1] > ends[1]) starts <- c(0, starts)
          if (starts[length(starts)] > ends[length(ends)]) ends <- c(ends, X$end[i] - X$start[i])
          }
          
          # if there is no end
          if (length(starts) > 0 & length(ends) == 0) ends <- X$end[i] - X$start[i]
          
          # if there is no start
          if (length(ends) > 0 & length(starts) == 0) starts <- 0
          
          
          #put time of detection in data frame
          detec_tab <-
            data.frame(
              sound.files = X$sound.files[i],
              duration = ends - starts,
              org.selec = X$selec[i], # this one allows to relate to segments in a segmented sound file n X (several selection for the same sound file)
              selec = NA,
              start = starts,
              end = ends
            )
          
          
          #remove signals based on duration
          if (!is.null(mindur))
            detec_tab <- detec_tab[detec_tab$duration > mindur, ]
          if (!is.null(maxdur))
            detec_tab <- detec_tab[detec_tab$duration < maxdur, ]
          
          if (nrow(detec_tab) > 0)
          {
            if (xprov)
              detec_tab$selec <-
                paste(X$selec[i], 1:nrow(detec_tab), sep = "-") else
                  detec_tab$selec <- 1:nrow(detec_tab)
          }
          
          #if nothing was detected
          if (nrow(detec_tab) == 0)
            detec_tab <-
            data.frame(
              sound.files = X$sound.files[i],
              duration = NA,
              org.selec = X$selec[i],
              selec = NA,
              start = NA,
              end = NA
            )

        
          
        if (output == "data.frame")
          # return data frame or list
          return(detec_tab) else {
            output_list <- list(
              selec.table = detec_tab,
              envelopes = data.frame(
                sound.files = X$sound.files[i],
                org.selec = X$selec[i],
                time = seq(X$start[i], X$end[i], along.with = envp),
                abs.time = NA,
                amplitude = envp
              )
            )
           
            return(output_list)
          }

      }
    
    #Apply over each sound file
    # set clusters for windows OS
    if (Sys.info()[1] == "Windows" & parallel > 1)
      cl <- parallel::makeCluster(parallel) else
      cl <- parallel
    
    # run function over sound files or selections in loop
    ad <- pbapply::pblapply(
      X = 1:nrow(X),
      cl = cl,
      FUN = function(i)
      {
        adFUN(i,
              X,
              wl,
              bp,
              envt,
              thinning,
              threshold,
              ssmooth,
              mindur,
              maxdur,
              output,
              power,
              X.class
              )
      }
    )
    
    if (output == "data.frame")
      detections <- do.call(rbind, ad) else
    {
      # if output is a list
      detections <- do.call(rbind, lapply(ad, '[[', 1))
      
      # envelopes
      envelopes <- do.call(rbind, lapply(ad, '[[', 2))
      
      if (!xprov)
        envelopes$org.selec <- NULL
    }
   
     #rename rows
    rownames(detections) <- 1:nrow(detections)
    
    # remove org.selec if X was not provided
    if (!xprov)
    detections$org.selec <- NULL
    
    # merge selections based on hold time
    if (!missing(hold.time) & nrow(detections) > 1) {
      
      # detections$end <- detections$end + hold.time
      detections$ovlp.sels <- NA
      
      # label overlapping signals (as in ovlp_sels())
      # calculate overlapping selection after adding hope time
      for(e in 1:(nrow(detections) - 1)) {
        # if overlap
      if (detections$sound.files[e] == detections$sound.files[e + 1])
        if (detections$end[e] + hold.time >= detections$start[e + 1]) 
          if (all(is.na(detections$ovlp.sels))) detections$ovlp.sels[c(e, e + 1)] <- 1 else # return 1 if is the first overlap
            if (is.na(detections$ovlp.sels[e])) # if current is NA add 1
            detections$ovlp.sels[c(e, e + 1)] <- max(detections$ovlp.sels, na.rm = TRUE) + 1 else
              detections$ovlp.sels[e + 1] <- detections$ovlp.sels[e] # otherwise use current for next
        }

      # subset non-overlapping and overlapping
      no_ovlp <- detections[is.na(detections$ovlp.sels), ]
      ovlp <- detections[!is.na(detections$ovlp.sels), ]
      
      # if some overlaps detected
      if (nrow(ovlp) > 0)  {
        
        # loop to merge selections
        out <-
          pbapply::pblapply(unique(ovlp$ovlp.sels), cl = cl, function(x) {
            # subset for one level
            Y <- ovlp[ovlp$ovlp.sels == x, ]
            
            # keep only one per overlapping group label
            Z <- Y[1, , drop = FALSE]
            
            # start is the minimum of all starts
            Z$start <- min(Y$start)
            
            # end is the maximum of all ends
            Z$end <- max(Y$end)
            
            return(Z)
          })
        
        # put list together in a data frame
        ovlp <- do.call(rbind, out)
        
        # add non-overlapping selections
        detections <- rbind(ovlp, no_ovlp)
        
        # remove extra column
        detections$ovlp.sels <- NULL
        
        # order selections by sound file and time
        detections <- detections[order(detections$sound.files, detections$start), ]
      } else detections <- no_ovlp # if not return non-overlapping
    }
    
    # output as data frame or list
    if (output == "data.frame")
      return(detections) else {
      output_list <- list(
        selection.table = detections,
        envelopes = envelopes,
        parameters = lapply(call.argms, eval),
        org.selection.table = X
      )
      
      # add class autodetec
      class(output_list) <- c("list", "autodetec.output")
      
      return(output_list)
     
    }
  }


##############################################################################################################
#' alternative name for \code{\link{autodetec}}
#'
#' @keywords internal
#' @details see \code{\link{autodetec}} for documentation. \code{\link{autodetec}} will be deprecated in future versions.
#' @export

auto_detec <- autodetec