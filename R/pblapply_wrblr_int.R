# internal warbleR function, not to be called by users. It is a modified version of pbapply::pblapply
# that allows to define internally if progress bar would be used (pbapply::pblapply uses pboptions to do this) 
#last modification on aug-10-2021 (MAS)
pblapply_wrblr_int <- function(X, FUN, cl = NULL, pbar = TRUE, ...) {
  
  # conver parallel 1 to null
  if (cl == 1) cl <- NULL
  
  FUN <- match.fun(FUN)
  if (!is.vector(X) || is.object(X)) 
    X <- as.list(X)
  if (!length(X)) 
    return(lapply(X, FUN, ...))
  if (!is.null(cl)) {
    if (.Platform$OS.type == "windows") {
      if (!inherits(cl, "cluster")) 
        cl <- NULL
    } else {
      if (inherits(cl, "cluster")) {
        if (length(cl) < 2L) 
          cl <- NULL
      } else {
        if (cl < 2) 
          cl <- NULL
      }
    }
  }
  
  if (is.null(cl)) {
    if (!pbar) 
      return(lapply(X, FUN, ...))
    Split <- splitpb(length(X), 1L, nout = 100)
    B <- length(Split)
    pb <- startpb(0, B)
    on.exit(closepb(pb), add = TRUE)
    rval <- vector("list", B)
    for (i in seq_len(B)) {
      rval[i] <- list(lapply(X[Split[[i]]], FUN, ...))
      setpb(pb, i)
    }
  } else {
    if (inherits(cl, "cluster")) {
      PAR_FUN <- parallel::parLapply
      if (pbar) 
        return(PAR_FUN(cl, X, FUN, ...))
      Split <- splitpb(length(X), length(cl), nout = 100)
      B <- length(Split)
      pb <- startpb(0, B)
      on.exit(closepb(pb), add = TRUE)
      rval <- vector("list", B)
      for (i in seq_len(B)) {
        rval[i] <- list(PAR_FUN(cl, X[Split[[i]]], FUN, 
                                ...))
        setpb(pb, i)
      }
    } else {
      if (!pbar) 
        return(parallel::mclapply(X, FUN, ..., mc.cores = as.integer(cl)))
      Split <- splitpb(length(X), as.integer(cl), nout = 100)
      B <- length(Split)
      pb <- startpb(0, B)
      on.exit(closepb(pb), add = TRUE)
      rval <- vector("list", B)
      for (i in seq_len(B)) {
        rval[i] <- list(parallel::mclapply(X[Split[[i]]], 
                                           FUN, ..., mc.cores = as.integer(cl)))
        setpb(pb, i)
      }
    }
  }
  rval <- do.call(c, rval, quote = TRUE)
  names(rval) <- names(X)
  rval
}
