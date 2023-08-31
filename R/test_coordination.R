#' Randomization test for singing coordination
#'
#' Monte Carlo randomization test to assess the statistical significance of overlapping or alternating singing (or any other simultaneously occurring behavior).
#' @usage test_coordination(X, iterations = 1000, ovlp.method = "count",
#' randomization = "keep.gaps", less.than.chance = TRUE, parallel = 1, pb = TRUE,
#' rm.incomp = FALSE, cutoff = 2, rm.solo = FALSE)
#' @param  X Data frame containing columns for singing event (sing.event),
#' individual (indiv), and start and end time of signal (start and end).
#' @param iterations number of iterations for shuffling and calculation of the expected number of overlaps. Default is 1000.
#' @param ovlp.method Character string defining the method to measure the amount of overlap. Three methods are available:
#' \itemize{
#'    \item \code{count}: count the number of overlapping signals (default)
#'    \item \code{time.overlap}: measure the total duration (in s) in which signals overlap
#'    \item \code{time.distance}: measure the time (in s) to the other individual's closest signal. This is the only method that can take more than 2 individuals.
#'    }
#' @param randomization Character string defining the procedure for signal randomization. Three methods are available:
#' \itemize{
#'  \item \code{keep.gaps} the position of both signals and gaps (i.e. intervals between signals) are randomized. Default.
#'  \item \code{sample.gaps} gaps are simulated using a lognormal distribution with
#'  mean and standard deviation derived from the observed gaps. Signal position is randomized.
#'  \item \code{keep.song.order} only the position of gaps is randomized.
#' }
#' More details in Masco et al. (2015).
#' @param less.than.chance Logical. If \code{TRUE} the test evaluates whether overlaps occur less often than expected by chance.
#' If \code{FALSE} the opposite pattern is evaluated (whether overlaps occur more often than expected by chance).
#' Default is \code{TRUE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param rm.incomp Logical. If \code{TRUE} removes the events that don't have 2 interacting individuals. Default is
#'  \code{FALSE}.
#' @param cutoff Numeric. Determines the minimum number of signals per individual in a singing event. Events not meeting
#' this criterium are removed. Default is 2.
#' Note that randomization tests are not reliable with very small sample sizes. Ideally 10 or more signals per individual
#' should be available in each singing event.
#' @param rm.solo Logical. Controls if signals that are not alternated at the start or end of the
#' sequence are removed (if \code{TRUE}). For instance, the sequence of signals A-A-A-B-A-B-A-B-B-B (in which A and B represent different individuals, as in the 'indiv' column) would be subset to
#' A-B-A-B-A-B. Default is  \code{FALSE}.
#' @return A data frame with the following columns:
#' \itemize{
#'    \item \code{sing.event}: singing event ID
#'    \item \code{obs.overlap}: observed amount of overlap (counts or total duration, depending on overlap method, see 'ovlp.method' argument)
#'    \item \code{mean.random.ovlp}: mean amount of overlap expected by chance
#'    \item \code{p.value}: p value
#'    \item \code{coor.score}: coordination score (\emph{sensu} Araya-Salas et al. 2017),
#'    calculated as:
#'    \deqn{(obs.overlap - mean.random.ovlp) / mean.random.ovlp}
#'    Positive values indicate a tendency to overlap while negative values indicate a tendency to alternate. NA values will be returned when events cannot be randomized (e.g. too few signals).
#'    }
#' @export
#' @name test_coordination
#' @details This function calculates the probability of finding an equal or more extreme amount of song overlap (higher or lower) in a coordinated singing event (or any pair-coordinated behavior).
#' The function shuffles the sequences of signals and silence-between-signals for both individuals to produce
#' a null distribution of overlaps expected by chance. The observed overlaps is compared to this
#' expected values. The p-values are calculated as the proportion of random expected values that were lower (or higher)
#' than the observed value. All procedures described in Masco et al. (2015) are implemented. In addition, either the number (\code{ovlp.method = "count"}) or the total duration (\code{ovlp.method = "time.overlap"}) in which signals overlap can be used for estimating the overall degree of overlap. The function runs one test for each singing event in the input data frame. This function assumes that there are no overlaps between signals belonging to the same individual. See Masco et al. (2015) for recommendations on randomization procedures for specific signal structures.
#' @examples{
#' #load  simulated singing data (see data documentation)
#' data(sim_coor_sing)
#'
#' # set global options (this can also be set within the function call)
#' warbleR_options(iterations = 100, pb = FALSE)
#'
#' # testing if coordination happens less than expected by chance
#' test_coordination(sim_coor_sing)
#'
#' # testing if coordination happens more than expected by chance
#' test_coordination(sim_coor_sing, less.than.chance = FALSE)
#'
#' # using "duration" method and "keep.song.order" as randomization procedure
#' test_coordination(sim_coor_sing, ovlp.method =  "time.overlap",
#' randomization = "keep.song.order")
#' }
#'
#' @references
#' {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals.
#'  Methods in Ecology and Evolution, 8(2), 184-191.
#'
#' Araya-Salas M., Wojczulanis-Jakubas K., Phillips E.M., Mennill D.J., Wright T.F.
#' (2017) To overlap or not to overlap: context-dependent coordinated singing in
#' lekking long-billed hermits. Animal Behavior  124, 57-65.
#'
#' Keenan EL, Odom KJ, M Araya-Salas, KG Horton, M Strimas-Mackey,MA Meatte, NI Mann,PJ Slater,JJ Price, and CN Templeton . 2020. Breeding season length predicts duet coordination and consistency in Neotropical wrens (Troglodytidae). Proceeding of the Royal Society B. 20202482.
#'
#' Masco, C., Allesina, S., Mennill, D. J., and Pruett-Jones, S. (2015). The Song
#' Overlap Null model Generator (SONG): a new tool for distinguishing between random
#' and non-random song overlap. Bioacoustics.
#'
#' Rivera-Caceres K, E Quiros-Guerrero E, M Araya-Salas, C Templeton & W Searcy. (2018). Early development of vocal interaction rules in a duetting songbird. Royal Society Open Science. 5, 171791.
#'
#' Rivera-Caceres K, E Quiros-Guerrero, M Araya-Salas & W Searcy. (2016). Neotropical wrens learn new duet as adults. Proceedings of the Royal Society B. 285, 20161819
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on apr-11-2018 (MAS)

test_coordination <- function(X = NULL, iterations = 1000, ovlp.method = "count",
                              randomization = "keep.gaps", less.than.chance = TRUE, parallel = 1, pb = TRUE,
                              rm.incomp = FALSE, cutoff = 2, rm.solo = FALSE) {
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(test_coordination)

  # get warbleR options
  opt.argms <- if (!is.null(getOption("warbleR"))) getOption("warbleR") else SILLYNAME <- 0

  # remove options not as default in call and not in function arguments
  opt.argms <- opt.argms[!sapply(opt.argms, is.null) & names(opt.argms) %in% argms]

  # get arguments set in the call
  call.argms <- as.list(base::match.call())[-1]

  # remove arguments in options that are in call
  opt.argms <- opt.argms[!names(opt.argms) %in% names(call.argms)]

  # set options left
  if (length(opt.argms) > 0) {
    for (q in seq_len(length(opt.argms))) {
      assign(names(opt.argms)[q], opt.argms[[q]])
    }
  }

  if (!is.data.frame(X)) stop2("X is not a data frame")

  # stop if some cells are not labeled
  if (any(is.na(X$sing.event))) stop2("NA's in singing event names ('sing.event' column) not allowed")

  if (any(is.na(X$indiv))) stop2("NA's in individual names ('indiv' column) not allowed")

  # if there are NAs in start or end stop
  if (any(is.na(c(X$end, X$start)))) stop2("NAs found in start and/or end")

  # remove hidden levels
  X <- droplevels(X)

  # remove solo singing
  if (rm.solo) {
    rmslX <- lapply(unique(X$sing.event), function(x) {
      Y <- X[X$sing.event == x, ]

      Y <- Y[order(Y$start), ]
      Y_list <- split(Y, Y$indiv)

      fst <- max(sapply(Y_list, function(x) which(Y$start == min(x$start)))) - 1
      lst <- min(sapply(Y_list, function(x) which(Y$start == max(x$start)))) - 1

      if (lst > nrow(Y)) {
        lst <- nrow(Y)
      }

      Y <- Y[fst:lst, ]
    })

    X <- do.call(rbind, rmslX)
  }

  # stop if some events do not have 2 individuals
  qw <- as.data.frame(tapply(X$sing.event, list(X$sing.event, X$indiv), length))
  qw2 <- qw
  qw2[qw2 > 0] <- 1
  indiv.cnt <- apply(qw2, 1, sum, na.rm = TRUE)
  sng.cnt <- apply(qw, 1, function(x) any(na.omit(x) < cutoff))

  # complete singing events
  if (any(indiv.cnt < 2)) {
    if (rm.incomp) {
      X <- X[X$sing.event %in% names(indiv.cnt)[indiv.cnt == 2], ]
      warning2("Some events didn't have 2 interacting individuals and were excluded")
    } else {
      warning2("Some singing events don't have 2 interacting individuals ('indiv' column)")
    }
  }

  # if any more event has more than 2 individuals
  if (any(indiv.cnt > 2) & ovlp.method != "time.closest") {
    stop2("Some events have more than 2 individuals, this is only possible with ovlp.method = 'time.closest'")
  }

  # deal with cutoff value
  if (any(sng.cnt)) {
    X <- X[X$sing.event %in% names(indiv.cnt)[!sng.cnt], ]
    warning2("Some individuals didn't have more songs that the 'cutoff' and the events were excluded")
  }

  # if nothing was left
  if (nrow(X) == 0) stop2("No events left after removing incomplete events")

  # if iterations is not vector or length==1 stop
  if (any(!is.vector(iterations), !is.numeric(iterations))) {
    stop2("'interations' must be a numeric vector of length 1")
  } else {
    if (!length(iterations) == 1) stop2("'interations' must be a numeric vector of length 1")
  }

  # round iterations
  iterations <- round(iterations)

  # interations should be positive
  if (iterations < 2) stop2("'iterations' must be > 1")

  # if parallel is not numeric
  if (!is.numeric(parallel)) stop2("'parallel' must be a numeric vector of length 1")
  if (any(!(parallel %% 1 == 0), parallel < 1)) stop2("'parallel' should be a positive integer")

  # randomization function
  rndmFUN <- function(Y) {
    Y <- Y[order(Y$start), ]
    Y_list <- split(Y, Y$indiv)

    # null model
    # duration of signals
    durs <- lapply(Y_list, function(x) x$end - x$start)

    # duration of gaps
    gaps <- lapply(Y_list, function(y) {
      sapply(1:(nrow(y) - 1), function(x) {
        y$start[x + 1] - y$end[x]
      })
    })

    # randomize
    rnd.dfs <- lapply(1:iterations, function(x) {
      # randomize gaps
      if (randomization %in% c("keep.gaps", "keep.song.order")) {
        gaps <- lapply(gaps, sample)
      }

      if (randomization == "sample.gaps") {
        # generate gaps from lognormal distribution
        gaps <- lapply(gaps, function(x) {
          stats::rlnorm(n = length(x), meanlog = mean(log(unlist(gaps))), sdlog = stats::sd(log(unlist(gaps))))
        })
      }

      # randomize signals
      if (randomization %in% c("keep.gaps", "sample.gaps")) {
        durs <- lapply(durs, sample)
      }

      # put all back together as a sequence of signals and gaps
      ndfs_list <- lapply(names(Y_list), function(x) {
        nbt <- NULL
        for (i in 1:(length(durs[[x]]) - 1))
        {
          nbt[i] <- durs[[x]][i] + gaps[[x]][i]
          if (i != 1) nbt[i] <- nbt[i] + nbt[i - 1]
        }
        nbt <- c(0, nbt)
        nbt <- nbt + min(Y_list[[x]]$start)
        net <- nbt + durs[[x]]

        ndf <- data.frame(
          indiv = x,
          start = nbt,
          end = net
        )

        return(ndf)
      })

      ndfs <- do.call(rbind, ndfs_list)
      ndfs <- ndfs[order(ndfs$start), ]

      rownames(ndfs) <- 1:nrow(ndfs)

      return(ndfs)
    })


    # add observed as the first element of list
    dfs <- c(list(Y), rnd.dfs)

    return(dfs)
  }

  # counting ovlp.method
  countFUN <- function(Z) {
    # order by time and add duration
    Z <- Z[order(Z$start), ]

    Z1 <- Z[Z$indiv == unique(Z$indiv)[1], ]

    Z2 <- Z[Z$indiv == unique(Z$indiv)[2], ]

    out <- sapply(1:nrow(Z1), function(i) {
      # target start and end
      trg.strt <- Z1$start[i]
      trg.end <- Z1$end[i]

      # get the ones that overlap
      return(sum(Z2$end > trg.strt & Z2$start < trg.end))
    })

    return(sum(out))
  }

  # time.overlap
  durFUN <- function(Z) {
    # order by time and add duration
    Z <- Z[order(Z$start), ]
    Z$duration <- Z$end - Z$start

    Z1 <- Z[Z$indiv == unique(Z$indiv)[1], ]

    Z2 <- Z[Z$indiv == unique(Z$indiv)[2], ]

    out <- sapply(1:nrow(Z1), function(i) {
      # target start and end
      trg.strt <- Z1$start[i]
      trg.end <- Z1$end[i]

      # get the ones that overlap
      Z2 <- Z2[Z2$end > trg.strt & Z2$start < trg.end, , drop = FALSE]

      if (nrow(Z2) > 0) # set new start and end at edges of overlaping signals
        {
          if (any(Z2$start < trg.strt)) {
            trg.strt <- max(Z2$end[Z2$start < trg.strt])
          }

          if (any(Z2$end > trg.end)) {
            trg.end <- min(Z2$start[Z2$end > trg.end])
          }

          # new duration
          no.ovlp.dur <- trg.end - trg.strt

          ovlp <- if (no.ovlp.dur > 0) Z1$duration[i] - no.ovlp.dur else Z1$duration[i]

          return(ovlp)
        } else {
        return(0)
      }
    })

    return(sum(out))
  }

  # time.distance ovlp.method
  closestFUN <- function(Z) {
    timediffs <- vapply(seq_len(nrow(Z)), function(i) {
      Z_others <- Z[Z$indiv != Z$indiv[i], ]

      timediff <- if (any(Z_others$end > Z$start[i] & Z_others$start < Z$end[i])) {
        0
      } else {
        min(abs(c(Z_others$start - Z$end[i], Z_others$end - Z$start[i])))
      }

      return(timediff)
    }, FUN.VALUE = numeric(1))

    return(mean(timediffs))
  }

  # select function/ovlp.method
  coortestFUN <- if (ovlp.method == "count") {
    countFUN
  } else
  # duration kept for compatibility with previous versions
  if (ovlp.method %in% c("time.overlap", "duration")) {
    durFUN
  } else
  # time to closest call from other individuals
  if (ovlp.method == "time.closest") closestFUN

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1) {
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel))
  } else {
    cl <- parallel
  }

  # run loop apply function
  cote <- pblapply_wrblr_int(pbar = pb, X = unique(X$sing.event), cl = cl, FUN = function(h) {
    ovlp <- try(sapply(rndmFUN(X[X$sing.event == h, ]), coortestFUN))

    if (!is(ovlp, "try-error")) {
      # get observed overlap (first element)
      obs.overlaps <- ovlp[1]

      # get random overlap (all except the first element)
      rov <- ovlp[-1]
      mean.random.ovlps <- mean(rov)

      # calculate p-value
      if (less.than.chance) p <- length(rov[rov <= obs.overlaps]) / iterations else p <- length(rov[rov >= obs.overlaps]) / iterations

      # coordination score
      if (obs.overlaps == 0 & mean.random.ovlps == 0) {
        coor.score <- 0
      } else {
        coor.score <- round((obs.overlaps - mean.random.ovlps) / mean.random.ovlps, 3)
      }

      l <- data.frame(sing.event = h, obs.ovlp = obs.overlaps, mean.random.ovlp = mean.random.ovlps, p.value = p, coor.score)
    } else {
      l <- data.frame(sing.event = h, obs.ovlp = NA, mean.random.ovlp = NA, p.value = NA, coor.score = NA)
    }

    return(l)
  })

  df <- do.call(rbind, cote)

  return(df)
}


##############################################################################################################
#' alternative name for \code{\link{test_coordination}}
#'
#' @keywords internal
#' @details see \code{\link{test_coordination}} for documentation. \code{\link{coor.test}} will be deprecated in future versions.
#' @export

coor.test <- test_coordination
