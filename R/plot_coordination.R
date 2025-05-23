#' Coordinated singing graphs
#'
#' \code{plot_coordination} creates graphs of coordinated singing and highlights the signals that overlap
#' in time. The signals are represented by polygons of different colors.
#' @param  X Data frame containing columns for singing event (sing.event),
#' individual (indiv), and start and end time of signal (start and end).
#' @param only.coor Logical. If \code{TRUE} only the segment in which both individuals are singing is
#' included (solo singing is removed). Default is \code{FALSE}.
#' @param ovlp Logical. If \code{TRUE} the vocalizations that overlap in time are highlighted.
#' Default is \code{TRUE}.
#' @param xl Numeric vector of length 1, a constant by which to scale
#'   image width. Default is 1.
#' @param res Numeric argument of length 1. Controls image resolution. Default is 80.
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param img Logical argument. If \code{FALSE}, image files are not produced and
#'  the graphs are shown in the current graphic device. Default is \code{TRUE}.
#' @param tlim Numeric vector of length 2 indicating the start and end time of the coordinated singing events
#' to be displayed in the graphs.
#' @param pb Logical argument to control progress bar and messages. Default is
#' \code{TRUE}.
#' @return The function returns a list of graphs, one for each singing event in the input data frame. The graphs can be plotted by simply calling the list. If 'img' is \code{TRUE} then the graphs are also saved in the working
#' directory as files.
#'
#' @export
#' @name plot_coordination
#' @details This function provides visualization for coordination of acoustic signals. Signals are shown as
#' polygon across a time axis. It also shows which signals overlap, the amount of overlap, and
#' highlights the individual responsible for the overlap using a color code. The width of the polygons
#' depicting the time of overlap.
#' @examples
#' \dontrun{
#' # load simulate singing events (see data documentation)
#' data(sim_coor_sing)
#'
#' #' # make plot_coordination in graphic device format
#' cgs <- plot_coordination(X = sim_coor_sing, ovlp = TRUE, only.coor = FALSE, img = FALSE)
#'
#' cgs
#' }
#'
#' @references {Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on aug-13-2016 (MAS)

plot_coordination <- function(X = NULL, only.coor = FALSE, ovlp = TRUE, xl = 1, res = 80, it = "jpeg",
                              img = TRUE, tlim = NULL, pb = TRUE) {
  # error message if ggplot2 is not installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop2("must install 'ggplot2' to use plot_coordination()")
  }

  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(plot_coordination)

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

  # stop if some events have less than 10 observations
  if (any(table(X$sing.event) < 10)) warning2("At least one singing event with less than 10 vocalizations")

  # stop if some cells are not labeled
  if (any(is.na(X$sing.event))) stop2("NA's in singing event names ('sing.event' column) not allowed")

  if (any(is.na(X$indiv))) stop2("NA's in individual names ('indiv' column) not allowed")

  if (any(is.na(X$start))) stop2("NA's in 'start' column not allowed")

  if (any(is.na(X$end))) stop2("NA's in 'end' column  not allowed")

  if (!is.null(tlim)) X <- X[X$start > tlim[1] & X$end < tlim[2], ]

  # if it argument is not "jpeg" or "tiff"
  if (!any(it == "jpeg", it == "tiff")) stop2(paste("Image type", it, "not allowed"))

  # stop if some events do not have 2 individuals
  qw <- as.data.frame((tapply(X$sing.event, list(X$sing.event, X$indiv), length)))
  qw <- tapply(X$indiv, X$sing.event, function(x) length(unique(x)))

  if (any(qw != 2)) stop2("Some singing events don't have 2 interacting individuals ('indiv' column)")

  # if xl is not vector or length!=1 stop
  if (is.null(xl)) {
    stop2("'xl' must be a numeric vector of length 1")
  } else {
    if (!is.vector(xl)) {
      stop2("'xl' must be a numeric vector of length 1")
    } else {
      if (!length(xl) == 1) stop2("'xl' must be a numeric vector of length 1")
    }
  }

  # if res is not vector or length==1 stop
  if (!is.vector(res)) {
    stop2("'res' must be a numeric vector of length 1")
  } else {
    if (!length(res) == 1) stop2("'res' must be a numeric vector of length 1")
  }

  X$sing.event <- as.character(X$sing.event)

  # to avoid "notes" when submitting to CRAN
  xmin <- xmax <- ymin <- ymax <- NULL

  # run loop
  invisible(ggs <- .pblapply(pbar = pb, X = unique(X$sing.event), message = "plotting coordination", total = 1, FUN = function(x) {
    y <- X[X$sing.event == x, ]
    y <- y[!is.na(y$start), ]
    y <- y[!is.na(y$end), ]
    y <- y[order(y$start), ]


    if (only.coor) {
      fst <- max(c(
        which(y$start == min(y$start[y$indiv == unique(y$indiv)[1]])),
        which(y$start == min(y$start[y$indiv == unique(y$indiv)[2]]))
      )) - 1

      lst <- min(c(
        which(y$start == max(y$start[y$indiv == unique(y$indiv)[1]])),
        which(y$start == max(y$start[y$indiv == unique(y$indiv)[2]]))
      )) + 1

      y <- y[fst:lst, ]
    }

    # data for indiv 1
    y1 <- y[y$indiv == unique(y$indiv)[1], ]

    # data for indiv 2
    y2 <- y[y$indiv == unique(y$indiv)[2], ]

    if (any(nrow(y1) == 0, nrow(y2) == 0)) {
      paste("in", x, "singing bouts do not overlap in time")
    } else {
      df1 <- data.frame(
        xmin = y1$start, ymin = rep(0.8, nrow(y1)), xmax = y1$end,
        ymax = rep(1.1, nrow(y1)), id = unique(y$indiv)[1], col = "#F9766E"
      )

      df2 <- data.frame(
        xmin = y2$start, ymin = rep(1.4, nrow(y2)), xmax = y2$end,
        ymax = rep(1.7, nrow(y2)), id = unique(y$indiv)[2], col = "#00BFC4"
      )

      df <- rbind(df1, df2)

      # determine which ones overlap
      if (ovlp) {
        btc <- max(c(min(y1$start), min(y2$start)))
        z <- y[c((which(y$start == btc) - 1):nrow(y)), ]

        et1 <- max(z$start[z$indiv == unique(z$indiv)[1]])
        et2 <- max(z$start[z$indiv == unique(z$indiv)[2]])
        etc <- min(c(et1, et2))
        z <- z[1:c((which(z$start == etc) + 1)), ]

        ov <- sapply(2:nrow(z), function(i) {
          if (z$start[i] > z$start[i - 1] & z$start[i] < z$end[i - 1]) {
            "ovlp"
          } else {
            "No ovlp"
          }
        })

        if (length(ov[ov == "ovlp"]) > 0) {
          z1 <- data.frame(z, ovl = c("No ovlp", ov))

          recdf <- NULL
          for (i in 1:nrow(z1))
          {
            if (z1$ovl[i] == "ovlp") {
              if (z1$indiv[z1$start == min(z1$start[i], z1$start[i - 1])] == unique(z$indiv)[1]) {
                col <- "#00BFC496"
                id <- "#!"
              } else {
                col <- "#F9766E96"
                id <- "^%"
              }
              if (is.null(recdf)) {
                recdf <- data.frame(
                  xmin = mean(z1$start[i], z1$end[i]), xmax = min(z1$end[i], z1$end[i - 1]),
                  ymin = 1.1, ymax = 1.4, id, col
                )
              } else {
                recdf <- rbind(recdf, data.frame(
                  xmin = mean(z1$start[i], z1$end[i]), xmax = min(z1$end[i], z1$end[i - 1]),
                  ymin = 1.1, ymax = 1.4, id, col
                ))
              }
            }
          }

          df <- rbind(df, recdf)
        }
      }

      cols <- c("#F9766E66", "#00BFC466")
      ids <- c(as.character(unique(y$indiv)[2]), as.character(unique(y$indiv)[1]))

      if (all(exists("recdf"), ovlp)) {
        if (nrow(recdf) > 0) {
          if (suppressWarnings(min(which(df$id == "#!"))) < suppressWarnings(min(which(df$id == "^%")))) {
            cols <- c("#00BFC466", "#F9766E66")
            ids <- c(as.character(unique(y$indiv)[2]), as.character(unique(y$indiv)[1]))
          } else {
            cols <- c("#F9766E66", "#00BFC466")
            ids <- c(as.character(unique(y$indiv)[1]), as.character(unique(y$indiv)[2]))
          }
        }
      }

      ggp <- ggplot2::ggplot(df, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, group = id, fill = col)) +
        ggplot2::geom_rect() +
        ggplot2::scale_x_continuous("Time (s)") +
        ggplot2::scale_y_continuous(name = NULL, breaks = c(0.95, 1.55), labels = c(unique(y$indiv)[1], unique(y$indiv)[2])) +
        ggplot2::scale_fill_manual(
          values = c("#F9766E", "#00BFC4", cols), name = "",
          labels = c(
            as.character(unique(y$indiv)[1]), as.character(unique(y$indiv)[2]), paste("overlap from", ids[1]),
            paste("overlap from", ids[2])
          )
        ) +
        ggplot2::theme(legend.position = "top") +
        ggplot2::ggtitle(x)

      if (img) {
        if (it == "jpeg") ite <- "coor.singing.jpeg" else ite <- "coor.singing.tiff"
        ggplot2::ggsave(
          plot = ggp, filename = paste(x, ite, sep = "-"),
          dpi = res, units = "in", width = 9 * xl, height = 2.5
        )
      }

      return(ggp)
    }
  }))
}
