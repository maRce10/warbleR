# internal warbleR function, not to be called by users. It is a modified version of seewave::filled.contour.modif2
# that allows to plot spectrograms on top of each other. 
filled_contour_color_wrblr_int <- function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
          z, xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE), col.lab, colaxis,
          zlim = range(z, finite = TRUE), levels = pretty(zlim, nlevels), add = FALSE,
          nlevels = 20, color.palette = cm.colors, col = color.palette(length(levels) - 
                                                                         1), plot.title, plot.axes, key.title, asp = NA, xaxs = "i", 
          yaxs = "i", las = 1, axisX = TRUE, axisY = TRUE) 
{
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      } else {
        z <- x
        x <- seq(0, 1, len = nrow(z))
      }
    } else stop("no 'z' matrix specified")
  } else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
  if (!add) {plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
    stop("no proper 'z' matrix specified")}
  if (!is.double(z)) 
    storage.mode(z) <- "double"
  .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                  col = col)
  if (missing(plot.axes)) {
    if (axisX) {
      title(main = "", xlab = "", ylab = "")
      axis(1)
    }
    if (axisY) {
      title(main = "", xlab = "", ylab = "")
      axis(2)
    }
  }
  else plot.axes
  box()
  if (missing(plot.title)) 
    title()
  else plot.title
  invisible()
}
