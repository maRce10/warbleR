#' Maps of 'Xeno-Canto' recordings by species
#' 
#' \code{xcmaps} creates maps to visualize the geographic spread of 'Xeno-Canto'
#'   recordings.
#' @usage xcmaps(X, img = TRUE, it = "jpeg", res = 100, labels = FALSE,
#'  path = NULL)   
#' @param X Data frame output from \code{\link{querxc}}.
#' @param img A logical argument specifying whether an image file of each species
#'   map should be returned, default is \code{TRUE}.
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param res Numeric argument of length 1. Controls image resolution.
#'   Default is 100 (faster) although 300 - 400 is recommended for publication/ 
#'   presentation quality.
#' @param labels A logical argument defining whether dots depicting recording locations are labeled.
#' If \code{TRUE} then the Recording_ID is used as label.
#' @param path Character string with the directory path where the image files will be saved. 
#' If \code{NULL} (default) then the current working directory is used.
#' Ignored if \code{img = FALSE}. 
#' @return A map of 'Xeno-Canto' recordings per species (image file), or a faceted
#'   plot of species map(s) in the active graphic device.
#' @export
#' @name xcmaps
#' @details This function creates maps for visualizing the geographic spread of recordings from the open-access
#' online repository  \href{http://www.xeno-canto.org/}{Xeno-Canto}. The function takes the output of 
#' \code{\link{querxc}} as input. Maps can be displayed in the graphic device or saved as images in the
#' working directory.
#' @examples
#' \dontrun{
#' # search in xeno-canto
#' X <- querxc("Phaethornis anthophilus", download = FALSE)
#' 
#' #create image in R graphic device
#' xcmaps(X, img = FALSE)
#' }
#' 
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com}) and Grace Smith Vidaurre

xcmaps <- function(X, img = TRUE, it = "jpeg", res = 100, labels = FALSE,
                   path = NULL) {

  # error message if wavethresh is not installed
  if (!requireNamespace("maps",quietly = TRUE))
    stop("must install 'maps' to use this function")
  
  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(xcmaps)
  
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
  
  #check path if not provided set to working directory
  if (is.null(path)) path <- getwd() else 
    if (!dir.exists(path)) stop("'path' provided does not exist") 
  
  #stop if X is not a data frame
  if (!is.data.frame(X))  stop("X is not a data frame")

  #if it argument is not "jpeg" or "tiff" 
  if (!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed")) 
  
  # Initialize species names (common name)
  spn <- length(unique(X$English_name))
    
  # reset graphic device
  try(dev.off(), silent = TRUE)
  
  # Set threshold for maximum number of panels per plot device
  if (spn <= 16) mat <- par(mfrow = c(ceiling(sqrt(spn)), round(sqrt(spn),0))) else par(mfrow = c(4, 4)) 
  par(mar = rep(0, 4))
  
  # Create a map per species, with the recordings plotted over each map
  for(i in sort(unique(paste(X$Genus,X$Specific_epithet)))) {
    
    y <- X[paste(X$Genus,X$Specific_epithet) == i, ]
    lat <- as.numeric(as.character(y$Latitude))
    lon <- as.numeric(as.character(y$Longitude))
    lat <- lat[!is.na(lat)]
    lon <- lon[!is.na(lon)]
    
    if (all(length(lat) > 0, length(lon) > 0))
    {if (abs(max(lon) - min(lon)) < 38) buf <- 12 else buf <- 5
     
     if (img){
       prop <- abs((min(lon) - buf)-(max(lon) + buf))/abs((min(lat) - buf)-(max(lat) + buf)) * 1.15
       
       img_wrlbr_int(filename = paste("Map of ", i, " recordings", it, sep = ""), 
                             width = 480 * prop, path = path) 

       #change margins
       par(mar= rep(2,4))
       
       
       #add empty  map
       maps::map("world", xlim = c(min(lon) - buf, max(lon) + buf), interior = FALSE,
                 ylim = c(min(lat) - buf, max(lat) + buf), fill = FALSE)  
       
       #change background color
       rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
              cm.colors(10)[3])
       
       #plot lat lon lines in background
       abline(h = seq(-90, 90, 5), col = "white", lwd = 0.9)
       abline(h = seq(-90, 90, 10), col = "white", lwd = 1.1)
       abline(v = seq(-180, 180, 5), col = "white", lwd = 0.9)
       abline(v = seq(-180, 180, 10), col = "white", lwd = 1.1)
       
       #add map
       maps::map("world", xlim = c(min(lon) - buf, max(lon) + buf), add = TRUE,
                 ylim = c(min(lat) - buf, max(lat) + buf), fill = TRUE, 
                 col = terrain.colors(10)[5], myborder = 0, interior =  F, lty = 2)
       mtext("Longitude (DD)", side = 1, line = 2)
       mtext("Latitude (DD)", side = 2, line = 2)
       mtext(i, side =3, line = 1, cex = 1.4, font = 4)
       
       #add axes
       maps::map.axes()
       
       #add contour lines
       maps::map("world", xlim = c(min(lon) - buf, max(lon) + buf), interior = FALSE,
                 ylim = c(min(lat) - buf, max(lat) + buf), fill = FALSE, add = TRUE)  
       
       # add points
       points(lon, lat, pch = 21, cex = 1.8, col = "#E37222", bg= "#E37222")
       
       #add labels 
       if (labels)
        text(lon, lat, labels = X$Recording_ID, cex = 0.7, pos = 4)
      
      # add scale
       maps::map.scale(ratio = FALSE, relwidth = 0.4)   
       dev.off()
     
       } else {
         #change margins
         if (par()$mfrow[1] > 2) par(mfrow = c(2,2))
         if (par()$mfrow[1] > 1) u <- 0 else u  <- 2
         par(mar= rep(u, 4), mai = rep(0.2, 4))
         
         
         #add empty  map
         maps::map("world", xlim = c(min(lon) - buf, max(lon) + buf), interior = FALSE,
                   ylim = c(min(lat) - buf, max(lat) + buf), fill = FALSE)  
         
         #change background color
         rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
               adjustcolor("#07889B", alpha.f = 0.2))
         
         #plot lat lon lines in background
         abline(h = seq(-90, 90, 5), col = "white", lwd = 0.9)
         abline(h = seq(-90, 90, 10), col = "white", lwd = 1.1)
         abline(v = seq(-180, 180, 5), col = "white", lwd = 0.9)
         abline(v = seq(-180, 180, 10), col = "white", lwd = 1.1)
         
         #add map
         maps::map("world", xlim = c(min(lon) - buf, max(lon) + buf), add = TRUE,
                   ylim = c(min(lat) - buf, max(lat) + buf), fill = TRUE, col = "white")
          maps::map("world", xlim = c(min(lon) - buf, max(lon) + buf), add = TRUE,
                   ylim = c(min(lat) - buf, max(lat) + buf), fill = TRUE, 
                   col = adjustcolor("darkolivegreen2", alpha.f = 0.7), myborder = 0, interior =  F, lty = 2)
         mtext("Longitude (DD)", side = 1, line = 2)
         mtext("Latitude (DD)", side = 2, line = 2)
         mtext(i, side =3, line = 1, cex = 1, font = 4)
         
         #add axes
         maps::map.axes()
         
         #add contour lines
         maps::map("world", xlim = c(min(lon) - buf, max(lon) + buf), interior = FALSE,
                   ylim = c(min(lat) - buf, max(lat) + buf), fill = FALSE, add = TRUE)  
         
         # add points
         points(lon, lat, pch = 21, cex = 1.3, bg= "white")
         points(lon, lat, pch = 21, cex = 1.3, col = "gray3", bg= adjustcolor("#E37222", alpha.f = 0.7), lwd = 0.7)
        
        #add labels 
         if (labels)
        text(lon, lat, labels = X$Recording_ID, cex = 0.7, pos = 4)
         
         # add scale
         maps::map.scale(ratio = FALSE, relwidth = 0.4)
      }    
    }
  }
}


##############################################################################################################
#' alternative name for \code{\link{xcmaps}}
#'
#' @keywords internal
#' @details see \code{\link{xcmaps}} for documentation. \code{\link{xcmaps}} will be deprecated in future versions.
#' @export

xc_maps <- xcmaps
