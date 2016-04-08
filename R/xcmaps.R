#' Maps of Xeno-Canto recordings by species
#' 
#' \code{xcmaps} creates maps to visualize the geographic spread of Xeno-Canto
#'   recordings.
#' @usage xcmaps(X, img = TRUE, it = "jpeg", res = 100)   
#' @param X Data frame output from \code{\link{querxc}}.
#' @param img A logical argument specifying whether an image file of each species
#'   map should be returned, default is \code{TRUE}.
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @param res Numeric argument of length 1. Controls image resolution.
#'   Default is 100 (faster) although 300 - 400 is recommended for publication/ 
#'   presentation quality.
#' @return A map of Xeno-Canto recordings per species (image file), or a faceted
#'   plot of species map(s) in the active graphic device.
#' @export
#' @name xcmaps
#' @details This function creates maps for visualizing the geographic spread of recordings from the open-access
#' online repository Xeno-Canto (\url{http://www.xeno-canto.org/}). The function takes the output of 
#' \code{\link{querxc}} as input. Maps can be displayed in the graphic devide or saved as images in the
#' working directory.
#' @examples
#' \dontrun{
#' X <- querxc("Phaethornis anthophilus", download = FALSE)
#' View(X)
#' xcmaps(X)
#' xcmaps(X, img = FALSE, it = "jpeg")
#' }
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu}) and Grace Smith Vidaurre

xcmaps <- function(X, img = TRUE, it = "jpeg", res = 100) {

  #stop if X is not a data frame
  if(!is.data.frame(X))  stop("X is not a data frame")

  #if it argument is not "jpeg" or "tiff" 
  if(!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed"))  
  
    # Initialize species names (common name)
  spn <- length(unique(X$English_name))
    
  # Set threshold for maximum number of panels per plot device
  if(spn <= 16) mat <- par(mfrow = c(ceiling(sqrt(spn)), round(sqrt(spn),0))) else par(mfrow = c(4, 4)) 
  par(mar = rep(0, 4))
  
  # Create a map per species, with the recordings plotted over each map
  for(i in sort(unique(paste(X$Genus,X$Specific_epithet)))) {
    
    y <- X[paste(X$Genus,X$Specific_epithet) == i, ]
    lat <- as.numeric(as.character(y$Latitude))
    lon <- as.numeric(as.character(y$Longitude))
    lat <- lat[!is.na(lat)]
    lon <- lon[!is.na(lon)]
    
    if(all(length(lat) > 0, length(lon) > 0))
    {if (abs(max(lon) - min(lon)) < 38) buf <- 12 else buf <- 5
     
     if(img){
       prop <- abs((min(lon) - buf)-(max(lon) + buf))/abs((min(lat) - buf)-(max(lat) + buf)) * 1.15
       if(it == "tiff") tiff(filename = paste("Map of ", i, " recordings", ".tiff", sep = ""), 
                             width = 480 * prop) else
         jpeg(filename = paste("Map of ", i, " recordings", ".jpeg", sep = ""), width = 480 * prop)

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
       map.axes()
       
       #add contour lines
       maps::map("world", xlim = c(min(lon) - buf, max(lon) + buf), interior = FALSE,
                 ylim = c(min(lat) - buf, max(lat) + buf), fill = FALSE, add = TRUE)  
       
       # add points
       points(lon, lat, pch = 21, cex = 1.8, col = "black", bg= heat.colors(10)[2])
       
      
      # add scale
       map.scale(ratio = FALSE, relwidth = 0.4)   
       dev.off()
     
       } else {
         #change margins
         if(par()$mfrow[1] > 2) par(mfrow = c(2,2))
         if(par()$mfrow[1] > 1) u <- 0 else u  <- 2
         par(mar= rep(u, 4), mai = rep(0.2, 4))
         
         
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
         mtext(i, side =3, line = 1, cex = 1, font = 4)
         
         #add axes
         map.axes()
         
         #add contour lines
         maps::map("world", xlim = c(min(lon) - buf, max(lon) + buf), interior = FALSE,
                   ylim = c(min(lat) - buf, max(lat) + buf), fill = FALSE, add = TRUE)  
         
         # add points
         points(lon, lat, pch = 21, cex = 1.8, col = "black", bg= heat.colors(10)[2])
         
         # add scale
       map.scale(ratio = FALSE, relwidth = 0.4)
      }    
    }
  }
}
