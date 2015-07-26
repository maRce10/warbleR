#' Maps of Xeno-Canto recordings by species
#' 
#' \code{xcmaps} creates maps to visualize the geographic spread of Xeno-Canto
#'   recordings.
#' @usage xcmaps(X, img = TRUE, it = "jpeg")   
#' @param X Data frame output from \code{\link{querxc}}.
#' @param img A logical argument specifying whether an image file of each species
#'   map should be returned, default is \code{TRUE}.
#' @param it A character vector of length 1 giving the image type to be used. Currently only
#' "tiff" and "jpeg" are admitted. Default is "jpeg".
#' @return A map of Xeno-Canto recordings per species (image file), or a faceted
#'   plot of species map(s) in RStudio.
#' @export
#' @name xcmaps
#' @examples
#' \dontrun{
#' X <- querxc("Phaethornis anthophilus", download = FALSE)
#' View(X)
#' xcmaps(X)
#' xcmaps(X, img = FALSE, it = "jpeg")
#' }
#' @author Marcelo Araya-Salas (\url{http://marceloarayasalas.weebly.com/}) and Grace Smith Vidaurre

xcmaps <- function(X, img = TRUE, it = "jpeg") {
  
  # Initialize species names (common name)
  spn <- length(unique(X$English_name))
    
  # Set threshold for maximum number of panels per plot device
  if(spn <= 16) mat <- par(mfrow = c(ceiling(sqrt(spn)), round(sqrt(spn),0))) else par(mfrow = c(4, 4)) 
  par(mar = rep(0, 4))
  
  #if it argument is not "jpeg" or "tiff" 
  if(!any(it == "jpeg", it == "tiff")) stop(paste("Image type", it, "not allowed"))  
  
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
       if(it == "tiff") tiff(filename = paste("Map of ", i, " recordings", ".tiff", sep = "")) else
         jpeg(filename = paste("Map of ", i, " recordings", ".jpeg", sep = ""))
       maps::map("world", xlim = c(min(lon) - buf, max(lon) + buf), 
                 ylim = c(min(lat) - buf, max(lat) + buf), fill = TRUE, col = "skyblue", myborder = 0)
       title(paste(y$Genus[1], y$Specific_epithet[1], "n=", nrow(y)))
       points(lon, lat, pch = 20, cex = 3, col = "red")
       dev.off()
     
       } else {
       map("world", xlim = c(min(lon) - buf, max(lon) + buf), 
           ylim = c(min(lat) - buf, max(lat) + buf), fill = TRUE, col = "skyblue", myborder = 0)
       
       title(paste(y$Genus[1], y$Specific_epithet[1], "n=", nrow(y)))
       points(lon, lat, pch = 20, cex = 3, col = "red")
       
      }    
    }
  }
}

  


