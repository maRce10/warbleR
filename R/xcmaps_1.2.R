#' Maps of Xeno Canto recordings by species
#' 
#' \code{xcmaps} creates maps to visualize the geographic spread of Xeno Canto
#'   recordings.
#' @usage xcmaps(X, tiff = TRUE)   
#' @param X Data frame output from querxc().
#' @param tiff A logical argument specifying whether a tiff file of each species
#'   map should be returned, default is TRUE.
#' @return A map of Xeno Canto recordings per species (tiff file), or a faceted
#'   R plot of species map(s).
#' @export
#' @name xcmaps
#' @examples
#' \dontrun{
#' X <- querxc("Phaethornis anthophilus", download = FALSE)
#' View(X)
#' xcmaps(X)
#' xcmaps(X, tiff = FALSE)
#' }
#' @author Marcelo Araya-Salas (http://marceloarayasalas.weebly.com) and Grace Smith Vidaurre

#load maps package
require(maps)

xcmaps <- function(X, tiff = TRUE) {
  
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
     
     if(tiff == TRUE){
       tiff(filename = paste("Map of ", i, " recordings", ".tiff", sep = ""))
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

  


