#' Maps of 'Xeno-Canto' recordings by species
#'
#' \code{map_xc} creates maps to visualize the geographic spread of 'Xeno-Canto'
#'   recordings.
#' @usage map_xc(X, img = TRUE, it = "jpeg", res = 100, labels = FALSE,
#'  path = NULL, leaflet.map = FALSE,
#'  leaflet.cluster = FALSE)
#' @param X Data frame output from \code{\link{query_xc}}.
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
#' @param leaflet.map Logical to control whether the package 'leaflet' is used for displaying the maps. 'leaflet' maps are interactive and display information about recordings and links to the Xeno-Canto website. If \code{TRUE} a single map is displayed regardless of the number of species and all other image related arguments are ignored. Default is \code{FALSE}. The hovering label shows the species scientific name (or the subspecies if only 1 species is present in 'X'). Note that colors will be recycled if more after 18 species (or subspecies).
#' @param leaflet.cluster Logical to control if icons are clustered by locality (as in Xeno-Canto maps). Default is \code{FALSE}.
#' @return A map of 'Xeno-Canto' recordings per species (image file), or a faceted
#'   plot of species map(s) in the active graphic device.
#' @export
#' @name map_xc
#' @details This function creates maps for visualizing the geographic spread of recordings from the open-access
#' online repository \href{https://www.xeno-canto.org/}{Xeno-Canto}. The function takes the output of
#' \code{\link{query_xc}} as input. Maps can be displayed in the graphic device (or Viewer if 'leaflet.map = TRUE') or saved as images in the
#' working directory. Note that only recordings with geographic coordinates are displayed.
#' @examples
#' \dontrun{
#' # search in xeno-canto
#' X <- query_xc("Phaethornis anthophilus", download = FALSE)
#'
#' # create image in R graphic device
#' map_xc(X, img = FALSE)
#'
#' # create leaflet map
#' map_xc(X, leaflet.map = TRUE)
#' }
#'
#' @references {
#' Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to streamline analysis of animal acoustic signals. Methods in Ecology and Evolution, 8(2), 184-191.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}) and Grace Smith Vidaurre

map_xc <- function(X, img = TRUE, it = "jpeg", res = 100, labels = FALSE,
                   path = NULL, leaflet.map = FALSE,
                   leaflet.cluster = FALSE) {
  # error message if maps is not installed
  if (!requireNamespace("maps", quietly = TRUE)) {
    stop2("must install 'maps' to use this function")
  }

  # error message if leaflet is not installed
  if (!requireNamespace("leaflet", quietly = TRUE) & leaflet.map) {
    stop2("must install 'leaflet' to use leaflet style maps (when 'leaflet.map = TRUE')")
  }

  #### set arguments from options
  # get function arguments
  argms <- methods::formalArgs(map_xc)

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

  # check path if not provided set to working directory
  if (is.null(path)) path <- getwd() else if (!dir.exists(path)) stop2("'path' provided does not exist")

  # stop if X is not a data frame
  if (!is.data.frame(X)) stop2("X is not a data frame")

  # make species column
  X$species <- paste(X$Genus, X$Specific_epithet)

  # make lat lon numeric and remove rows with no coords
  X$Latitude <- as.numeric(as.character(X$Latitude))
  X$Longitude <- as.numeric(as.character(X$Longitude))
  X <- X[!is.na(X$Latitude) & !is.na(X$Longitude), , drop = FALSE]

  # stop if no rows left
  if (nrow(X) == 0) stop2("not  a single  with observation has coordinates")

  # if no leatfet map
  if (!leaflet.map) {
    # if it argument is not "jpeg" or "tiff"
    if (!any(it == "jpeg", it == "tiff")) stop2(paste("Image type", it, "not allowed"))

    # get species names (common name)
    spn <- length(unique(X$English_name))

    # reset graphic device
    try(dev.off(), silent = TRUE)

    # Set threshold for maximum number of panels per plot device
    if (spn <= 16) mat <- par(mfrow = c(ceiling(sqrt(spn)), round(sqrt(spn), 0))) else par(mfrow = c(4, 4))
    par(mar = rep(0, 4))

    # Create a map per species, with the recordings plotted over each map
    for (i in sort(unique(X$species))) {
      y <- X[X$species == i, ]

      if (all(length(y$Latitude) > 0, length(y$Longitude) > 0)) {
        if (abs(max(y$Longitude) - min(y$Longitude)) < 38) buf <- 12 else buf <- 5

        if (img) {
          prop <- abs((min(y$Longitude) - buf) - (max(y$Longitude) + buf)) / abs((min(y$Latitude) - buf) - (max(y$Latitude) + buf)) * 1.15

          img_wrlbr_int(
            filename = paste("Map of ", i, " recordings", it, sep = ""),
            width = 480 * prop, path = path
          )

          # change margins
          # par(mar = rep(2.5,4))


          # add empty  map
          maps::map("world",
            xlim = c(min(y$Longitude) - buf, max(y$Longitude) + buf), interior = FALSE,
            ylim = c(min(y$Latitude) - buf, max(y$Latitude) + buf), fill = FALSE
          )

          # change background color
          rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
            col =
              cm.colors(10)[3]
          )

          # plot lat lon lines in background
          abline(h = seq(-90, 90, 5), col = "white", lwd = 0.9)
          abline(h = seq(-90, 90, 10), col = "white", lwd = 1.1)
          abline(v = seq(-180, 180, 5), col = "white", lwd = 0.9)
          abline(v = seq(-180, 180, 10), col = "white", lwd = 1.1)

          # add map
          maps::map("world",
            xlim = c(min(y$Longitude) - buf, max(y$Longitude) + buf), add = TRUE,
            ylim = c(min(y$Latitude) - buf, max(y$Latitude) + buf), fill = TRUE,
            col = terrain.colors(10)[5], myborder = 0, interior = F, lty = 2
          )
          mtext("Longitude (DD)", side = 1, line = 2)
          mtext("Latitude (DD)", side = 2, line = 2)
          mtext(i, side = 3, line = 1, cex = 1.4, font = 4)

          # add axes
          maps::map.axes()

          # add contour lines
          maps::map("world",
            xlim = c(min(y$Longitude) - buf, max(y$Longitude) + buf), interior = FALSE,
            ylim = c(min(y$Latitude) - buf, max(y$Latitude) + buf), fill = FALSE, add = TRUE
          )

          # add points
          points(y$Longitude, y$Latitude, pch = 21, cex = 1.8, col = "#E37222", bg = "#E37222")

          # add labels
          if (labels) {
            text(y$Longitude, y$Latitude, labels = X$Recording_ID, cex = 0.7, pos = 4)
          }

          # add scale
          maps::map.scale(ratio = FALSE, relwidth = 0.4)
          dev.off()
        } else {
          # change margins
          if (par()$mfrow[1] > 2) par(mfrow = c(2, 2))
          if (par()$mfrow[1] > 1) u <- 0 else u <- 2
          par(mar = rep(u, 4), mai = rep(0.2, 4))


          # add empty  map
          maps::map("world",
            xlim = c(min(y$Longitude) - buf, max(y$Longitude) + buf), interior = FALSE,
            ylim = c(min(y$Latitude) - buf, max(y$Latitude) + buf), fill = FALSE
          )

          # change background color
          rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
            col =
              adjustcolor("#07889B", alpha.f = 0.2)
          )

          # plot lat lon lines in background
          abline(h = seq(-90, 90, 5), col = "white", lwd = 0.9)
          abline(h = seq(-90, 90, 10), col = "white", lwd = 1.1)
          abline(v = seq(-180, 180, 5), col = "white", lwd = 0.9)
          abline(v = seq(-180, 180, 10), col = "white", lwd = 1.1)

          # add map
          maps::map("world",
            xlim = c(min(y$Longitude) - buf, max(y$Longitude) + buf), add = TRUE,
            ylim = c(min(y$Latitude) - buf, max(y$Latitude) + buf), fill = TRUE, col = "white"
          )
          maps::map("world",
            xlim = c(min(y$Longitude) - buf, max(y$Longitude) + buf), add = TRUE,
            ylim = c(min(y$Latitude) - buf, max(y$Latitude) + buf), fill = TRUE,
            col = adjustcolor("darkolivegreen2", alpha.f = 0.7), myborder = 0, interior = F, lty = 2
          )
          mtext("Longitude (DD)", side = 1, line = 2)
          mtext("Latitude (DD)", side = 2, line = 2)
          mtext(i, side = 3, line = 1, cex = 1, font = 4)

          # add axes
          maps::map.axes()

          # add contour lines
          maps::map("world",
            xlim = c(min(y$Longitude) - buf, max(y$Longitude) + buf), interior = FALSE,
            ylim = c(min(y$Latitude) - buf, max(y$Latitude) + buf), fill = FALSE, add = TRUE
          )

          # add points
          points(y$Longitude, y$Latitude, pch = 21, cex = 1.3, bg = "white")
          points(y$Longitude, y$Latitude, pch = 21, cex = 1.3, col = "gray3", bg = adjustcolor("#E37222", alpha.f = 0.7), lwd = 0.7)

          # add labels
          if (labels) {
            text(y$Longitude, y$Latitude, labels = X$Recording_ID, cex = 0.7, pos = 4)
          }

          # add scale
          maps::map.scale(ratio = FALSE, relwidth = 0.4)
        }
      }
    }
  } else { # if leaflet map

    cols <- c("red", "darkred", "lightred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "darkpurple", "pink", "cadetblue", "gray", "lightgray", "black")[c(c(1, 6, 12) + rep(1:6, each = 3), 1)]

    # repeat many times so colors are "recycled"
    cols <- rep(cols, 100)

    # change NAs in subspecies
    X$Subspecies <- as.character(X$Subspecies)
    X$Subspecies[is.na(X$Subspecies) | X$Subspecies == ""] <- "not provided"

    # if only one species use subspecies for color marker
    if (length(unique((X$species))) == 1) {
      # label pop up markers
      X$labels <- X$Subspecies
      X$labels[X$labels == "not provided"] <- "Subsp. not provided"
    } else {
      # labels for hovering
      X$labels <- X$species
    }

    # color for marker
    mrkcol <- cols[1:(length(unique(X$labels)))][as.numeric(as.factor(X$labels))]
    mrkcol[X$labels == "Subsp. not provided"] <- "white"

    # use ios icons with marker colors
    icons <- leaflet::awesomeIcons(
      icon = "ios-close",
      iconColor = "black",
      library = "ion",
      markerColor = mrkcol
    )

    # make content for popup
    content <- paste0("<b><a href='https://www.xeno-canto.org/", X$Recording_ID, "'>", paste0("XC", X$Recording_ID), "</a></b>", "<br/><i>", paste(X$Genus, X$Specific_epithet, sep = " "), "</i><br/> Subspecies: ", X$Subspecies, "<br/> Country: ", X$Country, "<br/> Locality: ", X$Locality, "<br/> Voc.type: ", X$Vocalization_type, "<br/> Recordist: ", X$Recordist, paste0("<b><a href='https://www.xeno-canto.org/", X$Recording_ID, "/download'>", "<br/>", "listen</a>"))


    # make base map
    leaf.map <- leaflet::leaflet(X)

    # add tiles
    leaf.map <- leaflet::addTiles(leaf.map)

    # add markers
    if (leaflet.cluster) {
      leaf.map <- leaflet::addAwesomeMarkers(
        map = leaf.map, ~Longitude, ~Latitude, icon = icons, label = ~labels, popup = content, data = X, clusterOptions = leaflet::markerClusterOptions(),
        clusterId = "rec.cluster"
      )
    } else {
      leaf.map <- leaflet::addAwesomeMarkers(map = leaf.map, ~Longitude, ~Latitude, icon = icons, label = ~labels, popup = content, data = X)
    }


    # add minimap view at bottom right
    leaf.map <- leaflet::addMiniMap(leaf.map)

    # add zoom-out button
    leaf.map <- leaflet::addEasyButton(leaf.map, leaflet::easyButton(
      icon = "fa-globe", title = "Zoom to full view",
      onClick = leaflet::JS("function(btn, map){ map.setZoom(1); }")
    ))

    if (leaflet.cluster) {
      leaf.map <- leaflet::addEasyButton(leaf.map, leaflet::easyButton(
        states = list(
          leaflet::easyButtonState(
            stateName = "unfrozen-markers",
            icon = "ion-toggle",
            title = "Freeze Clusters",
            onClick = leaflet::JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'rec.cluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }")
          ),
          leaflet::easyButtonState(
            stateName = "frozen-markers",
            icon = "ion-toggle-filled",
            title = "UnFreeze Clusters",
            onClick = leaflet::JS("
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'rec.cluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }")
          )
        )
      ))
    }

    # plot map
    leaf.map
  }
}

##############################################################################################################
#' alternative name for \code{\link{map_xc}}
#'
#' @keywords internal
#' @details see \code{\link{map_xc}} for documentation. \code{\link{xcmaps}} will be deprecated in future versions.
#' @export

xcmaps <- map_xc
