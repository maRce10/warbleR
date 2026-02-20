# Maps of 'Xeno-Canto' recordings by species

`map_xc` creates maps to visualize the geographic spread of 'Xeno-Canto'
recordings. DEPRECATED.

## Usage

``` r
map_xc(
  X,
  img = TRUE,
  it = "jpeg",
  res = 100,
  labels = FALSE,
  path = NULL,
  leaflet.map = FALSE,
  leaflet.cluster = FALSE
)
```

## Arguments

- X:

  Data frame output from
  [`query_xc`](https://marce10.github.io/warbleR/reference/query_xc.md).

- img:

  A logical argument specifying whether an image file of each species
  map should be returned, default is `TRUE`.

- it:

  A character vector of length 1 giving the image type to be used.
  Currently only "tiff" and "jpeg" are admitted. Default is "jpeg".

- res:

  Numeric argument of length 1. Controls image resolution. Default is
  100 (faster) although 300 - 400 is recommended for publication/
  presentation quality.

- labels:

  A logical argument defining whether dots depicting recording locations
  are labeled. If `TRUE` then the Recording_ID is used as label.

- path:

  Character string with the directory path where the image files will be
  saved. If `NULL` (default) then the current working directory is used.
  Ignored if `img = FALSE`.

- leaflet.map:

  Logical to control whether the package 'leaflet' is used for
  displaying the maps. 'leaflet' maps are interactive and display
  information about recordings and links to the Xeno-Canto website. If
  `TRUE` a single map is displayed regardless of the number of species
  and all other image related arguments are ignored. Default is `FALSE`.
  The hovering label shows the species scientific name (or the
  subspecies if only 1 species is present in 'X'). Note that colors will
  be recycled if more after 18 species (or subspecies).

- leaflet.cluster:

  Logical to control if icons are clustered by locality (as in
  Xeno-Canto maps). Default is `FALSE`.

## Value

A map of 'Xeno-Canto' recordings per species (image file), or a faceted
plot of species map(s) in the active graphic device.

## Details

DEPRECATED. This function creates maps for visualizing the geographic
spread of recordings from the open-access online repository
[Xeno-Canto](https://www.xeno-canto.org/). The function takes the output
of [`query_xc`](https://marce10.github.io/warbleR/reference/query_xc.md)
as input. Maps can be displayed in the graphic device (or Viewer if
'leaflet.map = TRUE') or saved as images in the working directory. Note
that only recordings with geographic coordinates are displayed.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>) and Grace Smith Vidaurre
