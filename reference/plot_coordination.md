# Coordinated singing graphs

`plot_coordination` creates graphs of coordinated singing and highlights
the signals that overlap in time. The signals are represented by
polygons of different colors.

## Usage

``` r
plot_coordination(
  X = NULL,
  only.coor = FALSE,
  ovlp = TRUE,
  xl = 1,
  res = 80,
  it = "jpeg",
  img = TRUE,
  tlim = NULL,
  pb = TRUE
)
```

## Arguments

- X:

  Data frame containing columns for singing event (sing.event),
  individual (indiv), and start and end time of signal (start and end).

- only.coor:

  Logical. If `TRUE` only the segment in which both individuals are
  singing is included (solo singing is removed). Default is `FALSE`.

- ovlp:

  Logical. If `TRUE` the vocalizations that overlap in time are
  highlighted. Default is `TRUE`.

- xl:

  Numeric vector of length 1, a constant by which to scale image width.
  Default is 1.

- res:

  Numeric argument of length 1. Controls image resolution. Default is
  80.

- it:

  A character vector of length 1 giving the image type to be used.
  Currently only "tiff" and "jpeg" are admitted. Default is "jpeg".

- img:

  Logical argument. If `FALSE`, image files are not produced and the
  graphs are shown in the current graphic device. Default is `TRUE`.

- tlim:

  Numeric vector of length 2 indicating the start and end time of the
  coordinated singing events to be displayed in the graphs.

- pb:

  Logical argument to control progress bar and messages. Default is
  `TRUE`.

## Value

The function returns a list of graphs, one for each singing event in the
input data frame. The graphs can be plotted by simply calling the list.
If 'img' is `TRUE` then the graphs are also saved in the working
directory as files.

## Details

This function provides visualization for coordination of acoustic
signals. Signals are shown as polygon across a time axis. It also shows
which signals overlap, the amount of overlap, and highlights the
individual responsible for the overlap using a color code. The width of
the polygons depicting the time of overlap.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
# load simulate singing events (see data documentation)
data(sim_coor_sing)

#' # make plot_coordination in graphic device format
cgs <- plot_coordination(X = sim_coor_sing, ovlp = TRUE, only.coor = FALSE, img = FALSE)

cgs
} # }
```
