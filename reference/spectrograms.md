# Spectrograms of selected signals

`spectrograms` creates spectrograms of signals from selection tables.

## Usage

``` r
spectrograms(
  X,
  wl = 512,
  flim = "frange",
  wn = "hanning",
  pal = reverse.gray.colors.2,
  ovlp = 70,
  inner.mar = c(5, 4, 4, 2),
  outer.mar = c(0, 0, 0, 0),
  picsize = 1,
  res = 100,
  cexlab = 1,
  propwidth = FALSE,
  xl = 1,
  osci = FALSE,
  gr = FALSE,
  sc = FALSE,
  line = TRUE,
  col = "#07889B",
  fill = adjustcolor("#07889B", alpha.f = 0.15),
  lty = 3,
  mar = 0.05,
  it = "jpeg",
  parallel = 1,
  path = NULL,
  pb = TRUE,
  fast.spec = FALSE,
  by.song = NULL,
  sel.labels = "selec",
  title.labels = NULL,
  dest.path = NULL,
  box = TRUE,
  axis = TRUE,
  ...
)
```

## Arguments

- X:

  'selection_table', 'extended_selection_table' or data frame containing
  columns for sound file name (sound.files), selection number (selec),
  and start and end time of signals (start and end). 'top.freq' and
  'bottom.freq' columns are optional. If using an
  'extended_selection_table' the sound files are not required (see
  [`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md)).

- wl:

  A numeric vector of length 1 specifying the window length of the
  spectrogram, default is 512.

- flim:

  A numeric vector of length 2 for the frequency limit (in kHz) of the
  spectrogram, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). The
  function also accepts 'frange' (default) which produces spectrograms
  with a frequency limit around the range of each signal (adding a 1 kHz
  margin).

- wn:

  Character vector of length 1 specifying window name. Default is
  "hanning". See function
  [`ftwindow`](https://rdrr.io/pkg/seewave/man/ftwindow.html) for more
  options.

- pal:

  A color palette function to be used to assign colors in the plot, as
  in [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default
  is reverse.gray.colors.2.

- ovlp:

  Numeric vector of length 1 specifying the percent overlap between two
  consecutive windows, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  70.

- inner.mar:

  Numeric vector with 4 elements, default is c(5,4,4,2). Specifies
  number of lines in inner plot margins where axis labels fall, with
  form c(bottom, left, top, right). See
  [`par`](https://rdrr.io/r/graphics/par.html).

- outer.mar:

  Numeric vector with 4 elements, default is c(0,0,0,0). Specifies
  number of lines in outer plot margins beyond axis labels, with form
  c(bottom, left, top, right). See
  [`par`](https://rdrr.io/r/graphics/par.html).

- picsize:

  Numeric argument of length 1. Controls relative size of spectrogram.
  Default is 1. Ignored when propwidth is `TRUE`.

- res:

  Numeric argument of length 1. Controls image resolution. Default is
  100 (faster) although 300 - 400 is recommended for publication/
  presentation quality.

- cexlab:

  Numeric vector of length 1 specifying the relative size of axis
  labels. See [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html).

- propwidth:

  Logical argument to scale the width of spectrogram proportionally to
  duration of the selection. Default is `FALSE`.

- xl:

  Numeric vector of length 1. A constant by which to scale spectrogram
  width if propwidth = `TRUE`. Default is 1.

- osci:

  Logical argument to add an oscillogram underneath spectrogram, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  `FALSE`.

- gr:

  Logical argument to add grid to spectrogram. Default is `FALSE`.

- sc:

  Logical argument to add amplitude scale to spectrogram, default is
  `FALSE`.

- line:

  Logical argument to add lines at start and end times of selection (or
  box if bottom.freq and top.freq columns are provided). Default is
  `TRUE`.

- col:

  Color of 'line'. Default is "#07889B".

- fill:

  Fill color of box around selections. Default is
  `adjustcolor("#07889B", alpha.f = 0.15)`.

- lty:

  Type of 'line' as in [`par`](https://rdrr.io/r/graphics/par.html).
  Default is 1.

- mar:

  Numeric vector of length 1. Specifies the margins adjacent to the
  start and end points of selections, dealineating spectrogram limits.
  Default is 0.05.

- it:

  A character vector of length 1 giving the image type to be used.
  Currently only "tiff" and "jpeg" are admitted. Default is "jpeg".

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used.

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- fast.spec:

  Logical. If `TRUE` then image function is used internally to create
  spectrograms, which substantially increases performance (much faster),
  although some options become unavailable, as collevels, and sc
  (amplitude scale). This option is indicated for signals with high
  background noise levels. Palette colors
  [`gray.1`](https://rdrr.io/pkg/monitoR/man/specCols.html),
  [`gray.2`](https://rdrr.io/pkg/monitoR/man/specCols.html),
  [`gray.3`](https://rdrr.io/pkg/monitoR/man/specCols.html),
  [`topo.1`](https://rdrr.io/pkg/monitoR/man/specCols.html) and
  [`rainbow.1`](https://rdrr.io/pkg/monitoR/man/specCols.html) (which
  should be imported from the package monitoR) seem to work better with
  'fast' spectrograms. Palette colors
  [`gray.1`](https://rdrr.io/pkg/monitoR/man/specCols.html),
  [`gray.2`](https://rdrr.io/pkg/monitoR/man/specCols.html),
  [`gray.3`](https://rdrr.io/pkg/monitoR/man/specCols.html) offer
  decreasing darkness levels.

- by.song:

  Character string with the column name containing song labels. If
  provide a single spectrogram containing all elements for each song
  will be produce. Note that the function assumes that each song has a
  unique label within a sound file. If `NULL` (default), spectrograms
  are produced for single selections.

- sel.labels:

  Character string with the name of the column(s) for selection
  labeling. Default is 'selec'. Set to `NULL` to remove labels.

- title.labels:

  Character string with the name(s) of the column(s) to use as title.
  Default is `NULL` (no title). Only sound file and song included if
  'by.song' is provided.

- dest.path:

  Character string containing the directory path where the image files
  will be saved. If `NULL` (default) then the folder containing the
  sound files will be used instead.

- box:

  Logical to control if the box around the spectrogram is plotted (see
  [`box`](https://rdrr.io/r/graphics/box.html)). Default is `TRUE`.

- axis:

  Logical to control if the Y and X axis are of the spectrogram are
  plotted (see [`box`](https://rdrr.io/r/graphics/box.html)). Default is
  `TRUE`.

- ...:

  Additional arguments to be passed to the internal spectrogram creating
  function for customizing graphical output. The function is a modified
  version of [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html),
  so it takes the same arguments.

## Value

Image files containing spectrograms of the signals listed in the input
data frame.

## Details

This function provides access to batch process of (a modified version
of) the [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html)
function from the 'seewave' package. The function creates spectrograms
for visualization of vocalizations. Setting inner.mar to c(4,4.5,2,1)
and outer.mar to c(4,2,2,1) works well when picsize = 2 or 3. Title font
size, inner.mar and outer.mar (from mar and oma) don't work well when
osci or sc = TRUE, this may take some optimization by the user. Setting
'fast' argument to TRUE significantly increases speed, although some
options become unavailable, as collevels, and sc (amplitude scale). This
option is indicated for signals with high background noise levels.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`track_freq_contour`](https://marce10.github.io/warbleR/reference/track_freq_contour.md)
for creating spectrograms to visualize frequency measurements by
[`spectro_analysis`](https://marce10.github.io/warbleR/reference/spectro_analysis.md),
[`snr_spectrograms`](https://marce10.github.io/warbleR/reference/snr_spectrograms.md)
for creating spectrograms to optimize noise margins used in
[`sig2noise`](https://marce10.github.io/warbleR/reference/sig2noise.md)

Other spectrogram creators:
[`color_spectro()`](https://marce10.github.io/warbleR/reference/color_spectro.md),
[`freq_DTW()`](https://marce10.github.io/warbleR/reference/freq_DTW.md),
[`multi_DTW()`](https://marce10.github.io/warbleR/reference/multi_DTW.md),
[`phylo_spectro()`](https://marce10.github.io/warbleR/reference/phylo_spectro.md),
[`snr_spectrograms()`](https://marce10.github.io/warbleR/reference/snr_spectrograms.md),
[`track_freq_contour()`](https://marce10.github.io/warbleR/reference/track_freq_contour.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>) and Grace Smith Vidaurre

## Examples

``` r
{
  # load and save data
  data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

  # make spectrograms
  spectrograms(
    X = lbh_selec_table, flim = c(0, 11), res = 300, mar = 0.05,
    wl = 300, path = tempdir()
  )

  # check this folder
  tempdir()
}
#> [1] "/tmp/RtmphhikVM"
```
