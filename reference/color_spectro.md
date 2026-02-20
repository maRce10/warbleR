# Highlight spectrogram regions

`color_spectro` highlights spectrogram regions specified by users

## Usage

``` r
color_spectro(
  wave,
  wl = 512,
  wn = "hanning",
  ovlp = 70,
  dB = "max0",
  collevels = NULL,
  selec.col = "red2",
  col.clm = NULL,
  base.col = "black",
  bg.col = "white",
  strength = 1,
  cexlab = 1,
  cexaxis = 1,
  tlab = "Time (s)",
  flab = "Frequency (kHz)",
  title = NULL,
  axisX = TRUE,
  axisY = TRUE,
  flim = NULL,
  rm.zero = FALSE,
  X = NULL,
  fast.spec = FALSE,
  t.mar = NULL,
  f.mar = NULL,
  interactive = NULL,
  add = FALSE
)
```

## Arguments

- wave:

  A 'wave' object produced by
  [`readWave`](https://rdrr.io/pkg/tuneR/man/readWave.html) or similar
  functions.

- wl:

  A numeric vector of length 1 specifying the window length of the
  spectrogram. Default is 512.

- wn:

  Character vector of length 1 specifying window name. Default is
  "hanning". See function
  [`ftwindow`](https://rdrr.io/pkg/seewave/man/ftwindow.html) for more
  options.

- ovlp:

  Numeric vector of length 1 specifying the percent overlap between two
  consecutive windows, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  70.

- dB:

  Character vector of length 1 controlling the amplitude weights as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  'max0'.

- collevels:

  Numeric. Levels used to partition amplitude range as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  `NULL`.

- selec.col:

  Character vector of length 1 specifying the color to be used to
  highlight selection. See 'col.clm' for specifying unique colors for
  each selection. Default is 'red2'. Ignored if 'col.cm' and 'X' are
  provided.

- col.clm:

  Character vector of length 1 indicating the name of the column in 'X'
  that contains the color names for each selection. Ignored if
  `X == NULL` or `interactive != NULL`. Default is `NULL`.

- base.col:

  Character vector of length 1 specifying the color of the background
  spectrogram. Default is 'black'.

- bg.col:

  Character vector of length 1 specifying the background color for both
  base and highlighted spectrograms. Default is 'white'.

- strength:

  Numeric vector of length 1 controlling the strength of the
  highlighting color (actually how many times it is repeated in the
  internal color palette). Must be a positive integer. Default is 1.

- cexlab:

  Numeric vector of length 1 specifying the relative size of axis
  labels. See [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html).
  Default is 1.

- cexaxis:

  Numeric vector of length 1 specifying the relative size of axis. See
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  1.

- tlab:

  Character vector of length 1 specifying the label of the time axis.

- flab:

  Character vector of length 1 specifying the label of the frequency
  axis.

- title:

  Logical argument to add a title to individual spectrograms. Default is
  `TRUE`.

- axisX:

  Logical to control whether time axis is plotted. Default is `TRUE`.

- axisY:

  Logical to control whether frequency axis is plotted. Default is
  `TRUE`.

- flim:

  A numeric vector of length 2 for the frequency limit (in kHz) of the
  spectrogram, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  `NULL`.

- rm.zero:

  Logical indicated if the 0 at the start of the time axis should be
  removed. Default is `FALSE`.

- X:

  Optional. Data frame containing columns for start and end time of
  signals ('start' and 'end') and low and high frequency ('bottom.freq'
  and 'top.freq').

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

- t.mar:

  Numeric vector of length 1. Specifies the margins adjacent to the
  start and end points to be added when highlighting selection. Default
  is `NULL`.

- f.mar:

  Numeric vector of length 1. Specifies the margins adjacent to the low
  and high frequencies to be added when highlighting selection. Default
  is `NULL`.

- interactive:

  Numeric. Allow user to interactively select the signals to be
  highlighted by clicking on the graphic device. Users must select the
  opposite corners of a square delimiting the spectrogram region to be
  highlighted. Controls the number of signals that users would be able
  to select (2 clicks per signal).

- add:

  Logical. If `TRUE` new highlighting can be applied to the current plot
  (which means that the function with `add = FALSE` should be run
  first). Default is `FALSE`.

## Value

A plot is produced in the graphic device.

## Details

This function highlights regions of the spectrogram with different
colors. The regions to be highlighted can be provided in a selection
table (as the example data 'lbh_selec_table') or interactively
('interactive' argument).

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
[`freq_DTW()`](https://marce10.github.io/warbleR/reference/freq_DTW.md),
[`multi_DTW()`](https://marce10.github.io/warbleR/reference/multi_DTW.md),
[`phylo_spectro()`](https://marce10.github.io/warbleR/reference/phylo_spectro.md),
[`snr_spectrograms()`](https://marce10.github.io/warbleR/reference/snr_spectrograms.md),
[`spectrograms()`](https://marce10.github.io/warbleR/reference/spectrograms.md),
[`track_freq_contour()`](https://marce10.github.io/warbleR/reference/track_freq_contour.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>) and Grace Smith Vidaurre

## Examples

``` r
if (FALSE) { # \dontrun{
data(list = c("Phae.long1", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files

# subset selection table
st <- lbh_selec_table[lbh_selec_table$sound.files == "Phae.long1.wav", ]

# read wave file as an R object
sgnl <- tuneR::readWave(file.path(tempdir(), st$sound.files[1]))

# create color column
st$colors <- c("red2", "blue", "green")

# highlight selections
color_spectro(
  wave = sgnl, wl = 300, ovlp = 90, flim = c(1, 8.6), collevels = seq(-40, 0, 5),
  dB = "B", X = st, col.clm = "colors", base.col = "skyblue", t.mar = 0.07, f.mar = 0.1,
  interactive = NULL
)

# interactive (selected manually: you have to select them by clicking on the spectrogram)
color_spectro(
  wave = sgnl, wl = 300, ovlp = 90, flim = c(1, 8.6), collevels = seq(-40, 0, 5),
  dB = "B", col.clm = "colors", t.mar = 0.07, f.mar = 1, interactive = 2
)
} # }
```
