# Spectrograms with frequency measurements

`track_freq_contour` creates spectrograms to visualize dominant and
fundamental frequency measurements (contours)

## Usage

``` r
track_freq_contour(
  X,
  wl = 512,
  wl.freq = 512,
  flim = NULL,
  wn = "hanning",
  pal = reverse.gray.colors.2,
  ovlp = 70,
  inner.mar = c(5, 4, 4, 2),
  outer.mar = c(0, 0, 0, 0),
  picsize = 1,
  res = 100,
  cexlab = 1,
  title = TRUE,
  propwidth = FALSE,
  xl = 1,
  osci = FALSE,
  gr = FALSE,
  sc = FALSE,
  bp = NULL,
  cex = c(0.6, 1),
  threshold.time = NULL,
  threshold.freq = NULL,
  contour = "both",
  col = c("#E37222B3", "#07889BB3"),
  pch = c(21, 24),
  mar = 0.05,
  lpos = "topright",
  it = "jpeg",
  parallel = 1,
  path = NULL,
  img.suffix = NULL,
  custom.contour = NULL,
  pb = TRUE,
  type = "p",
  leglab = c("Ffreq", "Dfreq"),
  col.alpha = 0.6,
  line = TRUE,
  fast.spec = FALSE,
  ff.method = "seewave",
  frange.detec = FALSE,
  fsmooth = 0.1,
  widths = c(2, 1),
  freq.continuity = NULL,
  clip.edges = 2,
  track.harm = FALSE,
  ...
)
```

## Arguments

- X:

  object of class 'selection_table', 'extended_selection_table' or data
  frame containing columns for sound file name (sound.files), selection
  number (selec), and start and end time of signal (start and end).

- wl:

  A numeric vector of length 1 specifying the window length of the
  spectrogram, default is 512.

- wl.freq:

  A numeric vector of length 1 specifying the window length of the
  spectrogram for measurements on the frequency spectrum. Default
  is 512. Higher values would provide more accurate measurements.

- flim:

  A numeric vector of length 2 for the frequency limit of the
  spectrogram (in kHz), as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  `NULL`.

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

  Numeric vector of length 1 specifying % of overlap between two
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
  Default is 1.

- res:

  Numeric argument of length 1. Controls image resolution. Default is
  100 (faster) although 300 - 400 is recommended for publication/
  presentation quality.

- cexlab:

  Numeric vector of length 1 specifying the relative size of axis
  labels. See [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html).

- title:

  Logical argument to add a title to individual spectrograms. Default is
  `TRUE`.

- propwidth:

  Logical argument to scale the width of spectrogram proportionally to
  duration of the selected call. Default is `FALSE`.

- xl:

  Numeric vector of length 1. A constant by which to scale spectrogram
  width. Default is 1.

- osci:

  Logical argument to add an oscillogram underneath spectrogram, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  `FALSE`.

- gr:

  Logical argument to add grid to spectrogram. Default is `FALSE`.

- sc:

  Logical argument to add amplitude scale to spectrogram, default is
  `FALSE`.

- bp:

  A numeric vector of length 2 for the lower and upper limits of a
  frequency bandpass filter (in kHz) or "frange" to indicate that values
  in bottom.freq and top.freq columns will be used as bandpass limits.
  Default is `NULL`.

- cex:

  Numeric vector of length 2, specifies relative size of points plotted
  for frequency measurements and legend font/points, respectively. See
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html).

- threshold.time:

  amplitude threshold (%) for the time domain. Use for fundamental and
  dominant frequency detection. If `NULL` (default) then the 'threshold'
  value is used.

- threshold.freq:

  amplitude threshold (%) for the frequency domain. Use for frequency
  range detection from the spectrum (see 'frange.detec'). If `NULL`
  (default) then the 'threshold' value is used.

- contour:

  Character vector, one of "df", "ff" or "both", specifying whether the
  dominant or fundamental frequencies or both should be plotted. Default
  is "both".

- col:

  Vector of length 1 or 2 specifying colors of points plotted to mark
  fundamental and dominant frequency measurements respectively (if both
  are plotted). Default is `c("#E37222B3", "#07889BB3")`. Extreme values
  (lowest and highest) are highlighted in yellow.

- pch:

  Numeric vector of length 1 or 2 specifying plotting characters for the
  frequency measurements. Default is c(21, 24).

- mar:

  Numeric vector of length 1. Specifies the margins adjacent to the
  selections to set spectrogram limits. Default is 0.05.

- lpos:

  Character vector of length 1 or numeric vector of length 2, specifying
  position of legend. If the former, any keyword accepted by xy.coords
  can be used (see below). If the latter, the first value will be the x
  coordinate and the second value the y coordinate for the legend's
  position. Default is "topright".

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

- img.suffix:

  A character vector of length 1 with a suffix (label) to add at the end
  of the names of image files. Default is `NULL`.

- custom.contour:

  A data frame with frequency contours for exactly the same sound files
  and selection as in X. The frequency values are assumed to be equally
  spaced in between the start and end of the signal. The first 2 columns
  of the data frame should contain the 'sound.files' and 'selec' columns
  and should be identical to the corresponding columns in X (same
  order).

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- type:

  A character vector of length 1 indicating the type of frequency
  contour plot to be drawn. Possible types are "p" for points, "l" for
  lines and "b" for both.

- leglab:

  A character vector of length 1 or 2 containing the label(s) of the
  frequency contour legend in the output image.

- col.alpha:

  A numeric vector of length 1 within \[0,1\] indicating how transparent
  the lines/points should be.

- line:

  Logical argument to add red lines (or box if bottom.freq and top.freq
  columns are provided) at start and end times of selection. Default is
  `TRUE`.

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

- ff.method:

  Character. Selects the method used to calculate the fundamental
  frequency. Either 'tuneR' (using
  [`FF`](https://rdrr.io/pkg/tuneR/man/FF.html)) or 'seewave' (using
  [`fund`](https://rdrr.io/pkg/seewave/man/fund.html)). Default is
  'seewave'. 'tuneR' performs faster (and seems to be more accurate)
  than 'seewave'.

- frange.detec:

  Logical. Controls whether frequency range of signal is automatically
  detected using the
  [`freq_range_detec`](https://marce10.github.io/warbleR/reference/freq_range_detec.md)
  function. If so, the range is used as the bandpass filter (overwriting
  'bp' argument). Default is `FALSE`.

- fsmooth:

  A numeric vector of length 1 to smooth the frequency spectrum with a
  mean sliding window (in kHz) used for frequency range detection (when
  `frange.detec = TRUE`). This help to average amplitude "hills" to
  minimize the effect of amplitude modulation. Default is 0.1.

- widths:

  Numeric vector of length 2 to control the relative widths of the
  spectro (first element) and spectrum (second element, (when
  `frange.detec = TRUE`)).

- freq.continuity:

  Numeric vector of length 1 to control whether dominant frequency
  detections outliers(i.e that differ from the frequency of the
  detections right before and after) would be removed. Should be given
  in kHz. Default is `NULL`.

- clip.edges:

  Integer vector of length 1 to control if how many 'frequency-wise
  discontinuous' detection would be remove at the start and end of
  signals (see 'freq.continuity' argument). Default is 2. Ignored if
  `freq.continuity = NULL`.

- track.harm:

  Logical to control if
  [`track_harmonic`](https://marce10.github.io/warbleR/reference/track_harmonic.md)
  or a modified version of
  [`dfreq`](https://rdrr.io/pkg/seewave/man/dfreq.html) is used for
  dominant frequency detection. Default is `FALSE` (use
  [`dfreq`](https://rdrr.io/pkg/seewave/man/dfreq.html)).

- ...:

  Additional arguments to be passed to the internal spectrogram creating
  function for customizing graphical output. The function is a modified
  version of [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html),
  so it takes the same arguments.

## Value

Spectrograms of the signals listed in the input data frame showing the
location of the dominant and fundamental frequencies.

## Details

This function provides visualization of frequency measurements as the
ones made by
[`spectro_analysis`](https://marce10.github.io/warbleR/reference/spectro_analysis.md),
[`freq_ts`](https://marce10.github.io/warbleR/reference/freq_ts.md) and
[`freq_DTW`](https://marce10.github.io/warbleR/reference/freq_DTW.md).
Frequency measures can be made by the function or input by the user (see
'custom.contour' argument). If `frange = TRUE` the function uses
[`freq_range_detec`](https://marce10.github.io/warbleR/reference/freq_range_detec.md)
to detect the frequency range. In this case the graphical output
includes a frequency spectrum showing the detection threshold. Extreme
values (lowest and highest) are highlighted in yellow. Note that, unlike
other warbleR functions that measure frequency contours,
track_freq_contour do not interpolate frequency values.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`spectrograms`](https://marce10.github.io/warbleR/reference/spectrograms.md)
for creating spectrograms from selections,
[`snr_spectrograms`](https://marce10.github.io/warbleR/reference/snr_spectrograms.md)
for creating spectrograms to optimize noise margins used in
[`sig2noise`](https://marce10.github.io/warbleR/reference/sig2noise.md)

Other spectrogram creators:
[`color_spectro()`](https://marce10.github.io/warbleR/reference/color_spectro.md),
[`freq_DTW()`](https://marce10.github.io/warbleR/reference/freq_DTW.md),
[`multi_DTW()`](https://marce10.github.io/warbleR/reference/multi_DTW.md),
[`phylo_spectro()`](https://marce10.github.io/warbleR/reference/phylo_spectro.md),
[`snr_spectrograms()`](https://marce10.github.io/warbleR/reference/snr_spectrograms.md),
[`spectrograms()`](https://marce10.github.io/warbleR/reference/spectrograms.md)

## Author

Grace Smith Vidaurre and Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
 # load data
 data(list = c("lbh_selec_table","Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
 writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
 writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
 writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
 writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
 # track dominant frequency graphs with freq range detection
 track_freq_contour(
   X = lbh_selec_table,
   flim = c(1, 12),
   ovlp = 90,
   it = "jpeg",
   contour = "df",
   type = "l",
   frange.detec = FALSE,
   path = tempdir()
   )
}
```
