# Spectrograms with background noise margins

`snr_spectrograms` creates spectrograms to visualize margins over which
background noise will be measured by
[`sig2noise`](https://marce10.github.io/warbleR/reference/sig2noise.md).

## Usage

``` r
snr_spectrograms(
  X,
  wl = 512,
  flim = NULL,
  wn = "hanning",
  ovlp = 70,
  inner.mar = c(5, 4, 4, 2),
  outer.mar = c(0, 0, 0, 0),
  picsize = 1,
  res = 100,
  cexlab = 1,
  title = TRUE,
  before = FALSE,
  eq.dur = FALSE,
  propwidth = FALSE,
  xl = 1,
  osci = FALSE,
  gr = FALSE,
  sc = FALSE,
  mar = 0.2,
  snrmar = 0.1,
  it = "jpeg",
  parallel = 1,
  path = NULL,
  pb = TRUE
)
```

## Arguments

- X:

  'selection_table', 'extended_selection_table' or any data frame with
  columns for sound file name (sound.files), selection number (selec),
  and start and end time of signal (start and end).

- wl:

  A numeric vector of length 1 specifying the window length of the
  spectrogram, default is 512.

- flim:

  A numeric vector of length 2 for the frequency limit in kHz of the
  spectrogram, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  `NULL`.

- wn:

  Character vector of length 1 specifying window name. Default is
  "hanning". See function
  [`ftwindow`](https://rdrr.io/pkg/seewave/man/ftwindow.html) for more
  options.

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

  Numeric argument of length 1, controls relative size of spectrogram.
  Default is 1.

- res:

  Numeric argument of length 1 that controls image resolution. Default
  is 100 (faster) although 300 - 400 is recommended for publication/
  presentation quality.

- cexlab:

  Numeric vector of length 1 specifying relative size of axis labels.
  See [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html).

- title:

  Logical argument to add a title to individual spectrograms. Default is
  `TRUE`.

- before:

  Logical. If `TRUE` noise is only measured right before the signal
  (instead of before and after). Default is `FALSE`.

- eq.dur:

  Logical. Controls whether the noise segment that is measured has the
  same duration than the signal (if `TRUE`, default `FALSE`). If `TRUE`
  then 'snrmar' argument is ignored.

- propwidth:

  Logical argument to scale the width of spectrogram proportionally to
  duration of the selected call. Default is `FALSE`.

- xl:

  Numeric vector of length 1, a constant by which to scale spectrogram
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

- mar:

  Numeric vector of length 1. Specifies the margins adjacent to the
  start and end points of the selections to define spectrogram limits.
  Default is 0.2. If snrmar is larger than mar, then mar is set to be
  equal to snrmar.

- snrmar:

  Numeric vector of length 1. Specifies the margins adjacent to the
  start and end points of the selections where noise will be measured.
  Default is 0.1.

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

## Value

Spectrograms per selection marked with margins where background noise
will be measured.

## Details

This function can be used to test different margins to facilitate
accurate SNR measurements when using
[`sig2noise`](https://marce10.github.io/warbleR/reference/sig2noise.md)
down the line. Setting margins for individual calls that have been
previously clipped from larger files may take some optimization, as for
calls within a larger file that are irregularly separated. Setting
inner.mar to c(4,4.5,2,1) and outer.mar to c(4,2,2,1) works well when
picsize = 2 or 3. Title font size, inner.mar and outer.mar (from `mar`
and `oma` in `par`) don't work well when osci or sc = `TRUE`, this may
take some optimization by the user.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191. [Wikipedia: Signal-to-noise
ratio](https://en.wikipedia.org/wiki/Signal-to-noise_ratio)

## See also

[`track_freq_contour`](https://marce10.github.io/warbleR/reference/track_freq_contour.md)
for creating spectrograms to visualize frequency measurements by
[`spectro_analysis`](https://marce10.github.io/warbleR/reference/spectro_analysis.md),
[`spectrograms`](https://marce10.github.io/warbleR/reference/spectrograms.md)
for creating spectrograms

Other spectrogram creators:
[`color_spectro()`](https://marce10.github.io/warbleR/reference/color_spectro.md),
[`freq_DTW()`](https://marce10.github.io/warbleR/reference/freq_DTW.md),
[`multi_DTW()`](https://marce10.github.io/warbleR/reference/multi_DTW.md),
[`phylo_spectro()`](https://marce10.github.io/warbleR/reference/phylo_spectro.md),
[`spectrograms()`](https://marce10.github.io/warbleR/reference/spectrograms.md),
[`track_freq_contour()`](https://marce10.github.io/warbleR/reference/track_freq_contour.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>) and Grace Smith Vidaurre

## Examples

``` r
if (FALSE) { # \dontrun{
data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound.files
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

# make Phae.long1 and Phae.long2 spectrograms
# snrmar needs to be smaller before moving on to sig2noise()

snr_spectrograms(lbh_selec_table,
  flim = c(0, 14), inner.mar = c(4, 4.5, 2, 1),
  outer.mar = c(4, 2, 2, 1), picsize = 2, res = 300, cexlab = 2, mar = 0.2,
  snrmar = 0.1, it = "jpeg", wl = 300, path = tempdir()
)

# make only Phae.long1 spectrograms
# snrmar now doesn't overlap neighboring signals

snr_spectrograms(lbh_selec_table[grepl(c("Phae.long1"), lbh_selec_table$sound.files), ],
  flim = c(3, 14), inner.mar = c(4, 4.5, 2, 1), outer.mar = c(4, 2, 2, 1),
  picsize = 2, res = 300, cexlab = 2, mar = 0.2, snrmar = 0.01, wl = 300,
  path = tempdir()
)

# check this folder!
tempdir()
} # }
```
