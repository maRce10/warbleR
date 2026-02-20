# Detect frequency range iteratively

`freq_range` detect frequency range iteratively from signals in a
selection table.

## Usage

``` r
freq_range(
  X,
  wl = 512,
  it = "jpeg",
  line = TRUE,
  fsmooth = 0.1,
  threshold = 10,
  dB.threshold = NULL,
  wn = "hanning",
  flim = NULL,
  bp = NULL,
  propwidth = FALSE,
  xl = 1,
  picsize = 1,
  res = 100,
  fast.spec = FALSE,
  ovlp = 50,
  pal = reverse.gray.colors.2,
  parallel = 1,
  widths = c(2, 1),
  main = NULL,
  img = TRUE,
  mar = 0.05,
  path = NULL,
  pb = TRUE,
  impute = FALSE
)
```

## Arguments

- X:

  object of class 'selection_table', 'extended_selection_table' or data
  frame with the following columns: 1) "sound.files": name of the sound
  files, 2) "selec": number of the selections, 3) "start": start time of
  selections, 4) "end": end time of selections.

- wl:

  A numeric vector of length 1 specifying the window length of the
  spectrogram, default is 512. This is used for calculating the
  frequency spectrum (using
  [`meanspec`](https://rdrr.io/pkg/seewave/man/meanspec.html)) and
  producing the spectrogram (using
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html), if
  `img = TRUE`).

- it:

  A character vector of length 1 giving the image type to be used.
  Currently only "tiff" and "jpeg" are admitted. Default is "jpeg".

- line:

  Logical argument to add red lines (or box if bottom.freq and top.freq
  columns are provided) at start and end times of selection. Default is
  `TRUE`.

- fsmooth:

  A numeric vector of length 1 to smooth the frequency spectrum with a
  mean sliding window in kHz. This help to average amplitude "hills" to
  minimize the effect of amplitude modulation. Default is 0.1.

- threshold:

  Amplitude threshold (%) for frequency range detection. The frequency
  range (not the cumulative amplitude) is represented as percentage
  (100% = highest amplitude). Default is 10. Ignored if 'dB.threshold'
  is supplied.

- dB.threshold:

  Amplitude threshold for frequency range detection (in dB). The value
  indicates the decrease in dB in relation to the highest amplitude
  (e.g. the peak frequency) in which range will be detected. For
  instance a dB.threshold = 20 means that the amplitude threshold would
  be 20 dB below the highest amplitude. If provided 'threshold' is
  ignored. Default is `NULL`. Note that the power spectrum is normalized
  when using a dB scale, so it looks different than the one produced
  when no dB scale is used (e.g. when using 'threshold' argument).

- wn:

  Character vector of length 1 specifying window name. Default is
  "hanning". See function
  [`ftwindow`](https://rdrr.io/pkg/seewave/man/ftwindow.html) for more
  options. This is used for calculating the frequency spectrum (using
  [`meanspec`](https://rdrr.io/pkg/seewave/man/meanspec.html)) and
  producing the spectrogram (using
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html), if
  `img = TRUE`).

- flim:

  A numeric vector of length 2 for the frequency limit of the
  spectrogram (in kHz), as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default is
  `NULL`.

- bp:

  A numeric vector of length 2 for the lower and upper limits of a
  frequency bandpass filter (in kHz) or "frange" to indicate that values
  in 'bottom.freq' and 'top.freq' columns will be used as bandpass
  limits. Default is c(0, 22).

- propwidth:

  Logical argument to scale the width of spectrogram proportionally to
  duration of the selected call. Default is `FALSE`.

- xl:

  Numeric vector of length 1. A constant by which to scale spectrogram
  width. Default is 1.

- picsize:

  Numeric argument of length 1. Controls relative size of spectrogram.
  Default is 1.

- res:

  Numeric argument of length 1. Controls image resolution. Default is
  100 (faster) although 300 - 400 is recommended for publication/
  presentation quality.

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
  'fast.spec' spectrograms. Palette colors
  [`gray.1`](https://rdrr.io/pkg/monitoR/man/specCols.html),
  [`gray.2`](https://rdrr.io/pkg/monitoR/man/specCols.html),
  [`gray.3`](https://rdrr.io/pkg/monitoR/man/specCols.html) offer
  decreasing darkness levels.

- ovlp:

  Numeric vector of length 1 specifying % of overlap between two
  consecutive windows, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default
  is 50. This is used for calculating the frequency spectrum (using
  [`meanspec`](https://rdrr.io/pkg/seewave/man/meanspec.html)) and
  producing the spectrogram (using
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html), if
  `img = TRUE`).

- pal:

  Color palette function for spectrogram. Default is
  reverse.gray.colors.2. See
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html) for more
  palettes. Palettes as
  [`gray.2`](https://rdrr.io/pkg/monitoR/man/specCols.html) may work
  better when `fast.spec = TRUE`.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- widths:

  Numeric vector of length 2 to control the relative widths of the
  spectro (first element) and spectrum (second element).

- main:

  Character vector of length 1 specifying the img title. Default is
  `NULL`.

- img:

  Logical. Controls whether a plot is produced. Default is `TRUE`.

- mar:

  Numeric vector of length 1. Specifies the margins adjacent to the
  selections to set spectrogram limits. Default is 0.05.

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used.

- pb:

  Logical argument to control progress bar and messages. Default is
  `TRUE`.

- impute:

  Logical. If `TRUE` then missing range values are imputed with the
  corresponding bandpass value (hence ignored when `bp = NULL`). Default
  is `FALSE`.

## Value

The original data frame with an additional 2 columns for low and high
frequency values. A plot is produced in the working directory if
`img = TRUE` (see details).

## Details

This functions aims to automatize the detection of frequency ranges. The
frequency range is calculated as follows:

- bottom.freq = the start frequency of the amplitude 'hill' containing
  the highest amplitude at the given threshold.

- top.freq = the end frequency of the amplitude 'hill' containing the
  highest amplitude at the given threshold.

If `img = TRUE` a graph including a spectrogram and a frequency spectrum
is generated for each selection (saved as an image file in the working
directory). The graph would include gray areas in the frequency ranges
excluded by the bandpass ('bp' argument), dotted lines highlighting the
detected range. The function
[`freq_range_detec`](https://marce10.github.io/warbleR/reference/freq_range_detec.md)
is used internally.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`freq_range_detec`](https://marce10.github.io/warbleR/reference/freq_range_detec.md),
[`freq_ts`](https://marce10.github.io/warbleR/reference/freq_ts.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  freq_range(
    X = lbh_selec_table, wl = 112, fsmooth = 1, threshold = 13, widths = c(4, 1),
    img = TRUE, pb = TRUE, it = "tiff", line = TRUE, mar = 0.1, bp = c(1, 10.5),
    flim = c(0, 11), path = tempdir()
  )
}
#>       sound.files channel selec     start       end bottom.freq  top.freq
#> 1  Phae.long1.wav       1     1 1.1693549 1.3423884    3.149291  8.782169
#> 2  Phae.long1.wav       1     2 2.1584085 2.3214565    3.149291  8.782169
#> 3  Phae.long1.wav       1     3 0.3433366 0.5182553    2.932642  8.998818
#> 4  Phae.long2.wav       1     1 0.1595983 0.2921692    3.365940 10.298713
#> 5  Phae.long2.wav       1     2 1.4570585 1.5832087    4.015888  8.998818
#> 6  Phae.long3.wav       1     1 0.6265520 0.7577715    4.882484  9.215467
#> 7  Phae.long3.wav       1     2 1.9742132 2.1043921    4.665835  8.998818
#> 8  Phae.long3.wav       1     3 0.1233643 0.2545812    4.665835  9.215467
#> 9  Phae.long4.wav       1     1 1.5168116 1.6622365    2.715993  8.998818
#> 10 Phae.long4.wav       1     2 2.9326920 3.0768784    2.715993  9.432117
#> 11 Phae.long4.wav       1     3 0.1453977 0.2904966    2.499343  8.998818
```
