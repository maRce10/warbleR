# Acoustic dissimilarity using dynamic time warping on dominant frequency contours

`freq_DTW` calculates acoustic dissimilarity of frequency contours using
dynamic time warping. Internally it applies the
[`dtwDist`](https://rdrr.io/pkg/dtw/man/dtwDist.html) function from the
`dtw` package.

## Usage

``` r
freq_DTW(
  X = NULL,
  type = "dominant",
  wl = 512,
  wl.freq = 512,
  length.out = 20,
  wn = "hanning",
  ovlp = 70,
  bp = NULL,
  threshold = 15,
  threshold.time = NULL,
  threshold.freq = NULL,
  img = TRUE,
  parallel = 1,
  path = NULL,
  ts.df = NULL,
  img.suffix = "dfDTW",
  pb = TRUE,
  clip.edges = TRUE,
  window.type = "none",
  open.end = FALSE,
  scale = FALSE,
  fsmooth = 0.1,
  adjust.wl = TRUE,
  max.obs.per.core = 20,
  ...
)
```

## Arguments

- X:

  object of class 'selection_table', 'extended_selection_table' or data
  frame containing columns for sound file name (sound.files), selection
  number (selec), and start and end time of signal (start and end).

- type:

  Character string to determine the type of contour to be detected.
  Three options are available, "dominant" (default), "fundamental" and
  "entropy".

- wl:

  A numeric vector of length 1 specifying the window length of the
  spectrogram, default is 512.

- wl.freq:

  A numeric vector of length 1 specifying the window length of the
  spectrogram for measurements on the frequency spectrum. Default
  is 512. Higher values would provide more accurate measurements.

- length.out:

  A numeric vector of length 1 giving the number of measurements of
  frequency desired (the length of the time series).

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

- bp:

  A numeric vector of length 2 for the lower and upper limits of a
  frequency bandpass filter (in kHz). Default is `NULL`.

- threshold:

  amplitude threshold (%) for frequency detection. Default is 15.

- threshold.time:

  amplitude threshold (%) for the time domain. Use for frequency
  detection. If `NULL` (default) then the 'threshold' value is used.

- threshold.freq:

  amplitude threshold (%) for the frequency domain. Use for frequency
  range detection from the spectrum (see 'frange.detec'). If `NULL`
  (default) then the 'threshold' value is used.

- img:

  Logical argument. If `FALSE`, image files are not produced. Default is
  `TRUE`.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing). In this function parallelization improves performance only
  if the number of rows in 'X' is at least twice the number of cores to
  be used.

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used.

- ts.df:

  Optional. Data frame with frequency contour time series of signals to
  be compared. If provided "X" is ignored.

- img.suffix:

  A character vector of length 1 with a suffix (label) to add at the end
  of the names of image files. Default is `NULL`.

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- clip.edges:

  Logical argument to control whether edges (start or end of signal) in
  which amplitude values above the threshold were not detected will be
  removed. If `TRUE` (default) this edges will be excluded and contours
  will be calculated on the remaining values. Note that DTW cannot be
  applied if missing values (e.i. when amplitude is not detected).

- window.type:

  [`dtw`](https://rdrr.io/pkg/dtw/man/dtw.html) windowing control
  parameter. Character: "none", "itakura", or a function (see
  [`dtw`](https://rdrr.io/pkg/dtw/man/dtw.html)).

- open.end:

  [`dtw`](https://rdrr.io/pkg/dtw/man/dtw.html) control parameter.
  Performs open-ended alignments (see
  [`dtw`](https://rdrr.io/pkg/dtw/man/dtw.html)).

- scale:

  Logical. If `TRUE` frequency values are z-transformed using the
  [`scale`](https://rdrr.io/r/base/scale.html) function, which "ignores"
  differences in absolute frequencies between the signals in order to
  focus the comparison in the frequency contour, regardless of the pitch
  of signals. Default is `TRUE`.

- fsmooth:

  A numeric vector of length 1 to smooth the frequency spectrum with a
  mean sliding window (in kHz) used for frequency range detection (when
  `frange.detec = TRUE`). This help to average amplitude "hills" to
  minimize the effect of amplitude modulation. Default is 0.1.

- adjust.wl:

  Logical. If `TRUE` 'wl' (window length) is reset to be lower than the
  number of samples in a selection if the number of samples is less than
  'wl'. Default is `TRUE`.

- max.obs.per.core:

  Numeric. Maximum number of observations per core to be used in
  parallel computing. Default is 100. Reduce this value if you have
  memory issues.

- ...:

  Additional arguments to be passed to
  [`track_freq_contour`](https://marce10.github.io/warbleR/reference/track_freq_contour.md)
  for customizing graphical output.

## Value

A matrix with the pairwise dissimilarity values. If img is `FALSE` it
also produces image files with the spectrograms of the signals listed in
the input data frame showing the location of the dominant frequencies.

## Details

This function extracts the dominant frequency values as a time series
and then calculates the pairwise acoustic dissimilarity using dynamic
time warping. The function uses the
[`approx`](https://rdrr.io/r/stats/approxfun.html) function to
interpolate values between dominant frequency measures. If 'img' is
`TRUE` the function also produces image files with the spectrograms of
the signals listed in the input data frame showing the location of the
dominant frequencies.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191. Müller, M. (2007). Dynamic time warping.
Information retrieval for music and motion, 69-84.

## See also

[`spectrograms`](https://marce10.github.io/warbleR/reference/spectrograms.md)
for creating spectrograms from selections,
[`snr_spectrograms`](https://marce10.github.io/warbleR/reference/snr_spectrograms.md)
for creating spectrograms to optimize noise margins used in
[`sig2noise`](https://marce10.github.io/warbleR/reference/sig2noise.md)
and [`freq_ts`](https://marce10.github.io/warbleR/reference/freq_ts.md),
[`freq_ts`](https://marce10.github.io/warbleR/reference/freq_ts.md), for
frequency contour overlaid spectrograms.

Other spectrogram creators:
[`color_spectro()`](https://marce10.github.io/warbleR/reference/color_spectro.md),
[`multi_DTW()`](https://marce10.github.io/warbleR/reference/multi_DTW.md),
[`phylo_spectro()`](https://marce10.github.io/warbleR/reference/phylo_spectro.md),
[`snr_spectrograms()`](https://marce10.github.io/warbleR/reference/snr_spectrograms.md),
[`spectrograms()`](https://marce10.github.io/warbleR/reference/spectrograms.md),
[`track_freq_contour()`](https://marce10.github.io/warbleR/reference/track_freq_contour.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  # load data
  data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) # save sound files
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))

  # dominant frequency
  freq_DTW(lbh_selec_table,
    length.out = 30, flim = c(1, 12), bp = c(2, 9),
    wl = 300, path = tempdir()
  )

  # fundamental frequency
  freq_DTW(lbh_selec_table,
    type = "fundamental", length.out = 30, flim = c(1, 12),
    bp = c(2, 9), wl = 300, path = tempdir()
  )
}
#> calculating DTW distances, no progress bar ...
#> calculating DTW distances, no progress bar ...
#>                  Phae.long1.wav-1 Phae.long1.wav-2 Phae.long1.wav-3
#> Phae.long1.wav-1           0.0000          20.2601          20.2518
#> Phae.long1.wav-2          20.2601           0.0000          13.2831
#> Phae.long1.wav-3          20.2518          13.2831           0.0000
#> Phae.long2.wav-1          15.4428          89.3132          83.3434
#> Phae.long2.wav-2          17.7108          84.4165          75.8621
#> Phae.long3.wav-1          35.7880         105.3692          93.4466
#> Phae.long3.wav-2          12.7952          89.8322          81.4853
#> Phae.long3.wav-3          33.2880          99.5629          86.9659
#> Phae.long4.wav-1          20.6536          23.4967          31.0400
#> Phae.long4.wav-2          20.3148          22.7303          29.2321
#> Phae.long4.wav-3          19.8954          21.8002          28.3667
#>                  Phae.long2.wav-1 Phae.long2.wav-2 Phae.long3.wav-1
#> Phae.long1.wav-1          15.4428          17.7108          35.7880
#> Phae.long1.wav-2          89.3132          84.4165         105.3692
#> Phae.long1.wav-3          83.3434          75.8621          93.4466
#> Phae.long2.wav-1           0.0000          11.0944           9.0332
#> Phae.long2.wav-2          11.0944           0.0000          25.5835
#> Phae.long3.wav-1           9.0332          25.5835           0.0000
#> Phae.long3.wav-2           4.7227          12.1370           4.6336
#> Phae.long3.wav-3           8.2250          16.2625           3.5236
#> Phae.long4.wav-1          49.0280          42.5728          64.5342
#> Phae.long4.wav-2          50.2424          42.5583          63.3510
#> Phae.long4.wav-3          50.7279          41.8545          63.9772
#>                  Phae.long3.wav-2 Phae.long3.wav-3 Phae.long4.wav-1
#> Phae.long1.wav-1          12.7952          33.2880          20.6536
#> Phae.long1.wav-2          89.8322          99.5629          23.4967
#> Phae.long1.wav-3          81.4853          86.9659          31.0400
#> Phae.long2.wav-1           4.7227           8.2250          49.0280
#> Phae.long2.wav-2          12.1370          16.2625          42.5728
#> Phae.long3.wav-1           4.6336           3.5236          64.5342
#> Phae.long3.wav-2           0.0000           8.1572          53.3453
#> Phae.long3.wav-3           8.1572           0.0000          64.8988
#> Phae.long4.wav-1          53.3453          64.8988           0.0000
#> Phae.long4.wav-2          53.7884          64.0806           1.8670
#> Phae.long4.wav-3          53.8636          65.3580           2.8345
#>                  Phae.long4.wav-2 Phae.long4.wav-3
#> Phae.long1.wav-1          20.3148          19.8954
#> Phae.long1.wav-2          22.7303          21.8002
#> Phae.long1.wav-3          29.2321          28.3667
#> Phae.long2.wav-1          50.2424          50.7279
#> Phae.long2.wav-2          42.5583          41.8545
#> Phae.long3.wav-1          63.3510          63.9772
#> Phae.long3.wav-2          53.7884          53.8636
#> Phae.long3.wav-3          64.0806          65.3580
#> Phae.long4.wav-1           1.8670           2.8345
#> Phae.long4.wav-2           0.0000           1.4648
#> Phae.long4.wav-3           1.4648           0.0000
```
