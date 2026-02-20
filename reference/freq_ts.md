# Extract frequency contours as time series

`freq_ts` extracts the fundamental frequency values as a time series.

## Usage

``` r
freq_ts(
  X,
  type = "dominant",
  wl = 512,
  length.out = 20,
  wn = "hanning",
  ovlp = 70,
  bp = NULL,
  threshold = 15,
  img = TRUE,
  parallel = 1,
  path = NULL,
  img.suffix = "frequency.ts",
  pb = TRUE,
  clip.edges = FALSE,
  leglab = "frequency.ts",
  track.harm = FALSE,
  raw.contour = FALSE,
  adjust.wl = TRUE,
  ff.method = "seewave",
  entropy.range = c(2, 10),
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

- length.out:

  A numeric vector of length 1 giving the number of measurements of
  fundamental frequency desired (the length of the time series).

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

  amplitude threshold (%) for fundamental frequency detection. Default
  is 15.

- img:

  Logical argument. If `FALSE`, image files are not produced. Default is
  `TRUE`.

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
  of the names of image files.

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- clip.edges:

  Logical argument to control whether edges (start or end of signal) in
  which amplitude values above the threshold were not detected will be
  removed. If `TRUE` this edges will be excluded and signal contour will
  be calculated on the remaining values. Default is `FALSE`. \#' @param
  leglab A character vector of length 1 or 2 containing the label(s) of
  the frequency contour legend in the output image.

- leglab:

  A character vector of length 1 or 2 containing the label(s) of the
  frequency contour legend in the output image.

- track.harm:

  Logical. If `TRUE` warbleR's
  [`track_harmonic`](https://marce10.github.io/warbleR/reference/track_harmonic.md)
  function is used to track dominant frequency contours. Otherwise
  seewave's [`dfreq`](https://rdrr.io/pkg/seewave/man/dfreq.html) is
  used by default. Default is `FALSE`.

- raw.contour:

  Logical. If `TRUE` then a list with the original contours (i.e.
  without interpolating values to make all contours of equal length) is
  returned (and no images are produced).

- adjust.wl:

  Logical. If `TRUE` 'wl' (window length) is reset to be lower than the
  number of samples in a selection if the number of samples is less than
  'wl'. Default is `TRUE`. Used only for dominant frequency detection.

- ff.method:

  Character. Selects the method used to detect fundamental frequency
  contours. Either 'tuneR' (using
  [`FF`](https://rdrr.io/pkg/tuneR/man/FF.html)) or 'seewave' (using
  [`fund`](https://rdrr.io/pkg/seewave/man/fund.html)). Default is
  'seewave'. 'tuneR' performs faster (and seems to be more accurate)
  than 'seewave'.

- entropy.range:

  Numeric vector of length 2. Range of frequency in which to display the
  entropy values on the spectrogram (when img = TRUE). Default is c(2,
  10). Negative values can be used in order to stretch more the range.

- ...:

  Additional arguments to be passed to
  [`track_freq_contour`](https://marce10.github.io/warbleR/reference/track_freq_contour.md).
  for customizing graphical output.

## Value

A data frame with the fundamental frequency values measured across the
signals. If img is `TRUE` it also produces image files with the
spectrograms of the signals listed in the input data frame showing the
location of the fundamental frequencies (see
[`track_freq_contour`](https://marce10.github.io/warbleR/reference/track_freq_contour.md)
description for more details).

## Details

This function extracts the dominant frequency, fundamental frequency or
spectral entropy contours as time series. The function uses the
[`approx`](https://rdrr.io/r/stats/approxfun.html) function to
interpolate values between frequency measures. If there are no
frequencies above the amplitude threshold (for dominant and fundamental)
at the beginning or end of the signals then NAs will be generated. On
the other hand, if there are no frequencies above the amplitude
threshold in between signal segments in which amplitude was detected
then the values of this adjacent segments will be interpolated to fill
out the missing values (e.g. no NAs in between detected amplitude
segments).

## See also

[`sig2noise`](https://marce10.github.io/warbleR/reference/sig2noise.md),
[`track_freq_contour`](https://marce10.github.io/warbleR/reference/track_freq_contour.md),
`freq_ts`,
[`freq_DTW`](https://marce10.github.io/warbleR/reference/freq_DTW.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
#load data
data(list = c("Phae.long1", "Phae.long2","lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) #save sound files
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav")) #save sound files

# run function with dominant frequency
freq_ts(X = lbh_selec_table, length.out = 30, flim = c(1, 12), bp = c(2, 9),
wl = 300, pb = FALSE, path = tempdir())

# note a NA in the row 4 column 3 (dfreq-1)
# this can be removed by clipping edges (removing NAs at the start and/or end
# when no freq was detected)

freq_ts(X = lbh_selec_table, length.out = 30, flim = c(1, 12), bp = c(2, 9),
wl = 300, pb = FALSE, clip.edges = TRUE, path = tempdir())

# \donttest{
# run function with fundamental frequency
freq_ts(lbh_selec_table, type = "fundamental", length.out = 50,
flim = c(1, 12),  bp = c(2, 9), wl = 300, path = tempdir())
 
# run function with spectral entropy
# without clip edges
freq_ts(X = lbh_selec_table, type = "entropy", threshold = 10,
clip.edges = FALSE, length.out = 10, sp.en.range = c(-25, 10), path = tempdir(),
 img = FALSE)

# with clip edges and length.out 10
freq_ts(X = lbh_selec_table, type = "entropy", threshold = 10, bp = c(2, 12),
clip.edges = TRUE, length.out = 10, path = tempdir(), img = FALSE)
 # }
}
#>       sound.files selec ffreq-1 ffreq-2 ffreq-3 ffreq-4 ffreq-5 ffreq-6 ffreq-7
#> 1  Phae.long1.wav     1  0.8954  0.8428  0.8594  0.8910  0.8402  0.8801  0.8608
#> 2  Phae.long1.wav     2  0.9015  0.8020  0.8867  0.9032  0.8525  0.8418  0.9016
#> 3  Phae.long1.wav     3  0.9453  0.8812  0.8756  0.9307  0.8591  0.8781  0.9305
#> 4  Phae.long2.wav     1  0.9292  0.8915  0.9211  0.9203  0.8966  0.8790  0.8832
#> 5  Phae.long2.wav     2  0.9200  0.9000  0.9225  0.9245  0.8980  0.8786  0.8748
#> 6  Phae.long3.wav     1  0.9317  0.9047  0.8522  0.8797  0.8587  0.8638  0.8494
#> 7  Phae.long3.wav     2  0.9325  0.9040  0.8836  0.8730  0.8540  0.8670  0.8633
#> 8  Phae.long3.wav     3  0.9161  0.9012  0.8321  0.8720  0.8539  0.8622  0.8657
#> 9  Phae.long4.wav     1  0.9245  0.8420  0.8154  0.8248  0.8346  0.8837  0.8812
#> 10 Phae.long4.wav     2  0.9309  0.8351  0.8344  0.8342  0.8491  0.9000  0.8446
#> 11 Phae.long4.wav     3  0.9489  0.8281  0.8178  0.8389  0.8546  0.8864  0.8680
#>    ffreq-8 ffreq-9 ffreq-10
#> 1   0.7787  0.8262   0.9335
#> 2   0.7977  0.8135   0.9227
#> 3   0.7821  0.8256   0.8622
#> 4   0.8718  0.8937   0.9275
#> 5   0.8774  0.8726   0.9095
#> 6   0.8543  0.8618   0.9335
#> 7   0.8450  0.8472   0.8607
#> 8   0.8406  0.8502   0.8941
#> 9   0.7762  0.8041   0.9012
#> 10  0.7699  0.8185   0.9032
#> 11  0.8041  0.8307   0.9120
```
