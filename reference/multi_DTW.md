# A wrapper on [`dtwDist`](https://rdrr.io/pkg/dtw/man/dtwDist.html) for comparing multivariate contours

`multi_DTW` is a wrapper on
[`dtwDist`](https://rdrr.io/pkg/dtw/man/dtwDist.html) that simplify
applying dynamic time warping on multivariate contours.

## Usage

``` r
multi_DTW(
  ts.df1 = NULL,
  ts.df2 = NULL,
  pb = TRUE,
  parallel = 1,
  window.type = "none",
  open.end = FALSE,
  scale = FALSE,
  dist.mat = TRUE,
  ...
)
```

## Arguments

- ts.df1:

  Optional. Data frame with frequency contour time series of signals to
  be compared.

- ts.df2:

  Optional. Data frame with frequency contour time series of signals to
  be compared.

- pb:

  Logical argument to control progress bar. Default is `TRUE`. Note that
  progress bar is only used when parallel = 1.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing). Not available in Windows OS.

- window.type:

  [`dtw`](https://rdrr.io/pkg/dtw/man/dtw.html) windowing control
  parameter. Character: "none", "itakura", or a function (see
  [`dtw`](https://rdrr.io/pkg/dtw/man/dtw.html)).

- open.end:

  [`dtw`](https://rdrr.io/pkg/dtw/man/dtw.html) control parameter.
  Performs open-ended alignments (see
  [`dtw`](https://rdrr.io/pkg/dtw/man/dtw.html)).

- scale:

  Logical. If `TRUE` dominant frequency values are z-transformed using
  the [`scale`](https://rdrr.io/r/base/scale.html) function, which
  "ignores" differences in absolute frequencies between the signals in
  order to focus the comparison in the frequency contour, regardless of
  the pitch of signals. Default is `TRUE`.

- dist.mat:

  Logical controlling whether a distance matrix (`TRUE`, default) or a
  tabular data frame (`FALSE`) is returned.

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
Evolution, 8(2), 184-191.

## See also

[`freq_ts`](https://marce10.github.io/warbleR/reference/freq_ts.md)

Other spectrogram creators:
[`color_spectro()`](https://marce10.github.io/warbleR/reference/color_spectro.md),
[`freq_DTW()`](https://marce10.github.io/warbleR/reference/freq_DTW.md),
[`phylo_spectro()`](https://marce10.github.io/warbleR/reference/phylo_spectro.md),
[`snr_spectrograms()`](https://marce10.github.io/warbleR/reference/snr_spectrograms.md),
[`spectrograms()`](https://marce10.github.io/warbleR/reference/spectrograms.md),
[`track_freq_contour()`](https://marce10.github.io/warbleR/reference/track_freq_contour.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
# load data
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))

writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# measure
df <- freq_ts(X = lbh_selec_table, threshold = 10, img = FALSE, path = tempdir())
se <- freq_ts(X = lbh_selec_table, threshold = 10, img = FALSE, path = tempdir(), type = "entropy")

# run function
multi_DTW(df, se)
} # }
```
