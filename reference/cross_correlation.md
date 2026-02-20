# Time-frequency cross-correlation

`cross_correlation` estimates the similarity of two sound waves by means
of time-frequency cross-correlation

## Usage

``` r
cross_correlation(
  X = NULL,
  wl = 512,
  bp = "pairwise.freq.range",
  ovlp = 70,
  wn = "hanning",
  cor.method = "pearson",
  parallel = 1,
  path = NULL,
  pb = TRUE,
  na.rm = FALSE,
  output = "cor.mat",
  templates = NULL,
  surveys = NULL,
  compare.matrix = NULL,
  type = "fourier",
  nbands = 40,
  method = 1
)
```

## Arguments

- X:

  'selection_table', 'extended_selection_table' or data frame containing
  columns for sound files (sound.files), selection number (selec), and
  start and end time of signal (start and end). All selections must have
  the same sampling rate.

- wl:

  A numeric vector of length 1 specifying the window length of the
  spectrogram, default is 512.

- bp:

  A numeric vector of length 2 for the lower and upper limits of a
  frequency bandpass filter (in kHz). If columns for bottom and top
  frequency ('bottom.freq' and 'top.freq') are supplied
  "pairwise.freq.range" can be used (default). If so, the lowest values
  in 'bottom.freq' and the highest values in 'top.freq' for the
  selections involved in a pairwise comparison will be used as bandpass
  limits.

- ovlp:

  Numeric vector of length 1 specifying % of overlap between two
  consecutive windows, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default
  is 70. High values of ovlp slow down the function but produce more
  accurate results.

- wn:

  A character vector of length 1 specifying the window name as in
  [`ftwindow`](https://rdrr.io/pkg/seewave/man/ftwindow.html).

- cor.method:

  A character vector of length 1 specifying the correlation method as in
  [`cor`](https://rdrr.io/r/stats/cor.html).

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

- na.rm:

  Logical. If `TRUE` all NAs produced when pairwise cross-correlations
  failed are removed from the results. This means that all selections
  with at least 1 cross-correlation that failed are excluded.

- output:

  Character vector of length 1 to determine if only the correlation
  matrix is returned ('cor.mat', default) or a list ('list')
  containing 1) the correlation matrix and 2) a data frame with
  correlation values at each sliding step for each comparison. The list,
  which is also of class 'xcorr.output', can be used to graphically
  explore detections using
  [`full_spectrograms`](https://marce10.github.io/warbleR/reference/full_spectrograms.md).

- templates:

  Character vector with the selections in 'X' to be used as templates
  for cross-correlation detection. To refer to specific selections in
  'X' the user must use the format "sound.file-selec" (e.g.
  "file1.wav-1"). If only the sound file name is included then the
  entire sound file is used as template.

- surveys:

  Character vector with the selections in 'X' to be used as surveys for
  cross-correlation detection. To refer to specific selections in 'X'
  the user must use the format "sound.file-selec" (e.g. "file1.wav-1").
  If only the sound file name is included then the entire sound file is
  used as survey.

- compare.matrix:

  A character matrix with 2 columns indicating the selections to be
  compared (column 1 vs column 2). The columns must contained the ID of
  the selection, which is given by combining the 'sound.files' and
  'selec' columns of 'X', separated by '-' (i.e.
  `paste(X$sound.files, X$selec, sep = "-")`). Default is `NULL`. If the
  'selec' ID is not supplied then the entire sound file is used instead.
  If 'compare.matrix' is supplied only those comparisons will be
  calculated (as opposed to all pairwise comparisons as the default
  behavior) and the output will be a data frame composed of the supplied
  matrix and the correspondent cross-correlation values. Note that
  'method' is automatically set to 2 (create spectrograms on the fly)
  when 'compare.matrix' is supplied but can be set back to 1. When
  supplied 'output' is forced to be 'list'. Note that the use of this
  function for signal detection (with 'compare.matrix') will be
  deprecated in future warbleR versions. Look at the ohun package for
  automatic signal detection functions.

- type:

  A character vector of length 1 specifying the type of
  cross-correlation; either "fourier" (i.e. spectrographic
  cross-correlation using Fourier transform; internally using
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html); default) or
  "mfcc" (Mel cepstral coefficient spectrogram cross-correlation;
  internally using
  [`melfcc`](https://rdrr.io/pkg/tuneR/man/melfcc.html)).

- nbands:

  Numeric vector of length 1 controlling the number of warped spectral
  bands to calculate when using `type = "mfcc"` (see
  [`melfcc`](https://rdrr.io/pkg/tuneR/man/melfcc.html)). Default is 40.

- method:

  Numeric vector of length 1 to control the method used to create
  spectrogram matrices. Two option are available:

  - `1`: matrices are created first (keeping them internally as a list)
    and cross-correlation is calculated on a second step. Note that this
    method may require lots of memory if selection and or sound files
    are large.

  - `2`: matrices are created "on the fly" (i.e. at the same time that
    cross-correlation is calculated). More memory efficient but may
    require extracting the same matrix several times, which will affect
    performance. Note that when using this method the function does not
    check if sound files have the same sampling rate which if not, may
    produce an error.

## Value

If `output = "cor.mat"` the function returns a matrix with the maximum
(peak) correlation for each pairwise comparison (if 'compare.matrix' is
not supplied) or the peak correlation for each comparison in the
supplied 'compare.matrix'. Otherwise it will return a list that
includes 1) a matrix with the maximum correlation for each pairwise
comparison ('max.xcorr.matrix') and 2) a data frame with the correlation
statistic for each "sliding" step ('scores').

## Details

This function calculates the pairwise similarity of multiple signals by
means of time-frequency cross-correlation. Fourier or Mel frequency
ceptral coefficients spectrograms can be used as time-frequency
representations of sound. This method "slides" the spectrogram of the
shortest selection over the longest one calculating a correlation of the
amplitude values at each step. The function runs pairwise
cross-correlations on several signals and returns a list including the
correlation statistic for each "sliding" step as well as the maximum
(peak) correlation for each pairwise comparison. The correlation matrix
could have NA's if some of the pairwise correlation did not work (common
when sound files have been modified by band-pass filters). Make sure all
sound files have the same sampling rate (can be checked with
[`check_sels`](https://marce10.github.io/warbleR/reference/check_sels.md)
or
[`check_sound_files`](https://marce10.github.io/warbleR/reference/check_sound_files.md)).

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

H. Khanna, S.L.L. Gaunt & D.A. McCallum (1997). Digital spectrographic
cross-correlation: tests of sensitivity. Bioacoustics 7(3): 209-234

Lyon, R. H., & Ordubadi, A. (1982). Use of cepstra in acoustical signal
analysis. Journal of Mechanical Design, 104(2), 303-306.

## See also

[`mfcc_stats`](https://marce10.github.io/warbleR/reference/mfcc_stats.md),
[`spectro_analysis`](https://marce10.github.io/warbleR/reference/spectro_analysis.md),
[`freq_DTW`](https://marce10.github.io/warbleR/reference/freq_DTW.md)

## Author

Marcelo Araya-Salas <marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  # load data
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))

  # save sound files
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  # run cross correlation on spectrograms (SPCC)
  xcor <- cross_correlation(X = lbh_selec_table, wl = 300, ovlp = 90, path = tempdir())

  # run cross correlation on Mel cepstral coefficients (mfccs)
  xcor <- cross_correlation(
    X = lbh_selec_table, wl = 300, ovlp = 90, path = tempdir(),
    type = "mfcc"
  )

  # using the 'compare.matrix' argument to specify pairwise comparisons
  # create matrix with ID of signals to compare
  cmp.mt <- cbind(
    paste(lbh_selec_table$sound.files[1:10], lbh_selec_table$selec[1:10], sep = "-"),
    paste(lbh_selec_table$sound.files[2:11], lbh_selec_table$selec[2:11], sep = "-")
  )

  # run cross-correlation on the selected pairwise comparisongs
  xcor <- cross_correlation(
    X = lbh_selec_table, compare.matrix = cmp.mt,
    wl = 300, ovlp = 90, path = tempdir()
  )
}
#> Warning: The use of this function for signal detection (with 'compare.matrix') will be deprecated in future warbleR versions, please look at the ohun package for automatic signal detection functions (https://marce10.github.io/ohun/index.html)
```
