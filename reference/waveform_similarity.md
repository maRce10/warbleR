# Pairwise similarity of waveforms

`waveform_similarity` estimates the similarity of two sound waveforms

## Usage

``` r
waveform_similarity(
  X = NULL,
  wl = 512,
  bp = "pairwise.freq.range",
  ovlp = 70,
  sim.method = "correlation",
  type = "standard",
  parallel = 1,
  path = NULL,
  pb = TRUE,
  n = 100,
  output = "square"
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
  spectrogram, default is 512. Only used when applying a bandpass filter
  (`bp != NULL`).

- bp:

  A numeric vector of length 2 for the lower and upper limits of a
  frequency bandpass filter (in kHz). If columns for bottom and top
  frequency ('bottom.freq' and 'top.freq') are supplied
  "pairwise.freq.range" can be used (default). If so, the lowest values
  in 'bottom.freq' and the highest values in 'top.freq' for the
  selections involved in a pairwise comparison will be used as bandpass
  limits.

- ovlp:

  Numeric vector of length 1 specifying the percentage of overlap
  between two consecutive windows, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default
  is 70. High values of overlap slow down the function. Only used when
  applying a bandpass filter (`bp != NULL`).

- sim.method:

  A character string specifying the similarity method. Two option are
  available:

  - `"correlation"`: calculates the Pearson correlation between the
    waveforms of the two signals. Higher values indicate higher
    similarity.

  - `"DTW"`: calculates the Dynamic Time Warping distance between the
    waveforms of the two signals. Lower values indicate higher
    similarity.

- type:

  A character string specifying the approach for estimating similarity.
  Two option are available:

  - `"standard"`: estimates the similarity between the two waveforms
    with a single point estimate (e.g. the correlation or DTW distance
    between them). Default.

  - `"sliding"`: estimates the similarity between the two waveforms by
    calculating the correlation or DTW distance at each "sliding" step
    of the spectrogram of the shortest selection over the longest one.
    This approach is more computationally intensive but might be more
    appropriate when comparing sounds with large differences in duration
    or when the appropriate alignment of the waveforms is hard to
    determine.

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

- n:

  Numeric. Number of values used to represent each waveform. Default
  is 100. Note that both waveforms are forced to have the same length
  which is done by interpolating amplitude values using the function
  [`approx`](https://rdrr.io/r/stats/approxfun.html).

- output:

  A character string specifying the format of the output object with the
  estimated similarities. Two option are available:

  - `"square"`: a matrix in which each cell contains the pairwise
    similarity for the items in the row and columns. This is a symmetric
    matrix as both triangles above and below the diagonal are identical.
    Default.

  - `"rectangular"`: a data frame in which rows contain with the names
    of the two items being compared (1st and 2nd column) and the
    similarity value (3rd column). This is useful for plotting or
    subsetting the results.

## Value

The function returns a matrix (if `output = "square"`, default) or data
frame (if `output = "rectangular"`) with the similarity for each
pairwise comparison. The names of the items refer to the combination of
the 'sound.files' and 'selec' columns in 'X'. The values in the matrix
or in the third column of the data frame are the similarity values.

## Details

This function calculates the pairwise similarity of multiple waveforms
from annotations referenced in a selection table. Useful for the
analysis of acoustic fine structure (e.g. Prior et al. 2018). Waveforms
are forced to have the same length (see argument 'n'). This is done by
interpolating amplitude values using the function
[`approx`](https://rdrr.io/r/stats/approxfun.html). The function can be
used to compare waveforms using either the Pearson correlation
coefficient or the Dynamic Time Warping distance. The latter is a
measure of similarity between two sequences that may vary in the timing
of occurrence of the changes in amplitude. Make sure all sound files
have the same sampling rate (can be checked with
[`check_sels`](https://marce10.github.io/warbleR/reference/check_sels.md)
or
[`check_sound_files`](https://marce10.github.io/warbleR/reference/check_sound_files.md)).
Comparison can be done with a single point estimate (e.g. the
correlation or DTW distance between them) or by calculating the
correlation or DTW distance with a sliding window approach. This
approach is more computationally intensive but might be more appropriate
when comparing sounds with large differences in duration or when the
appropriate alignment of the waveforms is hard to determine.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

Müller, M. (2007). Dynamic time warping. Information retrieval for music
and motion, 69-84.

Prior, N. H., Smith, E., Lawson, S., Ball, G. F., & Dooling, R. J.
(2018). Acoustic fine structure may encode biologically relevant
information for zebra finches. Scientific reports, 8(1), 6212.

## See also

[`cross_correlation`](https://marce10.github.io/warbleR/reference/cross_correlation.md),
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

  # run waveform correlation
  wcor <- waveform_similarity(X = lbh_selec_table, path = tempdir())
}
```
