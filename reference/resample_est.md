# Resample wave objects in a extended selection table

`resample_est` changes sampling rate and bit depth of wave objects in a
extended selection table.

## Usage

``` r
resample_est(
  X,
  samp.rate = 44.1,
  bit.depth = 16,
  avoid.clip = TRUE,
  pb = FALSE,
  parallel = 1
)
```

## Arguments

- X:

  object of class 'extended_selection_table' (see
  [`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md)).

- samp.rate:

  Numeric vector of length 1 with the sampling rate (in kHz) for output
  files. Default is `NULL`.

- bit.depth:

  Numeric vector of length 1 with the dynamic interval (i.e. bit depth)
  for output files.

- avoid.clip:

  Logical to control whether the volume is automatically adjusted to
  avoid clipping high amplitude samples when resampling. Ignored if
  '`sox = FALSE`. Default is `TRUE`.

- pb:

  Logical argument to control progress bar. Default is `FALSE`.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

## Value

An extended selection table with the modified wave objects.

## Details

This function aims to simplify the process of homogenizing sound files
(sampling rate and bit depth). This is a necessary step before running
any further (bio)acoustic analysis.
[SOX](https://sourceforge.net/projects/sox/) must be installed.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`mp32wav`](https://marce10.github.io/warbleR/reference/mp32wav.md),
[`fix_wavs`](https://marce10.github.io/warbleR/reference/fix_wavs.md)

Other extended selection table manipulation:
[`by_element_est()`](https://marce10.github.io/warbleR/reference/by_element_est.md),
[`rename_est_waves()`](https://marce10.github.io/warbleR/reference/rename_est_waves.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# create extended selection table
X <- selection_table(
  X = lbh_selec_table, extended = TRUE, pb = FALSE,
  path = tempdir()
)

# resample
Y <- resample_est(X)
} # }
```
