# Count number of inflections in a frequency contour

`inflections` counts the number of inflections in a frequency contour
(or any time series)

## Usage

``` r
inflections(X = NULL, parallel = 1, pb = TRUE)
```

## Arguments

- X:

  data frame with the columns for "sound.files" (sound file name),
  "selec" (unique identifier for each selection) and columns for each of
  the frequency values of the contours. No other columns should be
  included.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar and messages. Default is
  `TRUE`.

## Value

A data frame with 3 columns: "sound.files", "selec" and "infls" (number
of inflections).

## Details

The function counts the number of inflections in a frequency contour.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`freq_ts`](https://marce10.github.io/warbleR/reference/freq_ts.md),
[`track_freq_contour`](https://marce10.github.io/warbleR/reference/track_freq_contour.md),
[`gaps`](https://marce10.github.io/warbleR/reference/gaps.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
# get warbleR sound file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# measure frequency contours
dom.freq.ts <- freq_ts(X = lbh_selec_table, path = tempdir())

# get number of inflections
inflections(X = dom.freq.ts)
}
#>       sound.files selec inflections
#> 1  Phae.long1.wav     1           6
#> 2  Phae.long1.wav     2           7
#> 3  Phae.long1.wav     3           7
#> 4  Phae.long2.wav     1           9
#> 5  Phae.long2.wav     2           6
#> 6  Phae.long3.wav     1           9
#> 7  Phae.long3.wav     2           5
#> 8  Phae.long3.wav     3           6
#> 9  Phae.long4.wav     1           3
#> 10 Phae.long4.wav     2           3
#> 11 Phae.long4.wav     3           3
```
