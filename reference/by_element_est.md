# Convert a by-song extended selection table to by-element

`by_element_est` converts a by-song extended selection table to
by-element.

## Usage

``` r
by_element_est(X, mar = 0.1, pb = FALSE, parallel = 1)
```

## Arguments

- X:

  object of class 'extended_selection_table' (see
  [`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md)).

- mar:

  Numeric vector of length 1 specifying the margins (in seconds)
  adjacent to the start and end points of the annotations when creating
  the ”by element' extended selection table. Default is 0.1.

- pb:

  Logical argument to control progress bar. Default is `FALSE`.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

## Value

A 'by element' extended selection table (see
[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md)).

## Details

This function converts extended selection tables in 'by song' format
(several selection per wave object) to a 'by element' format (one wave
object per selection).

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`mp32wav`](https://marce10.github.io/warbleR/reference/mp32wav.md),
[`fix_wavs`](https://marce10.github.io/warbleR/reference/fix_wavs.md)

Other extended selection table manipulation:
[`rename_est_waves()`](https://marce10.github.io/warbleR/reference/rename_est_waves.md),
[`resample_est()`](https://marce10.github.io/warbleR/reference/resample_est.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>) \#last modification on
nov-9-2022 (MAS)

## Examples

``` r
if (FALSE) { # \dontrun{
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# create extended selection table
by_song_est <- selection_table(lbh_selec_table,
  path = tempdir(),
  extended = TRUE, by.song = "song"
)

# convert into by element
by_element_est <- by_element_est(by_song_est, mar = 0.05)
} # }
```
