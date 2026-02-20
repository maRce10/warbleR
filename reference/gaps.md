# Gap duration

`gaps` measures gap duration

## Usage

``` r
gaps(X = NULL, by = "sound.files", parallel = 1, pb = TRUE)
```

## Arguments

- X:

  'selection_table', 'extended_selection_table' (created 'by.song') or
  data frame with the following columns: 1) "sound.files": name of the
  sound files, 2) "selec": number of the selections, 3) "start": start
  time of selections, 4) "end": end time of selections.

- by:

  Character vector with column names. Controls the levels at which gaps
  will be measured. "sound.files" must always be included.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar and messages. Default is
  `TRUE`.

## Value

A data frame identical to that supplied in 'X', with and additional
column ('gaps') with the duration of the time interval between
selections.

## Details

The function measures the time intervals (i.e. gaps) between selections.
The gap for a given selection is calculated as the time interval to the
selection immediately after. Hence, there is no gap for the last
selection in a sound file (or level determined by the 'by' argument).
Gap is set to 0 when selections overlap in time. Note that the sound
files are not required.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`inflections`](https://marce10.github.io/warbleR/reference/inflections.md),
[`song_analysis`](https://marce10.github.io/warbleR/reference/song_analysis.md),

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
# get warbleR sound file examples
data(list = "lbh_selec_table")

# get gaps
gaps(X = lbh_selec_table)
}
#>       sound.files channel selec     start       end bottom.freq  top.freq
#> 1  Phae.long1.wav       1     1 1.1693549 1.3423884    2.220105  8.604378
#> 2  Phae.long1.wav       1     2 2.1584085 2.3214565    2.169437  8.807053
#> 3  Phae.long1.wav       1     3 0.3433366 0.5182553    2.218294  8.756604
#> 4  Phae.long2.wav       1     1 0.1595983 0.2921692    2.316862  8.822316
#> 5  Phae.long2.wav       1     2 1.4570585 1.5832087    2.284006  8.888027
#> 6  Phae.long3.wav       1     1 0.6265520 0.7577715    3.006834  8.822316
#> 7  Phae.long3.wav       1     2 1.9742132 2.1043921    2.776843  8.888027
#> 8  Phae.long3.wav       1     3 0.1233643 0.2545812    2.316862  9.315153
#> 9  Phae.long4.wav       1     1 1.5168116 1.6622365    2.513997  9.216586
#> 10 Phae.long4.wav       1     2 2.9326920 3.0768784    2.579708 10.235116
#> 11 Phae.long4.wav       1     3 0.1453977 0.2904966    2.579708  9.742279
#>         gaps
#> 1  0.8160201
#> 2         NA
#> 3  0.6510996
#> 4  1.1648893
#> 5         NA
#> 6  1.2164417
#> 7         NA
#> 8  0.3719707
#> 9  1.2704555
#> 10        NA
#> 11 1.2263150
```
