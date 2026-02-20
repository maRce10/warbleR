# Calculates acoustic parameters at the song level

`song_analysis` calculates descriptive statistics of songs or other
higher levels of organization in the signals.

## Usage

``` r
song_analysis(
  X = NULL,
  song_colm = "song",
  mean_colm = NULL,
  min_colm = NULL,
  max_colm = NULL,
  elm_colm = NULL,
  elm_fun = NULL,
  sd = FALSE,
  parallel = 1,
  pb = TRUE,
  na.rm = FALSE,
  weight = NULL
)
```

## Arguments

- X:

  'selection_table', 'extended_selection_table' (created 'by.song') or
  data frame with the following columns: 1) "sound.files": name of the
  sound files, 2) "selec": number of the selections, 3) "start": start
  time of selections, 4) "end": end time of selections.

- song_colm:

  Character string with the column name containing song labels. It can
  be used to label any hierarchical level at which parameters need to be
  calculated (e.g. syllables, phrases). Note that the function assumes
  that song labels are not repeated within a sound file.

- mean_colm:

  Numeric vector with the index of the columns that will be averaged. If
  `NULL` the mean of all numeric columns in 'X' is returned.

- min_colm:

  Character vector with the name(s) of the columns for which the minimum
  value is needed. Default is `NULL`.

- max_colm:

  Character vector with the name(s) of the columns for which the maximum
  value is needed. Default is `NULL`.

- elm_colm:

  Character vector with the name(s) of the columns identifying the
  element labels (i.e. element types). If supplied 'unq.elms' and
  'mean.elm.count' are returned. Default is `NULL`.

- elm_fun:

  Function to be applied to the sequence of elements composing a song.
  Default is `NULL`. Ignored if 'elm_colm' is not supplied. The name of
  the column containing the function's output is "elm_fun'.

- sd:

  Logical value indicating whether standard deviation is also returned
  for variables in which averages are reported. Default is `FALSE`.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar and messages. Default is
  `TRUE`.

- na.rm:

  Logical value indicating whether 'NA' values should be ignored for
  calculations.

- weight:

  Character vector defining 1 or more numeric vectors to weight average
  measurements (i.e. song parameters). Names of numeric columns in 'X'
  can also be used. See
  [`weighted.mean`](https://rdrr.io/r/stats/weighted.mean.html). for
  more details. Default is `NULL` (unweighted average).

## Value

A data frame similar to the input 'X' data frame, but in this case each
row corresponds to a single song. The data frame contains the mean or
extreme values for numeric columns for each song. Columns that will be
averaged can be defined with 'mean_colm' (otherwise all numeric columns
are used). Columns can be weighted by other columns in the data set
(e.g. duration, frequency range). In addition, the function returns the
following song level parameters:

- `elm.duration`: mean length of elements (in s)

- `song.duration`: length of song (in s)

- `num.elms`: number of elements (or song units)

- `start`: start time of song (in s)

- `end`: end time of song (in s)

- `bottom.freq`: lowest 'bottom.freq' from all song elements (in kHz)

- `top.freq`: highest 'top.freq' from all song elements (in kHz)

- `freq.range`: difference between song's 'top.freq' and 'bottom.freq'
  (in kHz)

- `song.rate`: number of elements per second (NA if only 1 element).
  Calculated as the number of elements in the 'song' divided by the
  duration of the song. In this case song duration is calculated as the
  time between the start of the first element and the start of the last
  element, which provides a rate that is less affected by the duration
  of individual elements. Note that this calculation is different than
  that from 'song.duration' above.

- `gap.duration`: average length of gaps (i.e. silences) in between
  elements (in s, NA if only 1 element)

- `elm.types`: number of element types (i.e. number of unique types,
  only if 'elm_colm' is supplied)

- `mean.elm.count`: mean number of times element types are found (only
  if 'elm_colm' is supplied)

This function assumes that song labels are not repeated within a sound
file.

## Details

The function calculates average or extreme values of acoustic parameters
of elements in a song or other level of organization in the signals.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`spectro_analysis`](https://marce10.github.io/warbleR/reference/spectro_analysis.md)

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

# add a 'song' column
lbh_selec_table$song <- c("song1", "song1", "song1", "song2",
  "song2", "song3", "song3", "song3", "song4", "song4", "song4")

# measure acoustic parameters
sp <- spectro_analysis(lbh_selec_table[1:8, ], bp = c(1, 11), 300, fast = TRUE, path = tempdir())

# add song data
sp <- merge(sp, lbh_selec_table[1:8, ], by = c("sound.files", "selec"))

# caculate song-level parameters for all numeric parameters
song_analysis(X = sp, song_colm = "song", parallel = 1, pb = TRUE)

# caculate song-level parameters selecting parameters with mean_colm
song_analysis(X = sp, song_colm = "song",mean_colm = c("dfrange", "duration"),
 parallel = 1, pb = TRUE)

# caculate song-level parameters for selecting parameters with mean_colm, max_colm
# and min_colm and weighted by duration
song_analysis(X = sp, weight = "duration", song_colm = "song",
mean_colm =  c("dfrange", "duration"), min_colm =  "mindom", max_colm = "maxdom",
  parallel = 1, pb = TRUE)

# with two weights
song_analysis(X = sp, weight = c("duration", "dfrange"), song_colm = "song",
mean_colm = c("kurt", "sp.ent"), parallel = 1, pb = TRUE)

# with two weights no progress bar
song_analysis(X = sp, weight = c("duration", "dfrange"), song_colm = "song",
mean_colm = c("kurt", "sp.ent"), parallel = 1, pb = FALSE)
}
#>      sound.files selec     start      end top.freq bottom.freq  song      kurt
#> 1 Phae.long1.wav     1 0.3433366 2.321457 8.807053    2.169437 song1 11.894202
#> 2 Phae.long2.wav     1 0.1595983 1.583209 8.888027    2.284006 song2 12.223883
#> 3 Phae.long3.wav     1 0.1233643 2.104392 9.315153    2.316862 song3  9.024535
#>      sp.ent num.elms elm.duration freq.range song.duration song.rate
#> 1 0.9312400        3    0.1703334   6.637617      1.978120  1.652827
#> 2 0.9257034        2    0.1293606   6.604022      1.423610  1.541473
#> 3 0.9083270        3    0.1308718   6.998291      1.981028  1.620878
#>   gap.duration
#> 1    0.7335599
#> 2    1.1648893
#> 3    0.7942062
```
