# Find clipped selections

`find_clipping` gets the proportion of samples that are clipped.

## Usage

``` r
find_clipping(X, path = NULL, parallel = 1, pb = TRUE)
```

## Arguments

- X:

  'selection_table', 'extended_selection_table' or data frame with the
  following columns: 1) "sound.files": name of the sound files, 2)
  "selec": number of the selections, 3) "start": start time of
  selections, 4) "end": end time of selections.

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar and messages. Default is
  `TRUE`.

## Value

A data frame with the 'sound.files' and 'selec' columns in X plus an
additional column ('prop.clipped') indicating the proportion of clipped
samples for each row. If sound files are stereo the average proportion
of the two channels is returned.

## Details

Clipping (i.e. saturation) occurs when an audio signal is amplified
above the maximum limit of the recorder. This leads to distortion and a
lowering of audio quality. If stereo the mean proportion of both
channels is returned. The function assumes specific range values for
different bit depths as detailed in
[`normalize`](https://rdrr.io/pkg/tuneR/man/normalize.html).

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`sig2noise`](https://marce10.github.io/warbleR/reference/sig2noise.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  # load data
  data(list = c("Phae.long1", "Phae.long2", "lbh_selec_table"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav")) # save sound files
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))

  find_clipping(X = lbh_selec_table[1:5, ], path = tempdir())
}
#>      sound.files selec prop.clipped
#> 1 Phae.long1.wav     1 0.0000000000
#> 2 Phae.long1.wav     2 0.0005451077
#> 3 Phae.long1.wav     3 0.0000000000
#> 4 Phae.long2.wav     1 0.0000000000
#> 5 Phae.long2.wav     2 0.0000000000
```
