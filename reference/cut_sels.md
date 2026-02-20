# Cut selections into individual sound files

`cut_sels` cuts selections from a selection table into individual sound
files.

## Usage

``` r
cut_sels(
  X,
  mar = 0.05,
  parallel = 1,
  path = NULL,
  dest.path = NULL,
  pb = TRUE,
  labels = c("sound.files", "selec"),
  overwrite = FALSE,
  norm = FALSE,
  keep.stereo = FALSE,
  ...
)
```

## Arguments

- X:

  object of class 'selection_table', 'extended_selection_table' or data
  frame containing columns for sound file name (sound.files), selection
  number (selec), and start and end time of signals (start and end).

- mar:

  Numeric vector of length 1. Specifies the margins adjacent to the
  start and end points of selections, delineating spectrogram limits.
  Default is 0.05.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used.

- dest.path:

  Character string containing the directory path where the cut sound
  files will be saved. If `NULL` (default) then the directory containing
  the sound files will be used instead.

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- labels:

  String vector. Provides the column names that will be used as labels
  to create sound file names. Note that they should provide unique names
  (otherwise sound files will be overwritten). Default is
  `c("sound.files", "selec")`.

- overwrite:

  Logical. If `TRUE` sound files with the same name will be overwritten.
  Default is `FALSE`.

- norm:

  Logical indicating whether wave objects must be normalized first using
  the function
  [`normalize`](https://rdrr.io/pkg/tuneR/man/normalize.html).
  Additional arguments can be passed to
  [`normalize`](https://rdrr.io/pkg/tuneR/man/normalize.html) using
  \`...\`.\` Default is `FALSE`. See
  [`normalize`](https://rdrr.io/pkg/tuneR/man/normalize.html) for
  available options.

- keep.stereo:

  Logical. If `TRUE` both channels are kept in the clips, oterwise it
  will keep the channel referenced in the channel column (if supplied)
  or the first channel if a 'channel' column is not found in 'X'. Only
  applies to stereo (2-channel) files.

- ...:

  Additional arguments to be passed to the internal
  [`normalize`](https://rdrr.io/pkg/tuneR/man/normalize.html) function
  for customizing sound file output. Ignored if `norm = FALSE`.

## Value

Sound files of the signals listed in the input data frame.

## Details

This function allow users to produce individual sound files from the
selections listed in a selection table as in
[`lbh_selec_table`](https://marce10.github.io/warbleR/reference/lbh_selec_table.md).
Note that wave objects with a bit depth of 32 might not be readable by
some programs after exporting. In this case they should be "normalized"
(argument 'norm") with a lower bit depth. The function keeps the
original number of channels in the output clips only for 1- and
2-channel files.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`tailor_sels`](https://marce10.github.io/warbleR/reference/tailor_sels.md)
for tailoring selections

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>) and Grace Smith Vidaurre

## Examples

``` r
{
# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# cut selections
cut_sels(lbh_selec_table, path = tempdir())

#check this folder!!
tempdir()
}
#> [1] "/tmp/RtmphhikVM"
```
