# Splits sound files

`split_sound_files` splits sound files in shorter segments

## Usage

``` r
split_sound_files(
  path = NULL,
  sgmt.dur = 10,
  sgmts = NULL,
  files = NULL,
  parallel = 1,
  pb = TRUE,
  only.sels = FALSE,
  X = NULL
)
```

## Arguments

- path:

  Directory path where sound files are found. If `NULL` (default) then
  the current working directory is used.

- sgmt.dur:

  Numeric. Duration (in s) of segments in which sound files would be
  split. Sound files shorter than 'sgmt.dur' won't be split. Ignored if
  'sgmts' is supplied.

- sgmts:

  Numeric. Number of segments in which to split each sound file. If
  supplied 'sgmt.dur' is ignored.

- files:

  Character vector indicating the subset of files that will be split.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar. Default is `TRUE`. Only used
  when

- only.sels:

  Logical argument to control if only the data frame is returned (no
  wave files are saved). Default is `FALSE`.

- X:

  'selection_table' object or a data frame with columns for sound file
  name (sound.files), selection number (selec), and start and end time
  of signal (start and end). If supplied the data frame/selection table
  is modified to reflect the position of the selections in the new sound
  files. Note that some selections could split between 2 segments. To
  deal with this, a 'split.sels' column is added to the data frame in
  which those selection are labeled as 'split'. Default is `NULL`.

## Value

Wave files for each segment in the working directory (if
`only.sels = FALSE`, named as 'sound.file.name-#.wav') and a data frame
in the R environment containing the name of the original sound files
(org.sound.files), the name of the clips (sound.files) and the start and
end of clips in the original files. Clips are saved in .wav format. If
'X' is supplied then a data frame with the position of the selections in
the newly created clips is returned instead.

## Details

This function aims to reduce the size of sound files in order to
simplify some processes that are limited by sound file size. The
function keeps the original number of channels in the output clips only
for 1- and 2-channel files.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`cut_sels`](https://marce10.github.io/warbleR/reference/cut_sels.md)

Other data manipulation:
[`move_images()`](https://marce10.github.io/warbleR/reference/move_images.md),
[`open_wd()`](https://marce10.github.io/warbleR/reference/open_wd.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  # load data and save to temporary working directory
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))

  # split files in 1 s files
  split_sound_files(sgmt.dur = 1, path = tempdir())

  # Check this folder
  tempdir()
}
#> [1] "/tmp/RtmphhikVM"
```
