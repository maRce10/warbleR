# Create 'selection_table' and 'extended_selection_table' objects

`selection_table` converts data frames into an object of classes
'selection_table' or 'extended_selection_table'.

## Usage

``` r
selection_table(
  X,
  max.dur = 10,
  path = NULL,
  whole.recs = FALSE,
  extended = FALSE,
  mar = 0.1,
  by.song = NULL,
  pb = TRUE,
  parallel = 1,
  verbose = TRUE,
  skip.error = FALSE,
  file.format = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$",
  files = NULL,
  ...
)
```

## Arguments

- X:

  data frame with the following columns: 1) "sound.files": name of the
  .wav files, 2) "selec": unique selection identifier (within a sound
  file), 3) "start": start time and 4) "end": end time of selections.
  Columns for 'top.freq', 'bottom.freq' and 'channel' are optional. Note
  that, when 'channel' is not provided the first channel (i.e. left
  channel) would be used by default. Frequency parameters (including top
  and bottom frequency) should be provided in kHz. Alternatively, a
  'selection_table' class object can be input.

- max.dur:

  the maximum duration of expected for a selection (ie. end - start). If
  surpassed then an error message will be generated. Useful for
  detecting errors in selection tables.

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used.

- whole.recs:

  Logical. If `TRUE` the function will create a selection table for all
  sound files in the working directory (or "path") with \`start = 0\`
  and \`end = duration_sound_files()\`. Default is if `FALSE`. Note that
  this will not create a extended selection table. If provided 'X' is
  ignored.

- extended:

  Logical. If `TRUE`, the function will create an object of class
  'extended_selection_table' which included the wave objects of the
  selections as an additional attribute ('wave.objects') to the data
  set. This is and self-contained format that does not require the
  original sound files for running most acoustic analysis in
  [warbleR](https://cran.r-project.org/package=warbleR). This can
  largely facilitate the storing and sharing of (bio)acoustic data.
  Default is if `FALSE`. An extended selection table won't be created if
  there is any issue with the selection. See 'details'.

- mar:

  Numeric vector of length 1 specifying the margins (in seconds)
  adjacent to the start and end points of the selections when creating
  extended selection tables. Default is 0.1. Ignored if 'extended' is
  `FALSE`.

- by.song:

  Character string with the column name containing song labels. If
  provided a wave object containing for all selection belonging to a
  single song would be saved in the extended selection table (hence only
  applicable for extended selection tables). Note that the function
  assumes that song labels are not repeated within a sound file. If
  `NULL` (default), wave objects are created for each selection (e.g. by
  selection). Ignored if `extended = FALSE`.

- pb:

  Logical argument to control progress bar and messages. Default is
  `TRUE`.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- verbose:

  Logical argument to control if summary messages are printed to the
  console. Default is `TRUE`.

- skip.error:

  Logical to control if errors are omitted. If so, files that could not
  be read will be excluded and their name printed in the console.
  Default is `FALSE`, which will return an error if some files are
  problematic.

- file.format:

  Character string with the format of sound files. By default all sound
  file formats supported by warbleR are included
  ("\\wav\$\|\\wac\$\|\\mp3\$\|\\flac\$"). Note that several formats can
  be included using regular expression syntax as in
  [`grep`](https://rdrr.io/r/base/grep.html). For instance
  `"\.wav$|\.mp3$"` will only include .wav and .mp3 files. Ignored if
  `whole.recs = FALSE`.

- files:

  character vector indicating the set of files that will be
  consolidated. File names should not include the full file path.
  Optional.

- ...:

  Additional arguments to be passed to
  [`check_sels`](https://marce10.github.io/warbleR/reference/check_sels.md)
  for customizing checking routine.

## Value

An object of class selection_table which includes the original data
frame plus the following additional attributes:

- 1\) A data frame with the output of
  [`check_sels`](https://marce10.github.io/warbleR/reference/check_sels.md)
  run on the input data frame. If a extended selection table is created
  it will also include the original values in the input data frame for
  each selections. This are used by downstream warbleR functions to
  improve efficiency and avoid errors due to missing or mislabeled data,
  or selection out of the ranges of the original sound files.

- 2\) A list indicating if the selection table has been created by song
  (see 'by.song argument).

- 3\) If a extended selection table is created a list containing the
  wave objects for each selection (or song if 'by.song').

## Details

This function creates and object of class 'selection_table' or
'extended_selection_table' (if `extended = TRUE`, see below). First, the
function checks:

- 1\) if the selections listed in the data frame correspond to .wav
  files in the working directory

- 2\) if the sound files can be read and if so,

- 3\) if the start and end time of the selections are found within the
  duration of the sound files

If no errors are found the a selection table or extended selection table
will be generated. Note that the sound files should be in the working
directory (or the directory provided in 'path'). This is useful for
avoiding errors in downstream functions (e.g.
[`spectro_analysis`](https://marce10.github.io/warbleR/reference/spectro_analysis.md),
[`cross_correlation`](https://marce10.github.io/warbleR/reference/cross_correlation.md),
[`catalog`](https://marce10.github.io/warbleR/reference/catalog.md),
[`freq_DTW`](https://marce10.github.io/warbleR/reference/freq_DTW.md)).
Note also that corrupt files can be fixed using
[`fix_wavs`](https://marce10.github.io/warbleR/reference/fix_wavs.md)
('sox' must be installed to be able to run this function). The
'selection_table' class can be input in subsequent functions.

When `extended = TRUE` the function will generate an object of class
'extended_selection_table' which will also contain the wave objects for
each of the selections in the data frame. This transforms selection
tables into self-contained objects as they no longer need the original
sound files to run acoustic analysis. This can largely facilitate the
storing and sharing of (bio)acoustic data. Extended selection table size
will be a function of the number of selections `nrow(X)`, sampling rate,
selection duration and margin duration. As a guide, a selection table
with 1000 selections similar to the ones in 'lbh_selec_table' (mean
duration ~0.15 seconds) at 22.5 kHz sampling rate and the default margin
(`mar = 0.1`) will generate a extended selection table of ~31 MB (~310
MB for a 10000 rows selection table). You can check the size of the
output extended selection table with the
[`object.size`](https://rdrr.io/r/utils/object.size.html) function. Note
that extended selection table created 'by.song' could be considerable
larger.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## See also

[`check_sound_files`](https://marce10.github.io/warbleR/reference/check_sound_files.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  data(list = c(
    "Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4",
    "lbh_selec_table"
  ))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
  writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

  # make selection table
  st <- selection_table(X = lbh_selec_table, path = tempdir())

  is_selection_table(st)

  #' # make extended selection table
  st <- selection_table(
    X = lbh_selec_table, extended = TRUE,
    path = tempdir()
  )

  is_extended_selection_table(st)

  ### make extended selection by song
  # create a song variable
  lbh_selec_table$song <- as.numeric(as.factor(lbh_selec_table$sound.files))

  st <- selection_table(
    X = lbh_selec_table, extended = TRUE,
    by.song = "song", path = tempdir()
  )
}
#> all selections are OK 
#> 
#> all selections are OK 
#> 
#> all selections are OK 
#> 
```
