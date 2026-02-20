# Check selection data frames

`check_sels` checks whether selections can be read by subsequent
functions.

## Usage

``` r
check_sels(
  X = NULL,
  parallel = 1,
  path = NULL,
  check.header = FALSE,
  pb = TRUE,
  wav.size = FALSE,
  verbose = TRUE,
  fix.selec = FALSE
)
```

## Arguments

- X:

  'selection_table' object or data frame with the following columns: 1)
  "sound.files": name of the .wav files, 2) "selec": number of the
  selections, 3) "start": start time of selections, 4) "end": end time
  of selections.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used.

- check.header:

  Logical. Controls whether sound file headers correspond to the actual
  file properties (i.e. if is corrupted). This could significantly
  affect the performance of the function (much slower) particularly with
  long sound files.

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- wav.size:

  Logical argument to control if the size of the wave object when the
  selection is imported into R (as when using
  [`readWave`](https://rdrr.io/pkg/tuneR/man/readWave.html) is
  calculated and added as a column. Size is return in MB. Default is
  `FALSE`.

- verbose:

  Logical to control whether the summary messages are printed to the
  console. Defaut is `TRUE`.

- fix.selec:

  Logical to control if labels in 'selec' column should be fixed. This
  column should not be duplicated within a sound file. If that happens
  and `fix.selec = TRUE` duplicated labels will be changed. Default is
  `FALSE`.

## Value

A data frame including the columns in the input data frame (X) and the
following additional columns:

- `check.res`: diagnose for each selection

- `duration`: duration of selection in seconds

- `min.n.samples` number of samples in a selection. Note the number of
  samples available in a selection limits the minimum window length (wl
  argument in other functions) that can be used in batch analyses.

- `sample.rate`: sampling rate in kHz

- `channels`: number of channels

- `bits`: bit depth

- `sound.file.samples`: number of samples in the sound file

## Details

This function checks the information in a selection data frame or
selection table (i.e. data frame with annotations on sound files) to
avoid problems in any warbleR analysis downstream. It specifically
checks if:

- 'X' is an object of class 'data.frame' or 'selection_table' (see
  [`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md))
  and contains the required columns to be used on any warbleR function
  ('sound.files', 'selec', 'start', 'end', if not returns an error)

- 'sound.files' in 'X' correspond to sound files in the working
  directory or in the provided 'path' (if no file is found returns an
  error, if some files are not found returns error info in the ouput
  data frame)

- time ('start', 'end') and frequency ('bottom.freq', 'top.freq', if
  provided) limit parameters are numeric and don't contain NAs (if not
  returns an error)

- there are no duplicated selection labels ('selec') within a sound file
  (if not returns an error)

- sound files can be read (error info in the ouput data frame)

- the start and end time of the selections are found within the duration
  of the sound files (error info in the ouput data frame)

- sound files can be read (error info in the ouput data frame)

- sound files header is not corrupted (only if `header = TRUE`, error
  info in the ouput data frame)

- selection time position (start and end) doesn't exceeds sound file
  length (error info in the ouput data frame)

- 'top.freq' is lower than half the sample rate (nyquist frequency,
  error info in the ouput data frame)

- negative values aren't found in time or frequency limit parameters
  (error info in the ouput data frame)

- 'start' higher than 'end' or 'bottom.freq' higher than 'top.freq'
  (error info in the ouput data frame)

- 'channel' value is not higher than number of channels in sound files
  (error info in the ouput data frame)

The function returns a data frame that includes the information in 'X'
plus additional columns about the format of sound files (see 'Value') as
well as the result of the checks ('check.res' column, value is 'OK' if
everything is fine). Sound files should be in the working directory (or
the directory provided in 'path'). Corrupt files can be fixed using
[`fix_wavs`](https://marce10.github.io/warbleR/reference/fix_wavs.md).

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
# save wav file examples
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))

check_sels(X = lbh_selec_table, path = tempdir())
}
#> checking annotations (step 0 of 0):
#> all selections are OK 
#> 
```
