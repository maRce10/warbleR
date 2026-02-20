# Fix .wav files to allow importing them into R

`fix_wavs` fixes sound files in .wav format so they can be imported into
R.

## Usage

``` r
fix_wavs(
  checksels = NULL,
  files = NULL,
  samp.rate = NULL,
  bit.depth = NULL,
  path = NULL,
  mono = FALSE
)
```

## Arguments

- checksels:

  Data frame with results from
  [`check_sels`](https://marce10.github.io/warbleR/reference/check_sels.md).
  Default is `NULL`. If both 'checksels' and 'files' are `NULL` then all
  files in 'path' are converted. Note that it only fixes/convert sound
  files in .wav format.

- files:

  Character vector with the names of the .wav files to fix. Default is
  `NULL`. If both 'checksels' and 'files' are `NULL` then all files in
  'path' are converted.

- samp.rate:

  Numeric vector of length 1 with the sampling rate (in kHz) for output
  files. Default is `NULL`. (remain unchanged).

- bit.depth:

  Numeric vector of length 1 with the dynamic interval (i.e. bit depth)
  for output files. Default is `NULL` (remain unchanged).

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used.

- mono:

  Logical indicating if stereo (2 channel) files should be converted to
  mono (1 channel). Default is `FALSE` (remain unchanged).

## Value

A folder inside the working directory (or path provided) called
'converted_sound_files', containing sound files in a format that can be
imported in R.

## Details

This function aims to simplify the process of converting sound files
that cannot be imported into R and/or homogenizing sound files.
Problematic files can be determined using
[`check_sound_files`](https://marce10.github.io/warbleR/reference/check_sound_files.md)
or
[`check_sels`](https://marce10.github.io/warbleR/reference/check_sels.md).
The
[`check_sels`](https://marce10.github.io/warbleR/reference/check_sels.md)
output can be directly input using the argument 'checksels'.
Alternatively a vector of file names to be "fixed" can be provided
(argument 'files'). If neither of those 2 are provided the function will
convert all .wav sound files in the working directory to the specified
sample rate/bit depth. Files are saved in a new directory
('converted_sound_files'). Internally the function calls
[SOX](https://sourceforge.net/projects/sox/)
([SOX](https://sourceforge.net/projects/sox/) must be installed). If
both 'checksels' and 'files' are `NULL` then all files in 'path' are
converted. Note that it only fixes/convert sound files in .wav format.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load example files and save to temporary working directory
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

fix_wavs(files = lbh_selec_table$sound.files, path = tempdir())

# check this folder
tempdir()
} # }
```
