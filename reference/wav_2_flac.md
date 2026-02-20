# Convert .wav files to .flac

`wav_2_flac` converts several .wav files to .flac compressed lossless
format

## Usage

``` r
wav_2_flac(
  files = NULL,
  path = NULL,
  overwrite = FALSE,
  pb = TRUE,
  parallel = 1,
  reverse = FALSE,
  compression = 5,
  flac.path = ""
)
```

## Arguments

- files:

  character vector with the names of files to be converted. If `NULL`
  all files in the working directory (or 'path' if supplied) are
  converted.

- path:

  Character string containing the directory path where the .wav files
  are located. If `NULL` (default) then the current working directory is
  used.

- overwrite:

  Logical. Control whether a .flac sound file that is already in the
  working directory should be overwritten.

- pb:

  Logical argument to control if progress bar is shown. Default is
  `TRUE`. It can also be set globally using the 'pb' option (see
  [`warbleR_options`](https://marce10.github.io/warbleR/reference/warbleR_options.md)).

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing). It can also be set globally using the 'parallel' option
  (see
  [`warbleR_options`](https://marce10.github.io/warbleR/reference/warbleR_options.md)).

- reverse:

  Logical argument to control if .wav files are converted into .flac
  files (default, `reverse = FALSE`) or .flac files are converted into
  .wav files `reverse = TRUE`.

- compression:

  Numeric string on length 1 indicating the level of compression for
  .flac files. Must a number between 0 (lowest) to 8 (highest
  compression). Default is 5.

- flac.path:

  Path to the flac program, mostly needed for windows OS.

## Value

.flac files saved in the working directory with same name as original
wav files.

## Details

The function will convert all .wav files in working directory or 'path'
supplied to .flac format (or the opposite if `reverse = TRUE`). For
reading 'flac' files on windows the path to the .exe is required. This
can be set using the 'flac.path' argument (or globally using the same
argument in
[`warbleR_options`](https://marce10.github.io/warbleR/reference/warbleR_options.md)).
Note that reading 'flac' files requires creating a temporary copy in
'wav' format, which can be particularly slow for long files.

convert all .wav files in working directory to .flac compressed lossless
format. It's just a silly wrapper over
([`wav2flac`](https://rdrr.io/pkg/seewave/man/wav2flac.html)) to
simplify converting several files at once. The function works
recursively, converting files within all subfolders.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
# create some .wav files
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))

# Convert all files to .flac format
wav_2_flac(path = tempdir())

# check this folder!!
open_wd(tempdir())
} # }
```
