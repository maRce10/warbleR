# An extended version of read_wave that reads several sound file formats and files from selection tables

`read_sound_file` reads several sound file formats as well as files
referenced in selection tables

## Usage

``` r
read_sound_file(
  X,
  index = NULL,
  from = X$start[index],
  to = X$end[index],
  channel = X$channel[index],
  header = FALSE,
  path = NULL
)
```

## Arguments

- X:

  'data.frame', 'selection_table' or 'extended_selection_table'
  containing columns for sound file name (sound.files), selection number
  (selec), and start and end time of signals (start and end).
  Alternatively, the name of a sound file or URL address to sound file
  can be provided. The function can read sound files in 'wav', 'mp3',
  'flac' and 'wac' format. The file name can contain the directory path.
  'top.freq' and 'bottom.freq' columns are optional. Default is `NULL`.

- index:

  Index of the selection in 'X' that will be read. Ignored if 'X' is
  `NULL`.

- from:

  Where to start reading, in seconds. Default is `X$start[index]`.

- to:

  Where to stop reading, in seconds. Default is `X$end[index]`.

- channel:

  Channel to be read from sound file (1 = left, 2 = right, or higher
  number for multichannel waves). Default is `X$channel[index]`. If a
  'channel' column does not exist it will read the first channel.

- header:

  If `TRUE`, only the header information of the Wave object is returned,
  otherwise (the default) the whole Wave object.

- path:

  Character string containing the directory path where the sound files
  are located. If `NULL` (default) then the current working directory is
  used. If 'X' refers to a sound file including its directory 'path' is
  ignored.

## Value

An object of class "Wave".

## Details

The function is a wrapper for
[`readWave`](https://rdrr.io/pkg/tuneR/man/readWave.html) that read
sound files, including those referenced in selection tables. It is also
used internally by warbleR functions to read wave objects from extended
selection tables (see
[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.md)
for details). For reading 'flac' files on windows the path to the .exe
is required. This can be set globally using the 'flac.path' argument in
[`warbleR_options`](https://marce10.github.io/warbleR/reference/warbleR_options.md).
Note that reading 'flac' files requires creating a temporary copy in
'wav' format, which can be particularly slow for long files.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
# write wave files with lower case file extension
data(list = c("Phae.long1"))
writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))

# read wave file
read_sound_file(X = "Phae.long1.wav", index = 1, path = tempdir())

# read from selection table
read_sound_file(X = lbh_selec_table, index = 1, path = tempdir())

# from extended selection table
library(NatureSounds)
read_sound_file(X = lbh.est, index = 1)

# read from selection table
read_sound_file(X = lbh_selec_table, index = 1, path = tempdir())

# URL file
read_sound_file(X = "https://www.xeno-canto.org/513948/download")
} # }
```
