# Measure the duration of sound files

`duration_sound_files` measures the duration of sound files

## Usage

``` r
duration_sound_files(
  files = NULL,
  path = NULL,
  skip.error = FALSE,
  file.format = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$"
)
```

## Arguments

- files:

  Character vector with the names of the sound files to be measured. The
  sound files should be in the working directory or in the directory
  provided in 'path'.

- path:

  Character string containing the directory path where the sound files
  are located.

- skip.error:

  Logical to control if errors are omitted. If so, files that could not
  be read will return `NA` in the 'duration' column. Default is `FALSE`,
  which will return an error if some files are problematic. If `NULL`
  (default) then the current working directory is used.

- file.format:

  Character string with the format of sound files. By default all sound
  file formats supported by warbleR are included
  ("\\wav\$\|\\wac\$\|\\mp3\$\|\\flac\$"). Note that several formats can
  be included using regular expression syntax as in
  [`grep`](https://rdrr.io/r/base/grep.html). For instance
  `"\.wav$|\.mp3$"` will only include .wav and .mp3 files.

## Value

A data frame with the duration (in seconds) of the sound files.

## Details

This function returns the duration (in seconds) of sound files.

## References

Araya-Salas, M., & Smith-Vidaurre, G. (2017). warbleR: An R package to
streamline analysis of animal acoustic signals. Methods in Ecology and
Evolution, 8(2), 184-191.

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  data(list = c("Phae.long1", "Phae.long2", "Phae.long3"))
  writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
  writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
  writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))

  duration_sound_files(path = tempdir())
}
#>         sound.files  duration
#> 1  Phae.long1-1.wav 0.2730667
#> 2  Phae.long1-2.wav 0.2630667
#> 3  Phae.long1-3.wav 0.2749333
#> 4    Phae.long1.wav 2.5000444
#> 5  Phae.long2-1.wav 0.2325778
#> 6  Phae.long2-2.wav 0.2261333
#> 7    Phae.long2.wav 1.7000444
#> 8  Phae.long3-1.wav 0.2312444
#> 9  Phae.long3-2.wav 0.2301778
#> 10 Phae.long3-3.wav 0.2312000
#> 11   Phae.long3.wav 2.2000000
#> 12 Phae.long4-1.wav 0.2454222
#> 13 Phae.long4-2.wav 0.2441778
#> 14 Phae.long4-3.wav 0.2451111
#> 15   Phae.long4.wav 3.2000000
#> 16      no_anns.wav 2.5000444
```
